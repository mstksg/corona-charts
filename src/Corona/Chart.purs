
module Corona.Chart where

import Prelude

import Apexcharts (createChart, render, Apexchart, Apexoptions)
import Apexcharts.Chart as C
import Control.Apply
import Apexcharts.Chart.Zoom as Z
import Data.Dated (Dated(..))
import Data.Dated as D
import Apexcharts.Common as CC
import Apexcharts.Series as SE
import Apexcharts.Xaxis as X
import Apexcharts.Yaxis as Y
import Control.Monad.ST as ST
import Corona.JHU as JHU
import D3.Scatter (ScatterPlot(..), Point(..), Scale(..), AxisConf(..))
import Data.Array as A
import Data.Array.ST as MA
import Data.Foldable
import Data.Int
import Data.JSDate (JSDate)
import Data.JSDate as JSDate
import Data.List as L
import Data.List.NonEmpty as NE
import Data.Map as M
import Data.Maybe
import Data.ModifiedJulianDay (Day(..))
import Data.ModifiedJulianDay as MJD
import Data.NonEmpty as NE
import Data.Options
import Data.Sequence as Seq
import Data.Sequence.NonEmpty as NESeq
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple
import Data.Void
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class
import Foreign.Object as O
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Query.EventSource as ES
import Type.Equality
import Type.Equiv

newtype Duration = Duration Number
newtype Percent  = Percent Number

data BaseProjection a =
        Time (Equiv a Day)
      | Confirmed (Equiv a Number)
      | Deaths (Equiv a Number)
      | Recovered (Equiv a Number)

type Projection b = forall r.
        (forall a. { base      :: BaseProjection a
                   , operation :: Operation a b
                   } -> r)
        -> r

-- data Condition a = AtLeast a | AtMost a

-- type SomeCondition = forall r. (forall a. { condition :: Condition a, projection :: Projection a } -> r ) -> r

-- type Refuted a = a -> Void

data Operation a b =
        Ident     (Equiv  a b)        -- ^ x
      | Delta     (Equiv a Number) (Equiv b Number)  -- ^ dx/dt
      | PGrowth   (Equiv a Number) (Equiv b Percent)   -- ^ (dx/dt)/x        -- how to handle percentage
      | Window    (Equiv a Number) (Equiv b Number) Int -- ^ moving average of x over t, window (2n+1)
      -- | DaysSince SomeCondition (Equiv a JSDate) (Equiv b Duration)
            -- ^ says since x has met a condition
      | Chain     (forall r. (forall c. Operation a c -> Operation c b -> r) -> r)

reflTo :: forall a b. Equiv a b -> a -> b
reflTo r = r to

reflFrom :: forall a b. Equiv a b -> b -> a
reflFrom r = r from

percentGrowth
    :: Number
    -> Number
    -> Percent
percentGrowth x y = Percent ((y - x) / x)

withConsec
    :: forall a b.
       (a -> a -> b)    -- ^ item n, item (n+1)
    -> Array a
    -> Array b
withConsec f xs = L.toUnfoldable $
                    L.zipWith f xs' (L.drop 1 xs')
  where
    xs' = L.fromFoldable xs

withWindow
    :: forall a b.
       Int  -- ^ 2*n+1
    -> (NESeq.Seq a -> b)
    -> Array a
    -> Array b
withWindow n f = L.toUnfoldable
             <<< map f
             <<< windows n
             <<< L.fromFoldable
      
windows
    :: forall a.
       Int  -- ^ 2*n+1
    -> L.List a
    -> L.List (NESeq.Seq a)
windows n = case _ of
    L.Cons y xs ->
      let ys = NESeq.fromFoldable1 (NE.NonEmpty y (L.take (2*n) xs))
          zs = L.drop (2*n) xs
      in  L.Cons ys (L.scanl go ys zs)
    L.Nil -> L.Nil
  where
    go :: NESeq.Seq a -> a -> NESeq.Seq a
    go xs x = case Seq.uncons (NESeq.tail xs) of
      Nothing           -> NESeq.singleton x
      Just (Tuple y ys) -> NESeq.Seq y (Seq.snoc ys x)
    
applyOperation
    :: forall a b. Operation a b
    -> Dated a
    -> Dated b
applyOperation = case _ of
    Ident refl  -> map (reflTo refl)
    -- \x -> Dated { start: x.start, values: map (reflTo refl) x.values }
    Delta ran rbn -> \(Dated x) -> Dated
        { start: MJD.addDays 1 x.start
        , values: withConsec
                    (\y0 y1 -> reflFrom rbn (reflTo ran y1 - reflTo ran y0))
                    x.values
        }
    PGrowth ran rbp -> \(Dated x) -> Dated
        { start: MJD.addDays 1 x.start
        , values: withConsec
                    (\y0 y1 -> reflFrom rbp $
                        percentGrowth (reflTo ran y0) (reflTo ran y1)
                    )
                    x.values
        }
    Window ran rbp n -> \(Dated x) -> Dated
        { start: MJD.addDays n x.start
        , values: withWindow n
                    (\ys -> reflFrom rbp $
                        sum (map (reflTo ran) ys) / toNumber (length ys)
                    )
                    x.values
        }
    Chain f -> f (\o1 o2 ->
            applyOperation o2 <<< applyOperation o1
        )
    -- DaysSince c refl refl' -> ?e

applyBaseProjection
    :: forall a. Partial
    => BaseProjection a
    -> Dated Number
    -> Dated a
applyBaseProjection = case _ of
    Time refl      -> D.mapWithIndex (\i _ -> reflFrom refl i)
    Confirmed refl -> map (reflFrom refl)
    
    
applyProjection
    :: forall a. Partial
    => Projection a
    -> Dated Number
    -> Dated a
applyProjection spr = spr (\pr ->
          applyOperation pr.operation <<< applyBaseProjection pr.base
        )

baseProjectionLabel :: forall a. BaseProjection a -> String
baseProjectionLabel = case _ of
    Time      _ -> "Date"
    Confirmed _ -> "Confirmed Cases"
    Deaths    _ -> "Deaths"
    Recovered _ -> "Recovered Cases"

operationLabel :: forall a b. Operation a b -> String
operationLabel = case _ of
    Ident   _     -> "Amount of"
    Delta   _ _   -> "Daily Increase in"
    PGrowth _ _   -> "Daily Percent Growth in"
    Window  _ _ n -> "Moving Average (size " <> show (2*n+1) <> ") of"
    Chain   f     -> f (\o1 o2 -> operationLabel o2 <> " " <> operationLabel o1)

projectionLabel :: forall a. Projection a -> String
projectionLabel spr = spr (\pr ->
        operationLabel pr.operation <> " " <> baseProjectionLabel pr.base
    )

toAxisConf :: forall a. Projection a -> Scale a -> AxisConf a
toAxisConf spr scl = 
      { scale: scl
      , label: projectionLabel spr
      }

toSeries
    :: forall a b. Partial
    => Projection a
    -> Projection b
    -> Dated Number
    -> Array (Point a b)
toSeries pX pY ps = D.datedValues $
    lift2 (\x y -> {x, y}) (applyProjection pX ps) (applyProjection pY ps)

-- type SeriesData a b =
--     { name   :: String
--     , values :: Array (Point a b)
--     }


-- type AxisConf a = { scale :: Scale a, label :: String }

-- type ScatterPlot a b =
--         { xAxis  :: AxisConf a
--         , yAxis  :: AxisConf b
--         , series :: Array (SeriesData a b)
--         }

-- data Aspect = Time
--             | Confirmed

-- confirmed, deaths, recovered
-- aspect:
--   * count
--   * moving
--   * change
--   * percent change

-- data DataView = DV
--     { countries :: Set Country
--     , spec      :: ViewSpec
--     }

-- type ViewSpec =
--     { xAxis :: Aspect
--     , yAxis :: Aspect
--     }

-- extractAspect
--     :: Array JSDate
--     -> JHU.CoronaCounts
--     -> Aspect
--     -> Array Number
-- extractAspect dat cc = case _ of
--     Time      -> map JSDate.getTime dat
--     Confirmed -> map toNumber cc.confirmed

-- aspectType :: Aspect -> AxisType
-- aspectType = case _ of
--     Time      -> DateTime
    

-- type SeriesData = 
--     { series :: Options SE.Series
--     , xAxis  :: Options X.Xaxis
--     , yAxis  :: Options Y.Yaxis
--     }

-- mkSeries
--     :: JHU.CoronaData
--     -> Country
--     -> ViewSpec
--     -> Maybe SeriesData
-- mkSeries dat country vs = do
--     countryCounts <- O.lookup country dat.counts
--     let xData    = extractAspect dat.dates countryCounts vs.xAxis
--         yData    = extractAspect dat.dates countryCounts vs.yAxis
--         fullData = A.zipWith (\x y -> [x, y]) xData yData
--         series   = SE.name  := country
--                 <> SE.data' := fullData
--     pure { series, xAxis: mempty, yAxis: mempty }
    
--       egyptData = case O.lookup "Egypt" dat of
    -- SE.name := country
  -- where
    -- xData

--     render $ createChart "#scatteredchart" (
--          SE.series := [
--             (SE.name := "Egypt" <> SE.data' := egyptData)
--          ]
--          <> C.chart := (C.type' := CC.Scatter <> C.height := 350 <> Z.zoom := (Z.enabled := true <> Z.type' := Z.XY))       
--          <> X.xaxis := (X.tickAmount := 10.0 <> X.type' := X.Datetime)
--          <> Y.yaxis := (Y.tickAmount := 10.0)
--       )

foreign import incrDate    :: Int -> JSDate -> JSDate
