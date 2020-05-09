
module Corona.Chart where

import Prelude

import Apexcharts (createChart, render, Apexchart, Apexoptions)
import Apexcharts.Chart as C
import Apexcharts.Chart.Zoom as Z
import Apexcharts.Common as CC
import Apexcharts.Series as SE
import Apexcharts.Xaxis as X
import Apexcharts.Yaxis as Y
import Control.Apply
import Control.Monad.ST as ST
import Corona.JHU (Counts, Country, CoronaData)
import Corona.JHU as JHU
import D3.Scatter.Type
import Data.Array as A
import Data.Array.ST as MA
import Data.Const
import Data.Dated (Dated(..))
import Data.Dated as D
import Data.Foldable
import Data.FunctorWithIndex
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
import Data.Semiring
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
import Type.Chain as C
import Type.Equality
import Type.Equiv

data BaseProjection a =
        Time      (a ~ Day)
      | Confirmed (a ~ Int)
      | Deaths    (a ~ Int)
      | Recovered (a ~ Int)

type Projection b = forall r.
        (forall a. { base       :: BaseProjection a
                   , operations :: C.Chain Operation a b
                   } -> r)
        -> r

data Condition a = AtLeast a | AtMost a

data Operation a b =
        Delta     (NType a) (a ~ b)       -- ^ dx/dt
      | PGrowth   (NType a) (b ~ Percent)   -- ^ (dx/dt)/x        -- how to handle percentage
      | Window    (NType a) (b ~ Number ) Int -- ^ moving average of x over t, window (2n+1)
      -- | PGrowth   (a ~ Number) (b ~ Percent)   -- ^ (dx/dt)/x        -- how to handle percentage
      -- | Window    (a ~ Number) (b ~ Number) Int -- ^ moving average of x over t, window (2n+1)
      -- | DaysSince SomeCondition (Equiv a JSDate) (Equiv b Duration)
            -- ^ says since x has met a condition
      -- | Chain     (forall r. (forall c. Operation a c -> Operation c b -> r) -> r)

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
        -- Delta     (NType a) (a ~ b)       -- ^ dx/dt
    Delta num rab -> \(Dated x) -> Dated
        { start: MJD.addDays 1 x.start
        , values: case num of
            NInt rac ->
                withConsec (\y0 y1 -> equivTo rab $ equivFrom rac $ equivTo rac y1 - equivTo rac y0)
                  x.values
            NNumber rac ->
                withConsec (\y0 y1 -> equivTo rab $ equivFrom rac $ equivTo rac y1 - equivTo rac y0)
                  x.values
            NPercent rac ->
                withConsec (\y0 y1 -> equivTo rab $ equivFrom rac $ equivTo rac y1 - equivTo rac y0)
                  x.values
        }
    PGrowth num rbp -> \(Dated x) -> Dated
        { start: MJD.addDays 1 x.start
        , values: case num of
            NInt rac ->
                withConsec (\y0 y1 -> equivFrom rbp $ percentGrowth
                                (toNumber $ equivTo rac y0)
                                (toNumber $ equivTo rac y1))
                    x.values
            NNumber rac ->
                withConsec (\y0 y1 -> equivFrom rbp $ percentGrowth
                                (equivTo rac y0)
                                (equivTo rac y1))
                    x.values
            NPercent rac ->
                withConsec (\y0 y1 -> equivFrom rbp $ percentGrowth
                                (unPercent $ equivTo rac y0)
                                (unPercent $ equivTo rac y1))
                    x.values
        }
    Window num rbn n -> \(Dated x) -> Dated
        { start: MJD.addDays n x.start
        , values: case num of
            NInt rac ->
                withWindow n (\ys -> equivFrom rbn $
                        toNumber (sum (map (equivTo rac) ys)) / toNumber (n*2+1))
                    x.values
            NNumber rac ->
                withWindow n (\ys -> equivFrom rbn $
                        sum (map (equivTo rac) ys) / toNumber (n*2+1))
                    x.values
            NPercent rac ->
                withWindow n (\ys -> equivFrom rbn $
                        sum (map (unPercent <<< equivTo rac) ys) / toNumber (n*2+1))
                    x.values
        }
    -- DaysSince c refl refl' -> ?e


applyBaseProjection
    :: forall a. 
       BaseProjection a
    -> Dated (Counts Int)
    -> Dated a
applyBaseProjection = case _ of
    Time      refl -> mapWithIndex (\i _ -> equivFrom refl i)
    Confirmed refl -> map (\c -> equivFrom refl c.confirmed)
    Deaths    refl -> map (\c -> equivFrom refl c.deaths   )
    Recovered refl -> map (\c -> equivFrom refl c.recovered)
    
    
applyProjection
    :: forall a.
       Projection a
    -> Dated (Counts Int)
    -> Dated a
applyProjection spr = spr (\pr ->
          C.runChainF applyOperation pr.operations <<< applyBaseProjection pr.base
        )

baseProjectionLabel :: forall a. BaseProjection a -> String
baseProjectionLabel = case _ of
    Time      _ -> "Date"
    Confirmed _ -> "Confirmed Cases"
    Deaths    _ -> "Deaths"
    Recovered _ -> "Recovered Cases"

operationLabel :: forall a b. Operation a b -> String
operationLabel = case _ of
    Delta   _ _   -> "Daily Increase in"
    PGrowth _ _   -> "Daily Percent Growth in"
    Window  _ _ n -> "Moving Average (size " <> show (2*n+1) <> ") of"

operationsLabel :: forall a b. C.Chain Operation a b -> String
operationsLabel c = res
  where
    go :: forall r s. Operation r s -> Const String r -> Const String s
    go o (Const s) = Const (s <> " " <> operationLabel o)
    Const res = C.runChainF go c (Const "Amount of")

projectionLabel :: forall a. Projection a -> String
projectionLabel spr = spr (\pr ->
        operationsLabel pr.operations <> " " <> baseProjectionLabel pr.base
    )

toAxisConf :: forall a. Projection a -> Scale a -> AxisConf a
toAxisConf spr scl = 
      { scale: scl
      , label: projectionLabel spr
      }

toSeries
    :: forall a b. 
       Projection a
    -> Projection b
    -> Dated (Counts Int)
    -> Array (Point a b)
toSeries pX pY ps = D.datedValues $
    lift2 (\x y -> {x, y}) (applyProjection pX ps) (applyProjection pY ps)

toScatterPlot
    :: forall a b.
       CoronaData
    -> Projection a
    -> Scale a
    -> Projection b
    -> Scale b
    -> Set Country
    -> ScatterPlot a b
toScatterPlot dat pX sX pY sY ctrys =
        { xAxis  : toAxisConf pX sX
        , yAxis  : toAxisConf pY sY
        , series : flip A.mapMaybe (A.fromFoldable ctrys) $ \ctry -> do
            cdat <- O.lookup ctry dat.counts
            pure 
              { name : ctry
              , values : toSeries pX pY (Dated { start: dat.start, values: cdat })
              }
        }


foreign import incrDate  :: Int -> JSDate -> JSDate
-- foreign import traceTime :: forall a b. DebugWarning => a -> (Unit -> b) -> b
