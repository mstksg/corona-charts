
module Corona.Chart where

import Prelude

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
import Data.Exists
import Data.Foldable
import Data.Functor.Product
import Data.FunctorWithIndex
import Data.Int
import Data.JSDate (JSDate)
import Data.JSDate as JSDate
import Data.List as L
import Data.List.Lazy as LL
import Data.List.NonEmpty as NE
import Data.Map as M
import Data.Maybe
import Data.ModifiedJulianDay (Day(..))
import Data.ModifiedJulianDay as MJD
import Data.Newtype
import Data.NonEmpty as NE
import Data.Number
import Data.Options
import Data.Ord
import Data.Ord.Max
import Data.Semiring
import Data.Sequence as Seq
import Data.Sequence.NonEmpty as NESeq
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple
import Data.Void
import Debug.Trace
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class
import Foreign.Object as O
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Query.EventSource as ES
import Type.Chain as C
import Type.DSum
import Type.Equality
import Type.Equiv
import Type.GCompare
import Undefined

data BaseProjection a =
        Time      (a ~ Day)
      | Confirmed (a ~ Int)
      | Deaths    (a ~ Int)
      | Recovered (a ~ Int)

baseType :: forall a. BaseProjection a -> SType a
baseType = case _ of
    Time      refl -> SDay refl
    Confirmed refl -> SInt refl
    Deaths    refl -> SInt refl
    Recovered refl -> SInt refl

derive instance eqBase :: Eq (BaseProjection a)
derive instance ordBase :: Ord (BaseProjection a)
instance gshowBase :: GShow BaseProjection where
    gshow = case _ of
      Time      _ -> "Time"
      Confirmed _ -> "Confirmed"
      Deaths    _ -> "Deaths"
      Recovered _ -> "Recovered"
instance showBase :: Show (BaseProjection a) where
    show = gshow

bTime      :: BaseProjection Day
bTime      = Time refl
bConfirmed :: BaseProjection Int
bConfirmed = Confirmed refl
bDeaths    :: BaseProjection Int
bDeaths    = Deaths refl
bRecovered :: BaseProjection Int
bRecovered = Recovered refl

instance decideBaseProjection :: Decide BaseProjection where
    decide = case _ of
      Time rX -> case _ of
        Time rY -> Just (equivFromF rY rX)
        _       -> Nothing
      Confirmed rX -> case _ of
        Confirmed rY -> Just (equivFromF rY rX)
        _       -> Nothing
      Deaths rX -> case _ of
        Deaths rY -> Just (equivFromF rY rX)
        _       -> Nothing
      Recovered rX -> case _ of
        Recovered rY -> Just (equivFromF rY rX)
        _       -> Nothing

instance geqBaseProjection :: GEq BaseProjection where
    geq = decide

allBaseProjections :: Array (Exists BaseProjection)
allBaseProjections = [
      mkExists (Time refl)
    , mkExists (Confirmed refl)
    , mkExists (Deaths refl)
    , mkExists (Recovered refl)
    ]


newtype Projection b = Projection (forall r.
        (forall a. { base       :: BaseProjection a
                   , operations :: C.Chain Operation a b
                   } -> r)
        -> r
      )

withProjection
    :: forall a b r.
       Projection b
    -> (forall a. { base       :: BaseProjection a
                  , operations :: C.Chain Operation a b } -> r
       )
    -> r
withProjection (Projection f) = f

projection
    :: forall a b.
       { base       :: BaseProjection a
       , operations :: C.Chain Operation a b
       }
    -> Projection b
projection x = Projection (\f -> f x)

instance gshowProjection :: GShow Projection where
    gshow spr = withProjection spr (\pr ->
        "{" <> show pr.base <> "}: " <> show pr.operations
      )
instance showProjection :: Show (Projection a) where
    show = gshow

data ToFractional a b =
      I2N (a ~ Int)     (b ~ Number)
    | N2N (a ~ Number)  (b ~ Number)
    | P2P (a ~ Percent) (b ~ Percent)

toFractional :: forall a. NType a -> Exists (ToFractional a)
toFractional = case _ of
    NInt r     -> mkExists (I2N r refl)
    NNumber r  -> mkExists (N2N r r)
    NPercent r -> mkExists (P2P r r)


toFractionalIn :: forall a b. ToFractional a b -> NType a
toFractionalIn = case _ of
    I2N r _ -> NInt r
    N2N r _ -> NNumber r
    P2P r _ -> NPercent r
toFractionalOut :: forall a b. ToFractional a b -> NType b
toFractionalOut = case _ of
    I2N _ r -> NNumber r
    N2N _ r -> NNumber r
    P2P _ r -> NPercent r

-- runToFractional :: forall a b. ToFractional a b -> a -> b
-- runToFractional = case _ of
--     I2N rai rbn -> equivFrom rbn <<< toNumber <<< equivTo rai
--     N2N ran rbn -> equivFrom rbn <<< equivTo ran
--     P2P rap rbp -> equivFrom rbp <<< equivTo rap

data Condition a = AtLeast a | AtMost a

derive instance eqCondition :: Eq a => Eq (Condition a)
derive instance ordCondition :: Ord a => Ord (Condition a)

instance functorCondition :: Functor Condition where
    map f = case _ of
      AtLeast x -> AtLeast (f x)
      AtMost  x -> AtMost (f x)

instance showCondition :: Show a => Show (Condition a) where
    show = case _ of
      AtLeast n -> "AtLeast " <> show n
      AtMost  n -> "AtMost " <> show n

gshowCondition :: forall a. SType a -> Condition a -> String
gshowCondition t = case _ of
      AtLeast n -> "AtLeast " <> sTypeShow t n
      AtMost  n -> "AtMost " <> sTypeShow t n

conditionValue :: forall a. Condition a -> a
conditionValue = case _ of
    AtLeast x -> x
    AtMost  x -> x

data CutoffType = After | Before

derive instance eqCutoffType :: Eq CutoffType
derive instance ordCutoffType :: Ord CutoffType
instance showCutoffType :: Show CutoffType where
    show = case _ of
      After  -> "After"
      Before -> "Before"

data Operation a b =
        Delta     (NType a) (a ~ b)       -- ^ dx/dt
      | PGrowth   (NType a) (b ~ Percent)   -- ^ (dx/dt)/x        -- how to handle percentage
      | Window    (ToFractional a b) Int -- ^ moving average of x over t, window (2n+1)
      | PMax      (NType a) (b ~ Percent)   -- ^ rescale to make max = 1 or -1
      | Restrict  (SType a) (a ~ b) CutoffType (Condition a)    -- ^ restrict before/after condition
            -- maybe this should take all types?
      | Take      (a ~ b) Int CutoffType    -- ^ take n
      | DayNumber (b ~ Days) CutoffType     -- ^ day number
      | PointDate (b ~ Day)     -- ^ day associated with point
      -- TODO: take based on date window

instance gshow2Operation :: GShow2 Operation where
    gshow2 = case _ of
      Delta   _ _ -> "Delta"
      PGrowth _ _ -> "PGrowth"
      Window  _ n -> "Window " <> show n
      PMax _ _    -> "PMax"
      Restrict nt _ co cond -> "Restrict " <> show co <> " (" <> gshowCondition nt cond <> ")"
      Take _ n co -> "Take " <> show n <> " " <> show co
      DayNumber _ c -> "DayNumber " <> show c
      PointDate _ -> "PointDate"
instance gshowOperation :: GShow (Operation a) where
    gshow = gshow2
instance showOperation :: Show (Operation a b) where
    show = gshow

operationType :: forall a b. Operation a b -> SType a -> SType b
operationType = case _ of
    Delta   _ r      -> equivToF r
    PGrowth _ r      -> \_ -> equivFromF r sPercent
    Window  tf _     -> \_ -> fromNType (toFractionalOut tf)
    PMax    _ r      -> \_ -> equivFromF r sPercent
    Restrict _ r _ _ -> equivToF r
    Take r _ _       -> equivToF r
    DayNumber r _    -> \_ -> equivFromF r sDays
    PointDate r      -> \_ -> equivFromF r sDay

-- operationInType :: forall a b. Operation a b -> SType a
-- operationInType = case _ of
--     Delta   nt _ -> fromNType nt
--     PGrowth nt _ -> fromNType nt
--     Window  tf _ -> toFractionalIn tf
--     DayNumber t _ _ -> t
-- operationOutType :: forall a b. Operation a b -> SType b
-- operationOutType = case _ of
--     Delta   nt r -> equivToF r (fromNType nt)
--     PGrowth _  r -> SPercent r
--     Window  tf _ -> toFractionalOut tf
--     DayNumber _ r _ -> SDays r

percentGrowth
    :: Number
    -> Number
    -> Percent
percentGrowth x y
    | x == zero = Percent nan
    | otherwise = Percent ((y - x) / x)

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
    :: forall a b.
       Operation a b
    -> Dated a
    -> Dated b
applyOperation = case _ of
    Delta num rab -> \(Dated x) -> Dated
        { start: MJD.addDays (-1) x.start
        , values: withConsec
            (\y0 y1 -> equivTo rab (nTypeSubtract num y1 y0))
            x.values
        }
    PGrowth num rbp -> \(Dated x) -> Dated
        { start: MJD.addDays (-1) x.start
        , values: withConsec (\y0 y1 -> equivFrom rbp $ percentGrowth
                                (nTypeNumber num y0)
                                (nTypeNumber num y1)
                  ) x.values
        }
    Window tf n -> \(Dated x) -> Dated
        { start: MJD.addDays n x.start
        , values: flip (withWindow n) x.values $ \ys ->
                    numberNType (toFractionalOut tf) $
                      sum (map (nTypeNumber (toFractionalIn tf)) ys)
                        / toNumber (n * 2 + 1)
        }
    PMax nt rbp -> \xs ->
      let ys :: Dated Number
          ys = nTypeNumber nt <$> xs
          maxAbs :: Number
          maxAbs = maybe 1.0 unwrap <<< flip foldMap ys $ \y ->
                     if y == zero
                       then Nothing
                       else Just (Max (abs y))
      in  equivFromF rbp $ Percent <<< (_ / maxAbs) <$> ys
    Restrict nt rab co cond ->
      let dropper = case co of
            After  -> D.dropUntil
            Before -> D.dropUntilEnd
      in  equivToF rab <<< dropper (applyCondition nt cond)
    Take rab n co ->
      let dropper = case co of
            After  -> D.take
            Before -> D.takeEnd
      in  equivToF rab <<< dropper n
    DayNumber rbds co -> \(Dated x) ->
      let emptyDated = Dated { start: x.start, values: [] }
      in  maybe emptyDated (equivFromF rbds) $
            case co of
              After  -> D.findHead (Dated x) <#> \{ day: d0 } ->
                  mapWithIndex (\i _ -> Days $ MJD.diffDays i d0) (Dated x)
              Before -> D.findLast (Dated x) <#> \{ day: dF } ->
                  mapWithIndex (\i _ -> Days $ MJD.diffDays i dF) (Dated x)
    PointDate rbd -> equivFromF rbd <<< mapWithIndex const

applyCondition :: forall a. SType a -> Condition a -> a -> Boolean
applyCondition t = case _ of
    AtLeast n -> \x -> sTypeCompare t x n == GT
    AtMost n  -> \x -> sTypeCompare t x n == LT

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
applyProjection spr allDat = withProjection spr (\pr ->
          C.runChain applyOperation pr.operations <<< applyBaseProjection pr.base
        ) allDat

baseProjection :: forall a. Projection a -> Exists BaseProjection
baseProjection pr = withProjection pr (mkExists <<< _.base)

baseProjectionLabel :: forall a. BaseProjection a -> String
baseProjectionLabel = case _ of
    Time      _ -> "Date"
    Confirmed _ -> "Confirmed Cases"
    Deaths    _ -> "Deaths"
    Recovered _ -> "Recovered Cases"

operationLabel :: forall a b. Operation a b -> String
operationLabel = case _ of
    Delta   _ _ -> "Daily Change in"
    PGrowth _ _ -> "Daily Percent Growth in"
    Window  _ n -> "Moving Average (±" <> show n <> ") of"
    PMax _ _    -> "Percent of maximum in"
    Restrict _ _ _ _ -> ""
    -- Restrict t _ co cond -> fold
    --     [ "(With only points "
    --     , case co of
    --         After -> "after"
    --         Before -> "before"
    --     , " being "
    --     , conditionLabel t cond
    --     , ")"
    --     ]
    Take r n c ->
      let cStr = case c of
            After -> "last"
            Before -> "first"
      in  "Keeping the " <> cStr <> " " <> show n <> " points of"
    DayNumber _ c -> case c of
      After  -> "Number of days since initial"
      Before -> "Number of days until final"
    PointDate _ -> "Observed date for value of"

-- | TODO: pretty print
conditionLabel :: forall a. SType a -> Condition a -> String
conditionLabel t = case _ of
    AtLeast n -> "at least " <> sTypeFormat t n
    AtMost  n -> "at most " <> sTypeFormat t n

operationsLabel :: forall a b. C.Chain Operation a b -> String
operationsLabel c = res
  where
    go :: forall r s. Operation r s -> Const String r -> Const String s
    go o (Const s) = Const (operationLabel o <> " " <> s)
    Const res = C.runChain go c (Const "")

projectionLabel :: forall a. Projection a -> String
projectionLabel spr = withProjection spr (\pr ->
        operationsLabel pr.operations <> baseProjectionLabel pr.base
    )

toAxisConf :: forall a. Projection a -> Scale a -> AxisConf a
toAxisConf spr scl =
      { scale: scl
      , label: projectionLabel spr
      }

toSeries
    :: forall a b c d.
       Projection a
    -> Projection b
    -> Projection c
    -> Projection d
    -> Dated (Counts Int)
    -> Array (Point a b c d)
toSeries pX pY pZ pT ps = D.datedValues $
    lift4 (\x y z t -> {x, y, z, t})
          (applyProjection pX ps)
          (applyProjection pY ps)
          (applyProjection pZ ps)
          (applyProjection pT ps)

toScatterPlot
    :: forall a b c d.
       CoronaData
    -> Projection a
    -> Scale a
    -> Projection b
    -> Scale b
    -> Projection c
    -> Scale c
    -> Projection d
    -> Scale d
    -> Set Country
    -> ScatterPlot a b c d
toScatterPlot dat pX sX pY sY pZ sZ pT sT ctrys =
        { xAxis  : toAxisConf pX sX
        , yAxis  : toAxisConf pY sY
        , zAxis  : toAxisConf pZ sZ
        , tAxis  : toAxisConf pT sT
        , series : flip A.mapMaybe (A.fromFoldable ctrys) $ \ctry -> do
            cdat <- O.lookup ctry dat.counts
            pure
              { name : ctry
              , values : toSeries pX pY pZ pT (Dated { start: dat.start, values: cdat })
              }
        }

setBase :: forall a. BaseProjection a -> DSum SType Projection -> DSum SType Projection
setBase base dp = withDSum dp (\tB spr ->
      withProjection spr (\pr ->
        let tC = baseType pr.base
        in  case decide tA tC of
              Just refl -> tB :=> projection {
                  base: equivToF refl base
                , operations: pr.operations
                }
              Nothing   -> tA :=> projection {
                  base: base
                , operations: C.nil
                }
      )
    )
  where
    tA = baseType base

setOperations :: forall a b c. SType a -> C.Chain Operation a c -> Projection b -> Maybe (Projection c)
setOperations tA chain spr = withProjection spr (\pr -> do
      let tQ = baseType pr.base
      refl <- decide tA tQ
      pure $ projection { base: equivFromF refl pr.base, operations: chain }
    )

foreign import incrDate  :: Int -> JSDate -> JSDate
foreign import traceTime :: forall a b. DebugWarning => a -> (Unit -> b) -> b
foreign import spy :: forall a. String -> a -> a

