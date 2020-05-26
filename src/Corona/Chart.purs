
module Corona.Chart where

import Prelude

import Control.Apply
import Control.Monad.ST as ST
import Corona.Data.Type
import D3.Scatter.Type
import Data.Array as A
import Data.Array.ST as MA
import Data.Bundle
import Data.Const
import Data.Dated (Dated(..))
import Data.Dated as D
import Data.Exists
import Data.Foldable
import Data.Functor.Compose
import Data.Functor.Product
import Data.FunctorWithIndex
import Data.Int
import Data.JSDate (JSDate)
import Data.JSDate as JSDate
import Data.Lazy
import Data.Lens
import Data.Lens.Record as LR
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
import Data.Point
import Data.Semiring
import Data.Sequence as Seq
import Data.Sequence.NonEmpty as NESeq
import Data.Set (Set)
import Data.Set as Set
import Data.Symbol (SProxy(..))
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

baseLens :: forall a. BaseProjection Int -> Lens' (Counts a) a
baseLens = case _ of
    Time _      -> undefined    -- illegal but purescript cannot verify
    Confirmed _ -> LR.prop (SProxy :: SProxy "confirmed")
    Deaths    _ -> LR.prop (SProxy :: SProxy "deaths")
    Recovered _ -> LR.prop (SProxy :: SProxy "recovered")

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
    :: forall h a b. Functor h
    => Operation a b
    -> Bundle h Dated a
    -> Bundle h Dated b
applyOperation = case _ of
    Delta num rab -> hoistBundle $ \(Dated x) -> Dated
        { start: MJD.addDays 1 x.start
        , values: withConsec
            (\y0 y1 -> equivTo rab (nTypeSubtract num y1 y0))
            x.values
        }
    PGrowth num rbp -> hoistBundle $ \(Dated x) -> Dated
        { start: MJD.addDays 1 x.start
        , values: withConsec (\y0 y1 -> equivFrom rbp $ percentGrowth
                                (nTypeNumber num y0)
                                (nTypeNumber num y1)
                  ) x.values
        }
    Window tf n -> hoistBundle $ \(Dated x) -> Dated
        { start: MJD.addDays n x.start
        , values: flip (withWindow n) x.values $ \ys ->
                    numberNType (toFractionalOut tf) $
                      sum (map (nTypeNumber (toFractionalIn tf)) ys)
                        / toNumber (n * 2 + 1)
        }
    PMax nt rbp -> equivFromF rbp <<< pMax <<< hoistBundle (map (nTypeNumber nt))
    Restrict nt rab co cond -> equivToF rab <<< mapPrincipal (
        let dropper = case co of
              After  -> D.dropUntil
              Before -> D.dropUntilEnd
        in  dropper (applyCondition nt cond)
      )
    Take rab n co -> equivToF rab <<< mapPrincipal (
        let dropper = case co of
              After  -> D.take
              Before -> D.takeEnd
        in  dropper n
      )
    DayNumber rbds co -> \bundle -> equivFromF rbds $
      fromMaybe (hoistBundle D.clearDated bundle) case co of
        After  -> D.findHead (bundlePrincipal bundle) <#> \{ day: d0 } ->
          hoistBundle (mapWithIndex (\i _ -> Days $ MJD.diffDays i d0)) bundle
        Before -> D.findLast (bundlePrincipal bundle) <#> \{ day: dF } ->
          hoistBundle (mapWithIndex (\i _ -> Days $ MJD.diffDays i dF)) bundle
    PointDate rbd -> hoistBundle $ equivFromF rbd <<< mapWithIndex const
  where
    pMax :: Bundle h Dated Number -> Bundle h Dated Percent
    pMax b = hoistBundle (map (Percent <<< (_ / maxAbs))) b
      where
        maxAbs :: Number
        maxAbs = maybe 1.0 unwrap <<< flip foldMap (unwrap b).principal $ \y ->
          if y == zero || isNaN y
            then Nothing
            else Just (Max (abs y))

applyOperations
    :: forall h a b. Functor h
    => C.Chain Operation a b
    -> Bundle h Dated a
    -> Bundle h Dated b
applyOperations = C.runChain applyOperation

applyCondition :: forall a. SType a -> Condition a -> a -> Boolean
applyCondition t = case _ of
    AtLeast n -> \x -> sTypeCompare t x n == GT
    AtMost n  -> \x -> sTypeCompare t x n == LT

newtype TimeCounts a = TC
    { start     :: Day
    , timespan  :: Int          -- ^ length - 1
    , counts    :: Counts (Lazy (Array a))
    }

mkTimeCounts
    :: Day                  -- ^ start
    -> Counts (Array Int)   -- ^ data
    -> TimeCounts Int
mkTimeCounts start dat = TC
    { start
    , timespan: maxLen - 1
    , counts: mapCounts (defer <<< const) dat
    }
  where
    maxLen = A.length dat.confirmed
       `max` A.length dat.deaths
       `max` A.length dat.recovered


applyBaseProjection
    :: forall a.
       BaseProjection a
    -> TimeCounts Int
    -> Dated a
applyBaseProjection = case _ of
    Time      r -> \(TC tc) -> D.generate tc.start tc.timespan (equivFrom r)
    Confirmed r -> \(TC tc) -> equivFromF r $
      Dated { start: tc.start, values: force tc.counts.confirmed }
    Deaths    r -> \(TC tc) -> equivFromF r $
      Dated { start: tc.start, values: force tc.counts.deaths }
    Recovered r -> \(TC tc) -> equivFromF r $
      Dated { start: tc.start, values: force tc.counts.recovered }

applyProjection
    :: forall h a. Functor h
    => Projection a
    -> Bundle h TimeCounts Int
    -> Bundle h Dated a
applyProjection spr allDat = withProjection spr (\pr ->
          applyOperations pr.operations <<< hoistBundle (applyBaseProjection pr.base)
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
    -- Window  _ n -> "Moving Average (±" <> show n <> ") of"
    Window  _ _ -> ""
    PMax _ _    -> "Percent of maximum of"
    Restrict _ _ _ _ -> ""
    -- Restrict t _ co cond -> fold
    --     [ "("
    --     , case co of
    --         After -> "after"
    --         Before -> "before"
    --     , " "
    --     , conditionLabel t cond
    --     , ")"
    --     ]
    Take r n c ->
      let cStr = case c of
            After -> "last"
            Before -> "first"
      in  "Keeping the " <> cStr <> " " <> show n <> " points of"
    DayNumber _ c -> case c of
      After  -> "Days of"
      Before -> "Days left of"
    PointDate _ -> "Date for value of"

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

newtype ProjScale a = PS
    { projection :: Projection a
    , scale :: Scale a
    }

toAxisConf :: forall a. ProjScale a -> AxisConf a
toAxisConf (PS { projection, scale }) = AC
      { scale: scale
      , label: projectionLabel projection
      }

toSeries
    :: forall h a b c d. Functor h
    => PointF Projection a b c d
    -> Bundle h TimeCounts Int
    -> { principal :: Array (Point a b c d)
       , mirrored :: PointF (Compose h Dated) a b c d
       }
toSeries projs bs =
    { principal: D.datedValues $ lift4 (\x y z t -> {x, y, z, t})
          principalOut.x
          principalOut.y
          principalOut.z
          principalOut.t
    , mirrored: mirroredOut
    }
  where
    bundleRes :: PointF (Bundle h Dated) a b c d
    bundleRes = hoistPointF (flip applyProjection bs) projs
    principalOut :: PointF Dated a b c d
    principalOut = hoistPointF bundlePrincipal bundleRes
    mirroredOut :: PointF (Compose h Dated) a b c d
    mirroredOut = hoistPointF (Compose <<< bundleMirrored) bundleRes

-- applyProjection
--     :: forall h a. Functor h
--     => Projection a
--     -> Bundle h TimeCounts Int
--     -> Bundle h Dated a
--     -- D.datedValues $
--     -- lift4 (\x y z t -> {x, y, z, t})
--     --     (bundlePrincipal $ applyProjection x b)
--     --     (bundlePrincipal $ applyProjection y b)
--     --     (bundlePrincipal $ applyProjection z b)
--     --     (bundlePrincipal $ applyProjection t b)
--   -- where
--     -- b :: Bundle Array TimeCounts Int
--     -- b = mkBundle ps []


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

