
module Corona.Chart.UI.Op where

import Prelude

import Control.Monad.State.Class as State
import Corona.Chart
import D3.Scatter.Type (SType(..))
import D3.Scatter.Type as D3
import Data.Array as A
import Data.Date as D
import Halogen.ChainPicker (Picker(..), SomePicker(..), PickerQuery(..))
import Data.Either
import Data.Enum
import Data.Exists
import Data.Functor.Product
import Data.FunctorWithIndex
import Data.Int
import Data.Int.Parse
import Data.Maybe
import Data.ModifiedJulianDay (Day)
import Data.ModifiedJulianDay as MJD
import Data.Number as N
import Data.Ord
import Data.Symbol (SProxy(..))
import Data.Traversable
import Data.Tuple
import Effect.Class
import Effect.Class.Console
import Halogen as H
import Halogen.ChainPicker as ChainPicker
import Halogen.HTML as HH
import Halogen.HTML.CSS as HC
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Util as HU
import Type.Chain as C
import Type.DProd
import Type.DSum
import Type.Equiv
import Undefined

pickerMap :: forall m a. SType a -> Array (SomePicker SType Operation m a)
pickerMap t0 = case t0 of
    SDay  _ -> [
        t0       :=> restrictPickOp t0
      , t0       :=> takePickOp
      , t0       :=> lagPickOp
      , D3.sDays :=> dayNumberPickOp
      , D3.sDay  :=> pointDatePickOp
      ]
    SDays _ -> [
        t0       :=> restrictPickOp t0
      , t0       :=> takePickOp
      , t0       :=> lagPickOp
      , D3.sDays :=> dayNumberPickOp
      , D3.sDay  :=> pointDatePickOp
      ]
    SInt  r -> [
        t0          :=> deltaPickOp (D3.NInt r)
      , D3.sPercent :=> pgrowthPickOp (D3.NInt r)
      , D3.sNumber  :=> windowPickOp (I2N r refl)
      , D3.sPercent :=> pmaxPickOp (D3.NInt r)
      , t0          :=> restrictPickOp t0
      , t0          :=> takePickOp
      , t0          :=> lagPickOp
      , D3.sDays    :=> dayNumberPickOp
      , D3.sNumber  :=> perCapitaPickOp (I2N r refl)
      , D3.sDay     :=> pointDatePickOp
      ]
    SNumber r -> [
        t0          :=> deltaPickOp (D3.NNumber r)
      , D3.sPercent :=> pgrowthPickOp (D3.NNumber r)
      , D3.sNumber  :=> windowPickOp (N2N r refl)
      , D3.sPercent :=> pmaxPickOp (D3.NNumber r)
      , t0          :=> restrictPickOp t0
      , t0          :=> takePickOp
      , t0          :=> lagPickOp
      , D3.sDays    :=> dayNumberPickOp
      , D3.sNumber  :=> perCapitaPickOp (N2N r refl)
      , D3.sDay     :=> pointDatePickOp
      ]
    SPercent r -> [
        t0          :=> deltaPickOp (D3.NPercent r)
      , D3.sPercent :=> pgrowthPickOp (D3.NPercent r)
      , D3.sPercent :=> windowPickOp (P2P r refl)
      , D3.sPercent :=> pmaxPickOp (D3.NPercent r)
      , t0          :=> restrictPickOp t0
      , t0          :=> takePickOp
      , t0          :=> lagPickOp
      , D3.sDays    :=> dayNumberPickOp
      , D3.sPercent :=> perCapitaPickOp (P2P r refl)
      , D3.sDay     :=> pointDatePickOp
      ]


type PickOp = Picker Operation

fakeState
    :: forall s r s m. Applicative m
    => (s -> Tuple r s)
    -> s
    -> m (Maybe r)
fakeState f x = pure (Just (fst (f x)))

toStateHandles
    :: forall a b s.
       (Operation a b -> Maybe s)
    -> Operation a b
    -> Boolean
toStateHandles f = isJust <<< f

toStateInitialize
    :: forall a b s.
       (Operation a b -> Maybe s)
    -> s
    -> Maybe (Operation a b)
    -> s
toStateInitialize f def x = fromMaybe def (f =<< x)

type MkPickOp a b s act m =
    { label :: String
    , render :: s -> H.ComponentHTML act () m
    , toState :: Operation a b -> Maybe s
    , modify :: act -> s -> s
    , fromState :: s -> Operation a b
    , defaultState :: s
    }

mkPickOp
    :: forall a b s act m.
       MkPickOp a b s act m
    -> PickOp m a b
mkPickOp mpo = Picker
    { label: mpo.label
    , component: H.mkComponent {
        initialState: \x -> fromMaybe mpo.defaultState (mpo.toState =<< x)
      , render: mpo.render
      , eval: H.mkEval $ H.defaultEval {
          handleAction = \a -> do
             State.modify_ (mpo.modify a)
             H.raise unit
        , handleQuery = \(PQState f) -> do
            s <- State.get
            let Tuple x new = f (mpo.fromState s)
            traverse_ H.put (mpo.toState new)
            pure (Just x)
        }
      }
    , handles: isJust <<< mpo.toState
    }

-- | Delta     (NType a) (a ~ b)       -- ^ dx/dt
deltaPickOp :: forall m a. D3.NType a -> PickOp m a a
deltaPickOp nt = mkPickOp
    { label: "Daily Change"
    , render: \_ -> HH.div [HU.classProp "daily-change"] []
    , toState: case _ of
        Delta _ _ -> Just unit
        _         -> Nothing
    , modify: const
    , fromState: \_ -> Delta nt refl
    , defaultState: unit
    }

-- | PGrowth   (NType a) (b ~ Percent)   -- ^ (dx/dt)/x        -- how to handle percentage
pgrowthPickOp :: forall m a. D3.NType a -> PickOp m a D3.Percent
pgrowthPickOp nt = mkPickOp
    { label: "Percent Growth"
    , render: \_ -> HH.div [HU.classProp "percent-growth"] []
    , toState: case _ of
        PGrowth _ _ -> Just unit
        _           -> Nothing
    , modify: const
    , fromState: \_ -> PGrowth nt refl
    , defaultState: unit
    }

-- | Window    (ToFractional a b) Int -- ^ moving average of x over t, window (2n+1)
windowPickOp :: forall m a b. ToFractional a b -> PickOp m a b
windowPickOp tf = mkPickOp
    { label: "Moving Average"
    , render: \st -> HH.div [HU.classProp "moving-average"] [
        HH.span_ [HH.text "Window (before/after)"]
      , HH.input [
          HP.type_ HP.InputNumber
        , HP.value (show st)
        , HE.onValueChange parseWindow
        ]
      ]
    , toState: case _ of
        Window _ j -> Just j
        _          -> Nothing
    , modify: const
    , fromState: Window tf
    , defaultState: 1
    }
  where
    parseWindow = map (abs <<< round) <<< N.fromString

-- | PMax      (NType a) (b ~ Percent)   -- ^ rescale to make max = 1 or -1
pmaxPickOp :: forall m a. D3.NType a -> PickOp m a D3.Percent
pmaxPickOp nt = mkPickOp
    { label: "Percent of Maximum"
    , render: \_ -> HH.div [HU.classProp "percent-of-maximum"] []
    , toState: case _ of
        PMax _ _ -> Just unit
        _        -> Nothing
    , modify: const
    , fromState: \_ -> PMax nt refl
    , defaultState: unit
    }

type RestrictState  a =
      { cutoffType :: CutoffType
      , condition  :: Condition a
      }

data RestrictAction a = RASetType CutoffType
                      | RASetCondType (Condition Unit)
                      | RASetLimit a

-- | Restrict  (SType a) (a ~ b) CutoffType (Condition a)    -- ^ restrict before/after condition
restrictPickOp :: forall m a. SType a -> PickOp m a a
restrictPickOp t = mkPickOp
    { label: "Restrict"
    , render: \st -> HH.div [HU.classProp "restrict"] [
        HH.span_ [HH.text "Keep points..."]
      , HH.select [ HU.classProp "cutoff-list"
                  , HE.onSelectedIndexChange (map RASetType <<< (cutoffList A.!! _))
                  ] $
          cutoffList <#> \c ->
            let isSelected = c == st.cutoffType
            in  HH.option [HP.selected isSelected] [HH.text (showCutoff c)]
      , HH.span_ [HH.text "...being..."]
      , HH.select [ HU.classProp "condition-list"
                  , HE.onSelectedIndexChange (map RASetCondType <<< (condList A.!! _))
                  ] $
          condList <#> \c ->
            let isSelected = c == void st.condition
            in  HH.option [HP.selected isSelected] [HH.text (showCond c)]
      , HH.div [HU.classProp "cond-num-picker"] [
          HH.input [
            HP.type_ inputType
          , HP.value (inputShow (conditionValue st.condition))
          , HE.onValueChange (map RASetLimit <<< inputParse)
          ]
        ]
      ]
    , toState: case _ of
        Restrict _ _ ct cond -> Just { cutoffType: ct, condition: cond}
        _                    -> Nothing
    , modify: \act st ->
        case act of
          RASetType co -> st { cutoffType = co }
          RASetCondType cu ->
            st { condition = conditionValue st.condition <$ cu }
          RASetLimit v ->
            st { condition = v <$ st.condition }
    , fromState: \st -> Restrict t refl st.cutoffType st.condition
    , defaultState:
        { cutoffType: After
        , condition: AtLeast $ case t of
            SDay r -> equivFrom r $ MJD.fromDate $
                        D.canonicalDate
                          (toEnumWithDefaults bottom top 2020)
                          D.January
                          (toEnumWithDefaults bottom top 22)
            SDays r -> equivFrom r $ D3.Days 0
            SInt r -> equivFrom r 100
            SNumber r -> equivFrom r 100.0
            SPercent r -> equivFrom r $ D3.Percent 0.2
        }
    }
  where
    { inputType, inputParse, inputShow } = inputField t
    showCutoff = case _ of
      After  -> "after"
      Before -> "before"
    showCond = case _ of
      AtLeast _ -> "at least"
      AtMost  _ -> "at most"

type TakeState  a =
      { cutoffType :: CutoffType
      , amount     :: Int
      }

data TakeAction a = TASetType CutoffType
                  | TASetAmount Int

-- | Take      (a ~ b) Int CutoffType    -- ^ take n
takePickOp :: forall m a. PickOp m a a
takePickOp = mkPickOp
    { label: "Take Amount"
    , render: \st -> HH.div [HU.classProp "take-amount"] [
        HH.span_ [HH.text "Keep only the..."]
      , HH.select [ HU.classProp "cutoff-list"
                  , HE.onSelectedIndexChange (map TASetType <<< (cutoffList A.!! _))
                  ] $
          cutoffList <#> \c ->
            let isSelected = c == st.cutoffType
            in  HH.option [HP.selected isSelected] [HH.text (showCutoff c)]
      , HH.div [HU.classProp "cond-num-picker"] [
          HH.input [
            HP.type_ HP.InputNumber
          , HP.value (show st.amount)
          , HE.onValueChange (map TASetAmount <<< parseAmount)
          ]
        ]
      , HH.span_ [HH.text "...points"]
      ]
    , toState: case _ of
        Take _ a t -> Just { amount: a, cutoffType: t }
        _          -> Nothing
    , modify: case _ of
        TASetType  co -> _ { cutoffType = co }
        TASetAmount v -> _ { amount = v }
    , fromState: \st -> Take refl st.amount st.cutoffType
    , defaultState:
        { cutoffType: Before
        , amount: 30
        }
    }
  where
    parseAmount = map round <<< N.fromString
    showCutoff = case _ of
      After  -> "first"
      Before -> "last"

-- | Lag       (a ~ b) Int               -- ^ lag amount
lagPickOp :: forall m a. PickOp m a a
lagPickOp = mkPickOp
    { label: "Lag"
    , render: \st -> HH.div [HU.classProp "lag"] [
        HH.span_ [HH.text "Lag time (days)"]
      , HH.input [
          HP.type_ HP.InputNumber
        , HP.value (show st)
        , HE.onValueChange parseAmount
        ]
      ]
    , toState: case _ of
        Lag _ j -> Just j
        _       -> Nothing
    , modify: const
    , fromState: Lag refl
    , defaultState: 13
    }
  where
    parseAmount = map round <<< N.fromString



-- | DayNumber (b ~ Days) CutoffType     -- ^ day number
dayNumberPickOp :: forall m a b. PickOp m a D3.Days
dayNumberPickOp = mkPickOp
    { label: "Day Count"
    , render: \st -> HH.div [HU.classProp "day-count"] [
        HH.span_ [HH.text "Days since/until..."]
      , HH.select [HU.classProp "cutoff-list", HE.onSelectedIndexChange (cutoffList A.!! _)] $
          cutoffList <#> \c ->
            let isSelected = c == st
            in  HH.option [HP.selected isSelected] [HH.text (showCutoff c)]
      ]
    , toState: case _ of
        DayNumber _ c -> Just c
        _             -> Nothing
    , modify: const
    , fromState: DayNumber refl
    , defaultState: After
    }
  where
    showCutoff = case _ of
      After  -> "first day"
      Before -> "last day"

-- | PerCapita (ToFractional a b)        -- ^ number per million population
perCapitaPickOp :: forall m a b. ToFractional a b -> PickOp m a b
perCapitaPickOp tf = mkPickOp
    { label: "Per Million People"
    , render: \st -> HH.div [HU.classProp "per-million-people"] []
    , toState: case _ of
        PerCapita _ -> Just unit
        _          -> Nothing
    , modify: const
    , fromState: \_ -> PerCapita tf
    , defaultState: unit
    }



-- | PointDate (b ~ Day)     -- ^ day associated with point
pointDatePickOp :: forall m a. PickOp m a Day
pointDatePickOp = mkPickOp
    { label: "Date Observed"
    , render: \_ -> HH.div [HU.classProp "date-observed"] []
    , toState: case _ of
        PointDate _ -> Just unit
        _           -> Nothing
    , modify: const
    , fromState: \_ -> PointDate refl
    , defaultState: unit
    }

condList :: Array (Condition Unit)
condList = [AtLeast unit, AtMost unit]

cutoffList :: Array CutoffType
cutoffList = [After, Before]

type InputField a =
    { inputType  :: HP.InputType
    , inputParse :: String -> Maybe a
    , inputShow  :: a -> String
    }

inputField :: forall a. SType a -> InputField a
inputField t =
    { inputType: case t of
        SDay _ -> HP.InputDate
        _         -> HP.InputNumber
    , inputParse: case t of
        SDay r -> equivFromF r <<< MJD.fromISO8601
        SDays r -> map (equivFrom r <<< D3.Days <<< round) <<< N.fromString
        SInt    r -> map (equivFrom r <<< round) <<< N.fromString
        SNumber r -> map (equivFrom r) <<< N.fromString
        SPercent r -> map (equivFrom r <<< D3.Percent <<< (_ / 100.0))
                     <<< N.fromString
    , inputShow: case t of
        SDay r -> MJD.toISO8601 <<< equivTo r
        SDays r -> show <<< D3.unDays <<< equivTo r
        SInt r -> show <<< equivTo r
        SNumber r -> showPrecision <<< equivTo r
        SPercent r -> showPrecision <<< (_ * 100.0) <<< D3.unPercent <<< equivTo r
    }
  where
    showPrecision x = if toNumber (round x) == x then show (round x) else show x
