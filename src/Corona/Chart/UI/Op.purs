
module Corona.Chart.UI.Op where


import Prelude

import Control.Monad.State.Class as State
import Corona.Chart
import D3.Scatter.Type as D3
import Data.Array as A
import Data.Either
import Data.Exists
import Data.Functor.Product
import Data.FunctorWithIndex
import Data.Int
import Data.Int.Parse
import Data.Maybe
import Debug.Trace
import Data.ModifiedJulianDay (Day)
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

validPickOps :: forall m a. D3.SType a -> Array (SomePickOp m a)
validPickOps t0 = case t0 of
    D3.SDay  _ -> [ D3.sDays :=> dayNumberPickOp ]
    D3.SDays _ -> [ D3.sDays :=> dayNumberPickOp ]
    D3.SInt  r -> [
        t0          :=> deltaPickOp (D3.NInt r)
      , D3.sPercent :=> pgrowthPickOp (D3.NInt r)
      , D3.sNumber  :=> windowPickOp (I2N r refl)
      , t0          :=> restrictPickOp (D3.NInt r)
      , D3.sDays    :=> dayNumberPickOp
      ]
    D3.SNumber r -> [
        t0          :=> deltaPickOp (D3.NNumber r)
      , D3.sPercent :=> pgrowthPickOp (D3.NNumber r)
      , D3.sNumber  :=> windowPickOp (N2N r refl)
      , t0          :=> restrictPickOp (D3.NNumber r)
      , D3.sDays    :=> dayNumberPickOp
      ]
    D3.SPercent r -> [
        t0          :=> deltaPickOp (D3.NPercent r)
      , D3.sPercent :=> pgrowthPickOp (D3.NPercent r)
      , D3.sPercent :=> windowPickOp (P2P r refl)
      , t0          :=> restrictPickOp (D3.NPercent r)
      , D3.sDays    :=> dayNumberPickOp
      ]

type State =
    { pickOpIx :: Int
    }

newtype SomePickOpQuery a r = SomePQState
  (forall b. D3.SType b -> Operation a b -> Tuple r (Operation a b))
  -- ^ it could possibly change the result type?  nah.  not in this case. we
  -- ignore if result type is different

type ChildSlots a =
        ( pickOp :: H.Slot (SomePickOpQuery a)
                           PickOpOutput
                           Int
        )

data Output = ChangeEvent (Exists D3.SType)

data Action = SetPickOpIx Int
            | TriggerUpdate

data Query a r = QueryOp
  (forall b. D3.SType b -> Operation a b
      -> Tuple r (DSum D3.SType (Operation a)))

component
    :: forall a i m. MonadEffect m
    => D3.SType a
    -> H.Component HH.HTML (Query a) i Output m
component t0 =
    H.mkComponent
      { initialState: \_ -> { pickOpIx: 0 }
      , render: render pos
      , eval: H.mkEval $ H.defaultEval
          { handleAction = handleAction pos
          , handleQuery  = handleQuery
          }
      }
  where
    pos = validPickOps t0

render
    :: forall m a.
       Array (SomePickOp m a)
    -> State
    -> H.ComponentHTML Action (ChildSlots a) m
render pos st = HH.div [HU.classProp "single-op-picker"] [
      HH.select [ HE.onSelectedIndexChange (map SetPickOpIx <<< goodIx) ] $
        flip mapWithIndex pos $ \i dspo -> withDSum dspo (\t (PickOp po) ->
          HH.option [HP.selected (i == st.pickOpIx)]
            [ HH.text po.label ]
        )
    , HH.div [HU.classProp "single-op-options"] $
        maybe [] (A.singleton <<< mkSlot) (pos A.!! st.pickOpIx)
    ]
  where
    goodIx i
      | i < A.length pos = Just i
      | otherwise        = Nothing
    mkSlot :: SomePickOp m a -> H.ComponentHTML Action (ChildSlots a) m
    mkSlot dspo = withDSum dspo (\t (PickOp po) ->
        HH.slot _pickOp st.pickOpIx
          (HU.hoistQuery (\(SomePQState f) -> PQState (f t)) po.component)
          unit
          (const (Just TriggerUpdate))
      )


handleAction
    :: forall a m. MonadEffect m
    => Array (SomePickOp m a)
    -> Action
    -> H.HalogenM State Action (ChildSlots a) Output m Unit
handleAction pos act = do
    oldIx <- H.gets (_.pickOpIx)
    st <- H.get
    case act of
      SetPickOpIx i -> H.put { pickOpIx: i }
      TriggerUpdate -> pure unit
    newIx <- H.gets (_.pickOpIx)

    case pos A.!! newIx of
      Nothing   -> log "hey what gives"
      Just dspo -> withDSum dspo (\t _ ->
        H.raise $ ChangeEvent (mkExists t)
      )

handleQuery
    :: forall a m r. MonadEffect m
    => Query a r
    -> H.HalogenM State Action (ChildSlots a) Output m (Maybe r)
handleQuery = case _ of
    QueryOp f -> do
      st <- H.get
      subSt <- H.query _pickOp st.pickOpIx $
          SomePQState (\t op -> Tuple (t :=> op) op)
      for subSt $ \o -> do
        let Tuple r dso = withDSum o f
        -- this needs to be reworked better: setting op's needs to go to the
        -- right index
        -- withDSum dso (\tNew oNew -> void $
        --    H.query _pickOp st.pickOpIx $
        --       SomePQState (\tOld oOld ->
        --           case decide tOld tNew of
        --             Nothing -> trace "what the" $ const $ Tuple unit oOld
        --             Just r  -> trace "ok huh" $ const $ Tuple unit (equivFromF r oNew)
        --         )
        -- )
        pure r
           
_pickOp :: SProxy "pickOp"
_pickOp = SProxy








data PickOpQuery a b r = PQState (Operation a b -> Tuple r (Operation a b))

data PickOpOutput = PickOpUpdate

type SomePickOp m a = DSum D3.SType (PickOp m a)

newtype PickOp m a b = PickOp
      { label     :: String
      , component :: H.Component HH.HTML (PickOpQuery a b) Unit PickOpOutput m
      }

fakeState
    :: forall s r s m. Applicative m
     => s -> (s -> Tuple r s) -> m (Maybe r)
fakeState x f = pure (Just (fst (f x)))

deltaPickOp :: forall m a. D3.NType a -> PickOp m a a
deltaPickOp nt = PickOp {
      label: "Daily Change"
    , component: H.mkComponent {
        initialState: \_ -> unit
      , render: \_ -> HH.div [HU.classProp "daily-change"] []
      , eval: H.mkEval $ H.defaultEval {
          handleAction = \_ -> H.raise PickOpUpdate
        , handleQuery  = case _ of
            PQState f -> fakeState (Delta nt refl) f
        }
      }
    }

pgrowthPickOp :: forall m a. D3.NType a -> PickOp m a D3.Percent
pgrowthPickOp nt = PickOp {
      label: "Percent Growth"
    , component: H.mkComponent {
        initialState: \_ -> unit
      , render: \_ -> HH.div [HU.classProp "percent-growth"] []
      , eval: H.mkEval $ H.defaultEval {
          handleAction = \_ -> H.raise PickOpUpdate
        , handleQuery  = case _ of
            PQState f -> fakeState (PGrowth nt refl) f
        }
      }
    }

windowPickOp :: forall m a b. ToFractional a b -> PickOp m a b
windowPickOp tf = PickOp {
      label: "Moving Average"
    , component: H.mkComponent {
        initialState: \_ -> 1
      , render: \st -> HH.div [HU.classProp "moving-average"] [
          HH.span_ [HH.text "Window size (before/after)"]
        , HH.input [
            HP.type_ HP.InputNumber
          , HP.value (show st)
          , HE.onValueInput parseWindow
          ]
        ]
      , eval: H.mkEval $ H.defaultEval {
          handleAction = \st -> do
            State.put st
            H.raise PickOpUpdate
        , handleQuery  = case _ of
            PQState f -> do
              i <- State.get
              case f (Window tf i) of
                Tuple x (Window _ j) -> do
                  H.put j
                  pure $ Just x
                _ -> pure Nothing
        }
      }
    }
  where
    parseWindow = map (abs <<< round) <<< N.fromString

type RestrictState  a =
      { cutoffType :: CutoffType
      , condition  :: Condition a
      }

data RestrictAction a = RASetType CutoffType
                      | RASetCondType (Condition Unit)
                      | RASetLimit a


restrictPickOp :: forall m a. D3.NType a -> PickOp m a a
restrictPickOp nt = PickOp {
      label: "Restrict"
    , component: H.mkComponent {
        initialState: \_ ->
            { cutoffType: After
            , condition: AtLeast $ D3.numberNType nt (toNumber 100)
            }
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
              HP.type_ HP.InputNumber
            , HP.value (showCondValue (conditionValue st.condition))
            , HE.onValueInput (map RASetLimit <<< parseCondValue)
            ]
          ]
        ]
      , eval: H.mkEval $ H.defaultEval {
          handleAction = \act -> do
            H.modify_ $ \st ->
              case act of
                RASetType co -> st { cutoffType = co }
                RASetCondType cu ->
                  st { condition = conditionValue st.condition <$ cu }
                RASetLimit v ->
                  st { condition = v <$ st.condition }
            H.raise PickOpUpdate
        , handleQuery  = case _ of
            PQState f -> do
              st <- State.get
              case f (Restrict nt refl st.cutoffType st.condition) of
                Tuple x (Restrict _ _ ct cond) -> do
                  H.put { cutoffType: ct, condition: cond }
                  pure $ Just x
                _ -> pure Nothing
        }
      }
    }
  where
    showCondValue = case nt of
      D3.NInt r -> show <<< equivTo r
      D3.NNumber r -> show <<< equivTo r
      D3.NPercent r -> show <<< (_ * toNumber 100) <<< D3.unPercent <<< equivTo r
    parseCondValue :: String -> Maybe a
    parseCondValue = case nt of
      D3.NInt    r -> map (equivFrom r <<< round) <<< N.fromString
      D3.NNumber r -> map (equivFrom r) <<< N.fromString
      D3.NPercent r -> map (equivFrom r <<< D3.Percent <<< (_ / toNumber 100))
                   <<< N.fromString
    showCutoff = case _ of
      After  -> "after"
      Before -> "before"
    showCond = case _ of
      AtLeast _ -> "at least"
      AtMost  _ -> "at most"


condList :: Array (Condition Unit)
condList = [AtLeast unit, AtMost unit]

cutoffList :: Array CutoffType
cutoffList = [After, Before]

dayNumberPickOp :: forall m a b. PickOp m a D3.Days
dayNumberPickOp = PickOp {
      label: "Day Count"
    , component: H.mkComponent {
        initialState: \_ -> After
      , render: \st -> HH.div [HU.classProp "day-count"] [
          HH.span_ [HH.text "Days since/until..."]
        , HH.select [HU.classProp "cutoff-list", HE.onSelectedIndexChange (cutoffList A.!! _)] $
            cutoffList <#> \c ->
              let isSelected = c == st
              in  HH.option [HP.selected isSelected] [HH.text (showCutoff c)]
        ]
      , eval: H.mkEval $ H.defaultEval {
          handleAction = \st -> do
            State.put st
            H.raise PickOpUpdate
        , handleQuery  = case _ of
            PQState f -> do
              c <- State.get
              case f (DayNumber refl c) of
                Tuple x (DayNumber _ d) -> do
                  H.put d
                  pure $ Just x
                _ -> pure Nothing
        }
      }
    }
  where
    showCutoff = case _ of
      After  -> "first day"
      Before -> "last day"

