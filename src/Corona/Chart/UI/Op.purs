
module Corona.Chart.UI.Op where


import Prelude

import Control.Monad.State.Class as State
import Corona.Chart
import D3.Scatter.Type as D3
import Data.Array as A
import Data.Either
import Data.Exists
import Data.Functor.Product
import Data.Int
import Data.Int.Parse
import Data.Maybe
import Data.ModifiedJulianDay (Day)
import Data.Number as N
import Data.Ord
import Data.Symbol (SProxy(..))
import Data.Tuple
import Effect.Class
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS as HC
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Util as HU
import Type.Chain as C
import Type.DProd
import Type.DSum
import Type.Equiv

data PickerQuery a b r = PQState (Operation a b -> Tuple r (Operation a b))

data PickerOutput = PickerUpdate

newtype Picker i m a b = Picker
      { label     :: String
      , component :: H.Component HH.HTML (PickerQuery a b) i PickerOutput m
      }

fakeState
    :: forall s r s m. Applicative m
     => s -> (s -> Tuple r s) -> m (Maybe r)
fakeState x f = pure (Just (fst (f x)))

deltaPicker :: forall i m a. D3.NType a -> Picker i m a a
deltaPicker nt = Picker {
      label: "Daily Change"
    , component: H.mkComponent {
        initialState: \_ -> unit
      , render: \_ -> HH.div [HU.classProp "daily-change"] []
      , eval: H.mkEval $ H.defaultEval {
          handleAction = \_ -> pure unit
        , handleQuery  = case _ of
            PQState f -> fakeState (Delta nt refl) f
        }
      }
    }

growthPicker :: forall i m a. D3.NType a -> Picker i m a D3.Percent
growthPicker nt = Picker {
      label: "Percent Growth"
    , component: H.mkComponent {
        initialState: \_ -> unit
      , render: \_ -> HH.div [HU.classProp "percent-growth"] []
      , eval: H.mkEval $ H.defaultEval {
          handleAction = \_ -> pure unit
        , handleQuery  = case _ of
            PQState f -> fakeState (PGrowth nt refl) f
        }
      }
    }

windowPicker :: forall i m a b. ToFractional a b -> Picker i m a b
windowPicker tf = Picker {
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
          handleAction = State.put
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

-- dayNumberPicker :: forall i m a b. Picker i m a D3.Days
-- dayNumberPicker = Picker {
--       label: "Day Count"
--     , component: H.mkComponent {
--         initialState: \_ -> After
--       , render: \st -> HH.div [HU.classProp "day-count"] [
--           HH.span_ [HH.text "Days since/until..."]
--         , HH.select [HE.onSelectedIndexChange (cutoffList A.!! _)] $
--             cutoffList <#> \c ->
--               let isSelected = c == st
--               in  HH.option [HP.selected isSelected] [HH.text (showCutoff c)]
--         ]
--       , eval: H.mkEval $ H.defaultEval {
--           handleAction = State.put
--         , handleQuery  = case _ of
--             PQState f -> do
--               c <- State.get
--               case f (DayNumber refl c) of
--                 Tuple x (DayNumber _ d) -> do
--                   H.put d
--                   pure $ Just x
--                 _ -> pure Nothing
--         }
--       }
--     }
--   where
--     cutoffList = [After, Before]
--     showCutoff = case _ of
--       After  -> "first day"
--       Before -> "last day"

-- data Operation a b =
--         Delta     (NType a) (a ~ b)       -- ^ dx/dt
--       | PGrowth   (NType a) (b ~ Percent)   -- ^ (dx/dt)/x        -- how to handle percentage
--       | Window    (ToFractional a b) Int -- ^ moving average of x over t, window (2n+1)
--       | CutoffOn  (a ~ b) ProjCond CutoffType
--       | DayNumber (b ~ Days) CutoffType

dayNumberPicker :: forall i m a b. Picker i m a D3.Days
dayNumberPicker = Picker {
      label: "Day Count"
    , component: H.mkComponent {
        initialState: \_ -> After
      , render: \st -> HH.div [HU.classProp "day-count"] [
          HH.span_ [HH.text "Days since/until..."]
        , HH.select [HE.onSelectedIndexChange (cutoffList A.!! _)] $
            cutoffList <#> \c ->
              let isSelected = c == st
              in  HH.option [HP.selected isSelected] [HH.text (showCutoff c)]
        ]
      , eval: H.mkEval $ H.defaultEval {
          handleAction = State.put
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
    cutoffList = [After, Before]
    showCutoff = case _ of
      After  -> "first day"
      Before -> "last day"

