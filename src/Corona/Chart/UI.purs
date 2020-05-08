
module Corona.Chart.UI where

import Prelude

import Corona.Chart
import Corona.JHU
import Data.Array as A
import D3.Scatter as D3
import Data.JSDate (JSDate)
import Data.Maybe
import Data.Set (Set)
import Data.Set as S
import Data.Symbol (SProxy(..))
import Effect.Class
import Foreign.Object as O
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS as HC
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.MultiSelect as MultiSelect
import Halogen.Query.EventSource as ES
import Halogen.Scatter as Scatter
import Type.Chain as C
import Type.Equality
import Type.Equiv


type AxisState a =
    { projection :: Projection a
    , scale      :: D3.Scale a
    }

type State a b =
    { xAxis     :: AxisState a
    , yAxis     :: AxisState b
    , countries :: Set Country
    }

data Action =
        SetCountries (Set Country)

type TempState = State JSDate Number

type ChildSlots =
        ( scatter     :: H.Slot Scatter.Query               Void                         Unit
        , multiselect :: H.Slot (MultiSelect.Query Country) (MultiSelect.Output Country) Unit
        )

-- type SomeState = forall r. (forall a b. State a b -> r) -> r

-- data Action =
--         AddCountries (Set Country)
--       | RemoveCountry Country
--       | RemoveAllCountries

initialCountries :: Set Country
initialCountries = S.fromFoldable ["US", "Egypt", "Italy"]

component :: forall f i o m. MonadEffect m => CoronaData -> H.Component HH.HTML f i o m
component dat =
  H.mkComponent
    { initialState
    , render: render dat
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction dat
        , initialize   = Just (SetCountries initialCountries)
        }
    }

initialState :: forall i. i -> TempState
initialState _ = {
      xAxis: {
        projection: \f -> f { base: Time refl
                            , operations: C.Nil refl
                            }
      , scale: D3.sDate
      }
    , yAxis: {
        projection: \f -> f { base: Confirmed refl
                            , operations: C.Nil refl
                            }
      , scale: D3.sLog
      }
  , countries: initialCountries
    }

render :: forall m. MonadEffect m => CoronaData -> TempState -> H.ComponentHTML Action ChildSlots m
render dat st =
    HH.div_ [
      HH.div_ [
        HH.slot _scatter unit (Scatter.component) unit absurd
      ]
    , HH.div_ [
        HH.slot _multi unit (MultiSelect.component) sel0 $ case _ of
          MultiSelect.SelectionChanged c -> Just (SetCountries (S.fromFoldable c))
          _ -> Nothing
      ]
    ]
  where
    sel0 :: MultiSelect.State Country
    sel0 = 
        { options: opts
        , selected: S.mapMaybe (\c -> A.findIndex (\x -> x.value == c) opts) initialCountries
        , filter: ""
        }
      where
        opts = map (\cty -> { label: cty, value: cty }) (O.keys dat.counts)

handleAction :: forall a o m. MonadEffect m => CoronaData -> Action -> H.HalogenM TempState Action ChildSlots o m Unit
handleAction dat = case _ of
    SetCountries cs -> do
      H.modify_ $ \s -> s { countries = cs }
      st :: TempState <- H.get
      _ <- H.query _scatter unit $ Scatter.Query
        { update: \f -> f (
              toScatterPlot
                dat
                st.xAxis.projection
                st.xAxis.scale
                st.yAxis.projection
                st.yAxis.scale
                st.countries
            )
        , next: unit
        }
      pure unit

_scatter :: SProxy "scatter"
_scatter = SProxy

_multi :: SProxy "multiselect"
_multi = SProxy

