
module Corona.Chart.UI where

import Prelude

import Corona.Chart
import Corona.Chart.UI.NumericOp as NumericOp
import Corona.JHU
import D3.Scatter.Type (SType(..), NType(..), Scale(..), NScale(..))
import D3.Scatter.Type as D3
import Data.Array as A
import Data.Either
import Data.Exists
import Data.Functor.Compose
import Data.Maybe
import Data.Set (Set)
import Corona.Chart.UI.Projection as Projection
import Data.Set as S
import Data.Symbol (SProxy(..))
import Debug.Trace
import Effect.Class
import Effect.Class.Console
import Foreign.Object as O
import Halogen as H
import Halogen.ChainPicker as ChainPicker
import Halogen.HTML as HH
import Halogen.HTML.Core as HH
import Halogen.HTML.Elements as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.MultiSelect as MultiSelect
import Halogen.Scatter as Scatter
import Halogen.Util as HU
import Type.Chain as C
import Type.DProd
import Type.DSum
import Type.Equiv
import Type.GCompare


type AxisState =
    { projection :: DSum SType Projection
    , numScale   :: NScale                   -- ^ date scale is always day
    }

type State =
    { xAxis     :: Projection.State
    , yAxis     :: Projection.State
    , countries :: Set Country
    }

data Axis = XAxis | YAxis
derive instance eqAxis :: Eq Axis
derive instance ordAxis :: Ord Axis

lookupScale
    :: forall a.
       SType a
    -> NScale
    -> D3.Scale a
lookupScale st ns = case D3.toNType st of
    Left (Left  r) -> D3.Date r
    Left (Right r) -> D3.Linear (Left r)
    Right nt       -> D3.runNScale ns nt

data Action =
        SetCountries (Set Country)
      | SetXProjection Projection.State
      | SetYProjection Projection.State

type ChildSlots =
        ( scatter     :: H.Slot Scatter.Query    Void              Unit
        , multiselect :: H.Slot (MultiSelect.Query Country) (MultiSelect.Output Country) Unit
        , xProjection :: H.Slot Projection.Query Projection.Output Unit
        , yProjection :: H.Slot Projection.Query Projection.Output Unit
        )


initialCountries :: Set Country
-- initialCountries = S.fromFoldable ["US"]
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

initialState :: forall i. i -> State
initialState _ = {
      xAxis: {
        projection: dsum D3.sDay $ projection {
            base: Time refl
          , operations: C.nil
          }
        -- projection: dsum D3.sDays $ projection {
        --   base: Time refl
        --   , operations: C.cons (DaysSince refl refl testConf (AtLeast (toNumber 100)))
        --       C.nil
        --   }
      , numScale: NScale (DProd D3.Log)
      }
    , yAxis: {
        projection: dsum D3.sInt $ projection {
            base: Confirmed refl
          , operations: C.Nil refl
          }
      , numScale: NScale (DProd D3.Log)
      }
    , countries: initialCountries
    }
  where
    testConf = dsum D3.nInt $ projection
      { base: Confirmed refl
      , operations: C.nil
      }

render :: forall m. MonadEffect m => CoronaData -> State -> H.ComponentHTML Action ChildSlots m
render dat st = HH.main_ [
      HH.h1_ [HH.text "COVID-19 Data"]
    , HH.div [HU.classProp "ui"] [
        HH.div [HU.classProp "plot"] [
          HH.h2_ [HH.text title]
        , HH.div [HU.classProp "d3"] [
            HH.slot _scatter unit (Scatter.component) unit absurd
          ]
        ]
      , HH.div [HU.classProp "options"] [
          HH.div [HU.classProp "countries"] [
            HH.slot _multiselect unit (MultiSelect.component) sel0 $ case _ of
              MultiSelect.SelectionChanged c -> Just (SetCountries (S.fromFoldable c))
          ]
        , HH.div [HU.classProp "axes"] [
            HH.slot _xProjection unit (Projection.component "X Axis")
              st.xAxis
              (\(Projection.Update s) -> Just (SetXProjection s))
          , HH.slot _yProjection unit (Projection.component "Y Axis")
              st.yAxis
              (\(Projection.Update s) -> Just (SetYProjection s))
          ]
        ]
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
        opts = O.keys dat.counts <#> \cty ->
                  { label: cty, value: cty }
    projLabel dp = withDSum dp (\_ -> projectionLabel)
    title = projLabel (st.yAxis.projection) <> " vs. " <> projLabel (st.xAxis.projection)

handleAction
    :: forall o m. MonadEffect m
     => CoronaData
     -> Action
     -> H.HalogenM State Action ChildSlots o m Unit
handleAction dat act = do
    case act of
      SetCountries cs  -> H.modify_ $ \st -> st { countries = cs }
      SetXProjection p -> H.modify_ $ \st -> st { xAxis     = p  }
      SetYProjection p -> H.modify_ $ \st -> st { yAxis     = p  }
    reRender dat

reRender
    :: forall o m. MonadEffect m
     => CoronaData
     -> H.HalogenM State Action ChildSlots o m Unit
reRender dat = do
    st :: State <- H.get
    traceM (show st)
    withDSum st.xAxis.projection (\tX pX ->
      withDSum st.yAxis.projection (\tY pY -> void $
        H.query _scatter unit $ Scatter.Query
          { update: \f -> f tX tY (
                toScatterPlot
                  dat
                  pX
                  (lookupScale tX (st.xAxis.numScale))
                  pY
                  (lookupScale tY (st.yAxis.numScale))
                  st.countries
              )
          , next: unit
          }
      )
    )

_scatter :: SProxy "scatter"
_scatter = SProxy

_multiselect :: SProxy "multiselect"
_multiselect = SProxy

_xProjection :: SProxy "xProjection"
_xProjection = SProxy

_yProjection :: SProxy "yProjection"
_yProjection = SProxy
