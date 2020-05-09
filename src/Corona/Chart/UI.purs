
module Corona.Chart.UI where

import Prelude

import Corona.Chart
import Corona.JHU
import Halogen.HTML.Core as HH
import D3.Scatter as D3
import D3.Scatter.Type (SType(..), NType(..), Scale(..), NScale(..))
import D3.Scatter.Type as D3
import Data.Array as A
import Data.JSDate (JSDate)
import Data.Either
import Data.Maybe
import Data.ModifiedJulianDay (Day)
import Data.Set (Set)
import Data.Set as S
import Data.Symbol (SProxy(..))
import Effect.Class
import Foreign.Object as O
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Elements as HH
import Halogen.HTML.CSS as HC
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.MultiSelect as MultiSelect
import Halogen.Query.EventSource as ES
import Halogen.Scatter as Scatter
import Type.Chain as C
import Type.DMap as DM
import Type.DProd
import Type.DSum
import Type.Equiv
import Type.Some (Some(..), withSome)
import Type.Some as Some


type AxisState =
    { projection :: DSum SType Projection
    , numScale   :: NScale                   -- ^ date scale is always day
    }

type State =
    { xAxis     :: AxisState
    , yAxis     :: AxisState
    , countries :: Set Country
    }


lookupScale
    :: forall a.
       SType a
    -> NScale
    -> D3.Scale a
lookupScale st ns = case D3.toNType st of
    Left  refl -> D3.Date refl
    Right nt   -> runDProd ns nt

data Action =
        SetCountries (Set Country)
      | SetXBase     (Some BaseProjection)
      | SetYBase     (Some BaseProjection)
      | SetXNumScale NScale
      | SetYNumScale NScale

type ChildSlots =
        ( scatter     :: H.Slot Scatter.Query               Void                         Unit
        , multiselect :: H.Slot (MultiSelect.Query Country) (MultiSelect.Output Country) Unit
        )

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

initialState :: forall i. i -> State
initialState _ = {
      xAxis: {
        projection: dsum D3.sDay $ projection {
          base: Time refl
          , operations: C.nil
          }
      , numScale: DProd D3.Log
      }
    , yAxis: {
        projection: dsum D3.sInt $ projection {
            base: Confirmed refl
          , operations: C.Nil refl
          }
      , numScale: DProd D3.Log
      }
    , countries: initialCountries
    }

classProp :: forall r a. String -> HP.IProp (class :: String | r) a
classProp cl = HP.class_ (HH.ClassName cl)

render :: forall m. MonadEffect m => CoronaData -> State -> H.ComponentHTML Action ChildSlots m
render dat st =
    HH.div [classProp "ui"] [
      HH.div [classProp "plot"] [
        HH.div [classProp "title"] [HH.span_ [HH.text title]]
      , HH.div [classProp "d3"] [
          HH.slot _scatter unit (Scatter.component) unit absurd
        ]
      ]
    , HH.div [classProp "options"] [
        HH.div [classProp "countries"] [
          HH.slot _multi unit (MultiSelect.component) sel0 $ case _ of
            MultiSelect.SelectionChanged c -> Just (SetCountries (S.fromFoldable c))
        ]
      , HH.div [classProp "axes"] [
          axisPicker "X axis" st.xAxis (Some.some bTime) SetXBase SetXNumScale
        , axisPicker "Y axis" st.xAxis (Some.some bTime) SetYBase SetYNumScale
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

axisPicker
    :: forall c m.
       String       -- ^ x or y
    -> AxisState
    -> Some BaseProjection      -- ^ default
    -> (Some BaseProjection -> Action)
    -> (NScale -> Action)
    -> H.ComponentHTML Action c m
axisPicker lab ax b0 raiseBase raiseScale = HH.div [classProp "axis-options"] [
      HH.div [classProp "base-projection"] [
        HH.span_ [HH.text lab]
      , HH.select [HE.onSelectedIndexChange (map raiseBase <<< indexToBase)] $
          allBaseProjections <#> \sbp -> withSome sbp (\bp ->
            HH.option [HP.selected (sbp == b0)] [HH.text (baseProjectionLabel bp)]
          )
      ]
    , HH.div [classProp "axis-scale"] [
        HH.span_ [HH.text (lab <> " scale")]
      , withDSum (ax.projection) (\t _ ->
          case D3.toNType t of
            Left _ -> 
              HH.select_ [ HH.option [HP.selected true] [HH.text "Date"] ]
            Right _ ->
              HH.select [HE.onSelectedIndexChange (map raiseScale <<< indexToNScale)]
              [ HH.option_ [HH.text "Linear"]
              , HH.option  [HP.selected true] [HH.text "Log"]
              ]
        )
      ]
    ]
  where
    indexToBase :: Int -> Maybe (Some BaseProjection)
    indexToBase = case _ of
      0 -> Just (Some.some bTime)
      1 -> Just (Some.some bConfirmed)
      2 -> Just (Some.some bDeaths)
      3 -> Just (Some.some bRecovered)
      _ -> Nothing
    indexToNScale :: Int -> Maybe NScale
    indexToNScale = case _ of
      0 -> Just (DProd Linear)
      1 -> Just (DProd Log)
      _ -> Nothing

handleAction
    :: forall o m. MonadEffect m
     => CoronaData
     -> Action
     -> H.HalogenM State Action ChildSlots o m Unit
handleAction dat = case _ of
    SetCountries cs -> do
      H.modify_ $ \st -> st { countries = cs }
      reRender dat
    SetXBase sb -> do
      H.modify_ $ \st -> st
        { xAxis = st.xAxis
            { projection = withSome sb (flip setBase st.xAxis.projection) }
        }
      reRender dat
    SetYBase sb -> do
      H.modify_ $ \st -> st
        { yAxis = st.yAxis
            { projection = withSome sb (flip setBase st.yAxis.projection) }
        }
      reRender dat
    SetXNumScale s -> do
      H.modify_ $ \st -> st { xAxis = st.xAxis { numScale = s } }
      reRender dat
    SetYNumScale s -> do
      H.modify_ $ \st -> st { yAxis = st.yAxis { numScale = s } }
      reRender dat

setBase :: forall a b r. BaseProjection a -> DSum SType Projection -> DSum SType Projection
setBase base dp = withDSum dp (\tB pr ->
      withProjection pr (\pr ->
        let tC = baseType pr.base
        in  case decide tA tC of
              Just refl -> dsum tB $ projection {
                  base: equivToF refl base
                , operations: pr.operations
                }
              Nothing   -> dsum tA $ projection {
                  base: base
                , operations: C.nil
                }
      )
    )
  where
    tA = baseType base

reRender
    :: forall o m. MonadEffect m
     => CoronaData
     -> H.HalogenM State Action ChildSlots o m Unit
reRender dat = do
    st :: State <- H.get
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

_multi :: SProxy "multiselect"
_multi = SProxy

