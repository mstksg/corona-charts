
module Corona.Chart.UI where

import Prelude

import Corona.Chart
import Corona.JHU
import Corona.UI.NumericOp as NumericOp
import D3.Scatter as D3
import D3.Scatter.Type (SType(..), NType(..), Scale(..), NScale(..))
import D3.Scatter.Type as D3
import Data.Array as A
import Data.Either
import Data.Exists
import Data.Functor.Compose
import Data.JSDate (JSDate)
import Data.Maybe
import Data.ModifiedJulianDay (Day)
import Data.Set (Set)
import Data.Set as S
import Data.Symbol (SProxy(..))
import Effect.Class
import Effect.Class.Console
import Foreign.Object as O
import Halogen as H
import Halogen.ChainPicker as ChainPicker
import Halogen.HTML as HH
import Halogen.HTML.CSS as HC
import Halogen.HTML.Core as HH
import Halogen.HTML.Elements as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.MultiSelect as MultiSelect
import Halogen.Query.EventSource as ES
import Halogen.Scatter as Scatter
import Halogen.Util as HU
import Type.Chain as C
import Type.DMap as DM
import Type.DProd
import Type.DSum
import Type.Equiv
import Type.GCompare


type AxisState =
    { projection :: DSum SType Projection
    , numScale   :: NScale                   -- ^ date scale is always day
    }

type State =
    { xAxis     :: AxisState
    , yAxis     :: AxisState
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
    Left  refl -> D3.Date refl
    Right nt   -> runDProd ns nt

data Action =
        SetCountries (Set Country)
      | SetBase      Axis (Exists BaseProjection)
      | SetOps       Axis (Exists SType)
      | SetNumScale  Axis NScale

type OpIx = { axis :: Axis, tagIn :: WrEx D3.SType }

type ChildSlots =
        ( scatter     :: H.Slot Scatter.Query               Void                         Unit
        , multiselect :: H.Slot (MultiSelect.Query Country) (MultiSelect.Output Country) Unit
        , opselect    :: H.Slot (ChainPicker.WrappedQuery Operation SType)
                            (ChainPicker.Output SType)
                            OpIx
        )

-- data Query f tag a r =

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
render dat st = HH.main_ [
      HH.h1_ [HH.text "COVID-19 Data"]
    , HH.div [classProp "ui"] [
        HH.div [classProp "plot"] [
          HH.h2_ [HH.text title]
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
            axisPicker XAxis st.xAxis (mkExists bTime)
          , axisPicker YAxis st.yAxis (mkExists bConfirmed)
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

axisPicker
    :: forall m.
       Axis
    -> AxisState
    -> Exists BaseProjection      -- ^ default
    -> H.ComponentHTML Action ChildSlots m
axisPicker axis aState b0 = HH.div [classProp "axis-options"] [
      HH.div [classProp "base-projection"] [
        HH.span_ [HH.text label]
      , HH.select [HE.onSelectedIndexChange (map (SetBase axis) <<< indexToBase)] $
          allBaseProjections <#> \sbp -> runExists (\bp ->
            HH.option [HP.selected (WrEx sbp == WrEx b0)] [HH.text (baseProjectionLabel bp)]
          ) sbp
      ]
    , HH.div [classProp "axis-op-chain"] [
        HH.span_ [HH.text (label <> " operations")]
      , withDSum aState.projection (\_ spr ->
          withProjection spr (\pr ->
            let tBase = baseType pr.base
            in  HH.slot _opselect
                  {axis: axis, tagIn: mkWrEx tBase}
                  (chainPicker tBase)
                  unit
                  $ \(ChainPicker.ChainUpdate u) ->
                      Just (SetOps axis u)

          )
        )
      ]
    , HH.div [classProp "axis-scale"] [
        HH.span_ [HH.text (label <> " scale")]
      , withDSum aState.projection (\t _ ->
          case D3.toNType t of
            Left _ -> 
              HH.select_ [ HH.option [HP.selected true] [HH.text "Date"] ]
            Right _ ->
              HH.select [HE.onSelectedIndexChange (map (SetNumScale axis) <<< indexToNScale)]
              [ HH.option_ [HH.text "Linear"]
              , HH.option  [HP.selected true] [HH.text "Log"]
              ]
        )
      ]
    ]
  where
    indexToBase :: Int -> Maybe (Exists BaseProjection)
    indexToBase = case _ of
      0 -> Just (mkExists bTime)
      1 -> Just (mkExists bConfirmed)
      2 -> Just (mkExists bDeaths)
      3 -> Just (mkExists bRecovered)
      _ -> Nothing
    indexToNScale :: Int -> Maybe NScale
    indexToNScale = case _ of
      0 -> Just (DProd Linear)
      1 -> Just (DProd Log)
      _ -> Nothing
    label :: String
    label = case axis of
      XAxis -> "X axis"
      YAxis -> "Y axis"

chainPicker
    :: forall a i m.
       SType a  -- ^ initial initial type
    -> H.Component HH.HTML (ChainPicker.WrappedQuery Operation SType) i (ChainPicker.Output SType) m
chainPicker = ChainPicker.wrappedComponent $ DProd (\t -> Compose $
      case D3.toNType t of
        Left  _  -> Nothing
        Right nt -> Just $ ChainPicker.Picker
          { component: HU.trimapComponent
                  (\(ChainPicker.SQ f) -> NumericOp.QueryOp f)
                  identity
                  (\(NumericOp.ChangeEvent t) -> t)
                  (NumericOp.component nt)
          , initialOut: mkExists t
          }
    )

handleAction
    :: forall o m. MonadEffect m
     => CoronaData
     -> Action
     -> H.HalogenM State Action ChildSlots o m Unit
handleAction dat act = do
    case act of
      SetCountries cs -> H.modify_ $ \st -> st { countries = cs }
      SetBase ax sb -> H.modify_ $ \st ->
        case ax of
          XAxis -> st
            { xAxis = st.xAxis
                { projection = runExists (flip setBase st.xAxis.projection) sb }
            }
          YAxis -> st
            { yAxis = st.yAxis
                { projection = runExists (flip setBase st.yAxis.projection) sb }
            }
      SetOps ax _ -> do
        dsp <- case ax of
          XAxis -> H.gets (_.xAxis.projection)
          YAxis -> H.gets (_.yAxis.projection)
        withDSum dsp (\tOut proj -> withProjection proj (\pr -> do
            let tIn = baseType pr.base
            qres <- H.query _opselect { axis: ax, tagIn: mkWrEx tIn }
                  $ ChainPicker.someQuery tIn (ChainPicker.AskSelected identity)
            case qres of
              Nothing       -> log "no response from component"
              Just (Left e) -> log $ "type mismatch: " <> runExists show e
              Just (Right dsc) -> withDSum dsc (\tNewOut chain ->
                H.modify_ $ \st ->
                  case ax of
                    XAxis -> st
                      { xAxis = st.xAxis
                          { projection = dsum tNewOut $ projection
                              { base: pr.base, operations: chain }
                          }
                      }
                    YAxis -> st
                      { yAxis = st.yAxis
                          { projection = dsum tNewOut $ projection
                              { base: pr.base, operations: chain }
                          }
                      }
              )
        )
      )
      SetNumScale ax s -> H.modify_ $ \st ->
        case ax of
          XAxis -> st { xAxis = st.xAxis { numScale = s } }
          YAxis -> st { yAxis = st.yAxis { numScale = s } }
    reRender dat

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

_opselect :: SProxy "opselect"
_opselect = SProxy

