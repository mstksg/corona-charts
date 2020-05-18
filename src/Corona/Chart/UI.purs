module Corona.Chart.UI where

import Prelude

import Control.Monad.Except
import Corona.Chart
import Corona.Chart.UI.Projection as Projection
import Corona.JHU
import Corona.Marshal as Marshal
import D3.Scatter.Type (SType(..), NType(..), Scale(..), NScale(..))
import D3.Scatter.Type as D3
import Data.Array as A
import Data.Either
import Data.Exists
import Data.Functor.Compose
import Data.Int
import Data.Lens
import Data.Lens.Record as LR
import Data.Maybe
import Data.Set (Set)
import Data.Set as S
import Data.Symbol (SProxy(..))
import Data.Traversable
import Debug.Trace
import Effect
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
import Halogen.Query as HQ
import Halogen.Scatter as Scatter
import Halogen.Util as HU
import Text.Parsing.StringParser as P
import Type.Ap
import Type.Chain as C
import Type.DProd
import Type.DSum
import Type.Equiv
import Type.GCompare
import Web.HTML as Web
import Web.HTML.Location as Location
import Web.HTML.Window as Window
import Web.URLSearchParams as USP


type AxisState =
    { projection :: DSum SType Projection
    , numScale   :: NScale                   -- ^ date scale is always day
    }

type State =
    { xAxis     :: Projection.State
    , yAxis     :: Projection.State
    , zAxis     :: Projection.State
    , tAxis     :: Projection.State
    , countries :: Set Country
    }

data Axis = XAxis | YAxis | ZAxis | TAxis
derive instance eqAxis :: Eq Axis
derive instance ordAxis :: Ord Axis

axisLens :: Axis -> Lens' State Projection.State
axisLens = case _ of
    XAxis -> LR.prop (SProxy :: SProxy "xAxis")
    YAxis -> LR.prop (SProxy :: SProxy "yAxis")
    ZAxis -> LR.prop (SProxy :: SProxy "zAxis")
    TAxis -> LR.prop (SProxy :: SProxy "tAxis")

axisParam :: Axis -> String
axisParam = case _ of
    XAxis -> "x"
    YAxis -> "y"
    ZAxis -> "z"
    TAxis -> "t"


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
      | SetProjection Axis Projection.State
      | LoadProjection Axis Projection.State
      | Highlight Country
      | Unhighlight
      | Redraw
      | LoadURI
      | CopyURI

type ChildSlots =
        ( scatter     :: H.Slot Scatter.Query    Void              Unit
        , multiselect :: H.Slot (MultiSelect.Query Country) (MultiSelect.Output Country) Unit
        , projection  :: H.Slot Projection.Query Projection.Output Axis
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
        , initialize   = Just LoadURI
        }
    }

initialState :: forall i. i -> State
initialState _ = {
      xAxis: {
        projection: D3.sDay :=> projection {
            base: Time refl
          , operations: C.nil
          }
      , numScale: NScale (DProd D3.Log)
      }
    , yAxis: {
        projection: D3.sInt :=> projection {
            base: Confirmed refl
          , operations: C.Nil refl
          }
      , numScale: NScale (DProd D3.Log)
      }
    , zAxis: {
        projection: D3.sDay :=> projection {
            base: Time refl
          , operations: C.nil
          }
      , numScale: NScale (DProd D3.Log)
      }
    , tAxis: {
        projection: D3.sDay :=> projection {
            base: Time refl
          , operations: C.nil
          }
      , numScale: NScale (DProd D3.Log)
      }
    , countries: initialCountries
    }
  where
    testConf = D3.nInt :=> projection
      { base: Confirmed refl
      , operations: C.nil
      }

render :: forall m. MonadEffect m => CoronaData -> State -> H.ComponentHTML Action ChildSlots m
render dat st = HH.div [HU.classProp "ui-wrapper"] [
      HH.div [HU.classProp "grid__col grid__col--5-of-5 plot-title"] [
        HH.h2_ [HH.text title]
      , HH.a [HE.onClick (\_ -> Just CopyURI)]
            [ HH.text "test" ]
      ]
    , HH.div [HU.classProp "grid__col grid__col--1-of-5 axis-y"] [
        HH.slot _projection YAxis (Projection.component "Y Axis")
          st.yAxis
          (\(Projection.Update s) -> Just (SetProjection YAxis s))
      ]
    , HH.div [HU.classProp "grid__col grid__col--4-of-5 plot"] [
        HH.slot _scatter unit (Scatter.component hw) unit absurd
      ]
    , HH.div [HU.classProp "grid__col grid__col--1-of-5 axis-z"] [
        HH.slot _projection ZAxis (Projection.component "Color Axis")
          st.zAxis
          (\(Projection.Update s) -> Just (SetProjection ZAxis s))
      ]
    , HH.div [HU.classProp "grid__col grid__col--1-of-5 axis-t"] [
        HH.slot _projection TAxis (Projection.component "Time Axis")
          st.tAxis
          (\(Projection.Update s) -> Just (SetProjection TAxis s))
      ]
    , HH.div [HU.classProp "grid__col grid__col--2-of-5 countries"] [
        HH.slot _multiselect unit (MultiSelect.component "Countries") sel0 $ case _ of
          MultiSelect.SelectionChanged c -> Just (SetCountries (S.fromFoldable c))
          MultiSelect.MouseOverOut c -> Just (Highlight c)
          MultiSelect.MouseOffOut -> Just Unhighlight
      ]
    , HH.div [HU.classProp "grid__col grid__col--1-of-5 axis-x"] [
        HH.slot _projection XAxis (Projection.component "X Axis")
          st.xAxis
          (\(Projection.Update s) -> Just (SetProjection XAxis s))
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
    hw = { height: 700.0, width: 1000.0 }
    testP =
        { projection: D3.sInt :=> projection
            { base: bConfirmed
            , operations: C.cons (Delta D3.nInt refl) C.nil
            }
      , numScale: NScale (DProd (D3.Linear <<< Right))
      }

handleAction
    :: forall o m. MonadEffect m
     => CoronaData
     -> Action
     -> H.HalogenM State Action ChildSlots o m Unit
handleAction dat = case _ of
    SetCountries cs   -> H.modify_ (\st -> st { countries = cs }) *> reRender dat
    SetProjection a p -> (axisLens a .= p) *> reRender dat
    LoadProjection a p -> loadProj a p
    Highlight c -> void $ H.query _scatter unit $ HQ.tell (Scatter.Highlight c)
    Unhighlight -> void $ H.query _scatter unit $ HQ.tell Scatter.Unhighlight
    Redraw -> reRender dat
    LoadURI -> loadUri *> reRender dat
    CopyURI -> do
      st <- H.get
      liftEffect $ do
        usp <- USP.new ""
        for_ [XAxis, YAxis, ZAxis, TAxis] $ \a ->
          USP.set usp  (axisParam a) $ Projection.stateSerialize $ view (axisLens a) st
        log =<< USP.toString usp
  where
    loadProj a p = do
      res <- H.query _projection a $
        HQ.tell (Projection.QueryPut p)
      when (isNothing res) $
        warn "warning: projection load did not return response"
    loadUri = do
      usp <- liftEffect $ USP.new
                      =<< Location.search
                      =<< Window.location
                      =<< Web.window
      for_ [XAxis, YAxis, ZAxis, TAxis] $ \a -> do
        res <- liftEffect $ parseAtKey usp (axisParam a) Projection.stateParser
        case res of
          Left e  -> error $ "loadURI error on " <> axisParam a <> ": " <> e
          Right p -> loadProj a p

reRender
    :: forall o m. MonadEffect m
     => CoronaData
     -> H.HalogenM State Action ChildSlots o m Unit
reRender dat = do
    st :: State <- H.get
    traceM (show st)
    withDSum st.xAxis.projection (\tX pX ->
      withDSum st.yAxis.projection (\tY pY ->
        withDSum st.zAxis.projection (\tZ pZ ->
          withDSum st.tAxis.projection (\tT pT -> void $
            H.query _scatter unit $ HQ.tell $ Scatter.Update
              (\f -> f tX tY tZ tT (
                    toScatterPlot
                      dat
                      pX
                      (lookupScale tX (st.xAxis.numScale))
                      pY
                      (lookupScale tY (st.yAxis.numScale))
                      pZ
                      (lookupScale tZ (st.zAxis.numScale))
                      pT
                      (lookupScale tT (st.tAxis.numScale))
                      st.countries
                  )
              )
          )
        )
      )
    )

parseAtKey
    :: forall a.
       USP.URLSearchParams
    -> USP.Key
    -> P.Parser a
    -> Effect (Either String a)
parseAtKey u k p = runExceptT $ do
    val <- maybe (throwError ("parameter not found: " <> k)) pure
                =<< lift (USP.get u k)
    either (throwError <<< show) pure $ P.runParser p val



_scatter :: SProxy "scatter"
_scatter = SProxy

_multiselect :: SProxy "multiselect"
_multiselect = SProxy

_projection :: SProxy "projection"
_projection = SProxy
