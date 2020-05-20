module Corona.Chart.UI where

import Prelude

import Control.Monad.Except
import Control.Monad.Maybe.Trans
import Control.Monad.State.Class as State
import Control.MonadZero as MZ
import Corona.Chart
import Corona.Chart.UI.Projection as Projection
import Corona.JHU
import Corona.Marshal as Marshal
import D3.Scatter.Type (SType(..), NType(..), Scale(..), NScale(..))
import D3.Scatter.Type as D3
import Data.Array as A
import Data.Date as Date
import Data.Either
import Data.Exists
import Data.Functor.Compose
import Data.FunctorWithIndex
import Data.Int
import Data.Lens
import Data.Lens.Record as LR
import Data.Map as Map
import Data.Maybe
import Data.ModifiedJulianDay as MJD
import Data.Set (Set)
import Data.Set as S
import Data.String as String
import Data.String.Pattern as Pattern
import Data.Symbol (SProxy(..))
import Data.Traversable
import Data.Tuple
import Debug.Trace
import Effect
import Effect.Class
import Effect.Class.Console
import Foreign as Foreign
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
import Undefined
import Web.HTML as Web
import Web.HTML.HTMLElement as HTMLElement
import Web.HTML.HTMLInputElement as HTMLInputElement
import Web.HTML.History as History
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
    , permalink :: Maybe String
    , loaded    :: Set (Maybe Axis)     -- nothing: countries
    }

data Axis = XAxis | YAxis | ZAxis | TAxis
derive instance eqAxis :: Eq Axis
derive instance ordAxis :: Ord Axis
instance showAxis :: Show Axis where
    show = case _ of
      XAxis -> "XAxis"
      YAxis -> "YAxis"
      ZAxis -> "ZAxis"
      TAxis -> "TAxis"

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
      | SaveFile
      | Redraw
      | Reset
      | LoadURI
      | CopyURI

type ChildSlots =
        ( scatter     :: H.Slot Scatter.Query    Void              Unit
        , multiselect :: H.Slot (MultiSelect.Query Country) (MultiSelect.Output Country) Unit
        , projection  :: H.Slot Projection.Query Projection.Output Axis
        )


initialCountries :: Set Country
initialCountries = S.fromFoldable [
    "US"
  , "Egypt"
  , "Italy"
  , "Japan"
  , "Russia"
  ]

component :: forall f i o m. MonadEffect m => CoronaData -> H.Component HH.HTML f i o m
component dat =
  H.mkComponent
    { initialState: \_ -> defaultState
    , render: render dat
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction dat
        , initialize   = Just LoadURI
        }
    }

defaultState :: State
defaultState = {
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
        projection: D3.sDays :=> projection {
            base: Confirmed refl
          , operations: Restrict D3.sInt refl After (AtLeast 50)
                   C.:> DayNumber refl After
                   C.:> C.nil
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
    , permalink: Nothing
    , loaded: S.empty
    }

render :: forall m. MonadEffect m => CoronaData -> State -> H.ComponentHTML Action ChildSlots m
render dat st = HH.div [HU.classProp "ui-wrapper"] [
      -- HH.div [HU.classProp "grid__col grid__col--5-of-5 plot-title"] [
      -- ]
      HH.div [HU.classProp "grid__col grid__col--1-of-4 left-column"] [
        HH.div [HU.classProp "dialog"] [
          HH.h3_ [HH.text "Welcome"]
        , HH.div [HU.classProp "sample-copy"] [
            HH.span_ [HH.text "Welcome!"]
          ]
        ]
      ]
    , HH.div [HU.classProp "grid__col grid__col--3-of-4 plot"] [
        -- HH.div [HU.classProp "plot-title"] [
        -- ]
        HH.div [HU.classProp "dialog"] [
          HH.h3_ [HH.text title]
        , HH.div [HU.classProp "plot-buttons"] [
            HH.button [
                HP.type_ HP.ButtonButton
              , HE.onClick (\_ -> Just CopyURI)
              , HU.classProp "copy-uri-button"
              ]
              [ HH.text "Copy Link to Chart" ]
          , HH.button [
                HP.type_ HP.ButtonButton
              , HE.onClick (\_ -> Just SaveFile)
              , HU.classProp "save-image-button"
              ]
              [ HH.text "Save as Image" ]
          , HH.button [
                HP.type_ HP.ButtonButton
              , HE.onClick (\_ -> Just Reset)
              , HU.classProp "reset-button"
              ]
              [ HH.text "Reset projections" ]
          , HH.input [
              HP.type_ HP.InputText
            , HP.value (fromMaybe "" st.permalink)
            , HP.readOnly true
            , HP.ref loaduriRef
            , HU.classProp "loaduri-input"
            , HP.prop (HH.PropName "hidden") true
            ]
          ]
        , HH.slot _scatter unit (Scatter.component hw) unit absurd
        ]
      , HH.div [HU.classProp "countries"] [
          HH.slot _multiselect unit (MultiSelect.component "Countries") sel0 $ case _ of
            MultiSelect.SelectionChanged c -> Just (SetCountries (S.fromFoldable c))
            MultiSelect.MouseOverOut c -> Just (Highlight c)
            MultiSelect.MouseOffOut -> Just Unhighlight
        ]
      ]
    , HH.div [HU.classProp "grid__col grid__col--1-of-4 axis-y"] [
        HH.slot _projection YAxis (Projection.component "Y Axis")
          st.yAxis
          (\(Projection.Update s) -> Just (SetProjection YAxis s))
      ]
    , HH.div [HU.classProp "grid__col grid__col--1-of-4 axis-z"] [
        HH.slot _projection ZAxis (Projection.component "Color Axis")
          st.zAxis
          (\(Projection.Update s) -> Just (SetProjection ZAxis s))
      ]
    , HH.div [HU.classProp "grid__col grid__col--1-of-4 axis-t"] [
        HH.slot _projection TAxis (Projection.component "Time Axis")
          st.tAxis
          (\(Projection.Update s) -> Just (SetProjection TAxis s))
      ]
    , HH.div [HU.classProp "grid__col grid__col--1-of-4 axis-x"] [
        HH.slot _projection XAxis (Projection.component "X Axis")
          st.xAxis
          (\(Projection.Update s) -> Just (SetProjection XAxis s))
      ]
    ]
    -- -- , HH.div [HU.classProp "grid__col grid__col--1-of-5 axis-y"] [
    -- , HH.div [HU.classProp "grid__col grid__col--1-of-5 left-column"] [
      --   HH.div [HU.classProp "save-link"] [

      --   ]
      -- , HH.div [HU.classProp "axis-y"] [
      --     HH.slot _projection YAxis (Projection.component "Y Axis")
      --       st.yAxis
      --       (\(Projection.Update s) -> Just (SetProjection YAxis s))
      --   ]
      -- ]
    -- , HH.div [HU.classProp "grid__col grid__col--1-of-5 axis-z"] [
      --   HH.slot _projection ZAxis (Projection.component "Color Axis")
      --     st.zAxis
      --     (\(Projection.Update s) -> Just (SetProjection ZAxis s))
      -- ]
    -- , HH.div [HU.classProp "grid__col grid__col--1-of-5 axis-t"] [
      --   HH.slot _projection TAxis (Projection.component "Time Axis")
      --     st.tAxis
      --     (\(Projection.Update s) -> Just (SetProjection TAxis s))
      -- ]
    -- , HH.div [HU.classProp "grid__col grid__col--2-of-5 countries"] [
      --   HH.slot _multiselect unit (MultiSelect.component "Countries") sel0 $ case _ of
      --     MultiSelect.SelectionChanged c -> Just (SetCountries (S.fromFoldable c))
      --     MultiSelect.MouseOverOut c -> Just (Highlight c)
      --     MultiSelect.MouseOffOut -> Just Unhighlight
      -- ]
    -- , HH.div [HU.classProp "grid__col grid__col--1-of-5 axis-x"] [
      --   HH.slot _projection XAxis (Projection.component "X Axis")
      --     st.xAxis
      --     (\(Projection.Update s) -> Just (SetProjection XAxis s))
      -- ]
    -- ]
  where
    sel0 :: MultiSelect.State Country
    sel0 =
        { options: opts
        , selected: S.mapMaybe (\c -> A.findIndex (\x -> x.value == c) opts)
                        initialCountries
        , filter: ""
        }
      where
        opts = O.keys dat.counts <#> \cty ->
                  { label: cty, value: cty }
    projLabel dp = withDSum dp (\_ -> projectionLabel)
    title = projLabel (st.yAxis.projection) <> " vs. " <> projLabel (st.xAxis.projection)
    hw = { height: 600.0, width: 1000.0 }

handleAction
    :: forall o m. MonadEffect m
     => CoronaData
     -> Action
     -> H.HalogenM State Action ChildSlots o m Unit
handleAction dat = case _ of
    SetCountries cs   -> H.modify_ (\st -> st { countries = cs })
                      *> reRender dat (Just Nothing)
    SetProjection a p -> do
       axisLens a .= p
       reRender dat (Just (Just a))
    LoadProjection a p -> loadProj a p
    Highlight c -> void $ H.query _scatter unit $ HQ.tell (Scatter.Highlight c)
    Unhighlight -> void $ H.query _scatter unit $ HQ.tell Scatter.Unhighlight
    SaveFile -> void $ H.query _scatter unit $ HQ.tell
        (Scatter.Export "coronavirus-plot.png")
    Redraw -> reRender dat Nothing
    Reset  -> do
      H.modify_ (_ { loaded = S.singleton Nothing :: Set (Maybe Axis) })
      for_ [XAxis, YAxis, ZAxis, TAxis] $ \a -> do
        loadProj a (defaultState ^. axisLens a)
    LoadURI -> loadUri
    CopyURI -> void <<< runMaybeT $ do
         e  <- MaybeT $ H.getHTMLElementRef loaduriRef
         ie <- maybe MZ.empty pure $ HTMLInputElement.fromHTMLElement e
         liftEffect $ do
            HTMLElement.setHidden false e
            HTMLInputElement.select ie
            HTMLInputElement.setSelectionRange 0 999999 "none" ie
            execCopy
            HTMLElement.setHidden true e
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
        let proj = case res of
              Left e  -> defaultState ^. axisLens a
              Right p -> p
        loadProj a proj
      ctyRes <- liftEffect $ parseAtKey usp countrySpec.param parseSet
      let ctys = case ctyRes of
            Left  e  -> defaultState.countries
            Right cs -> cs
      loadCountries ctys
    loadCountries ctys = do
        res <- H.query _multiselect unit $
          MultiSelect.SetState (\x -> { new: setter x, next: unit })
        when (isNothing res) $
          warn "warning: country load did not return response"
      where
        setter ms = ms
          { selected = S.fromFoldable <<< A.catMaybes $
                mapWithIndex (\i x ->
                    if x.value `S.member` ctys
                      then Just i
                      else Nothing
                  ) ms.options
          }

type URISpec =
        { default :: String
        , write   :: State -> String
        , param   :: String
        , include :: Boolean
        }

uriSpecs :: Array URISpec
uriSpecs = A.snoc axisSpec countrySpec
  where
    axisSpec = [XAxis, YAxis, ZAxis, TAxis] <#> \a -> do
      { default: Projection.stateSerialize $ defaultState ^. axisLens a
      , write: \st -> Projection.stateSerialize $ st ^. axisLens a
      , param: axisParam a
      , include: true
      }

countrySpec :: URISpec
countrySpec =
  { default: serializeSet defaultState.countries
  , write: \st -> serializeSet $ st.countries
  , param: "r"
  , include: false
  }

generateUri
    :: forall o m. MonadEffect m
    => H.HalogenM State Action ChildSlots o m
            { historyPush :: String, permalink :: String }
generateUri = do
    st <- H.get
    liftEffect $ do
      uspHistPush <- USP.new ""
      uspPerma <- USP.new ""
      for_ uriSpecs $ \spec -> do
        let toWrite = spec.write st
        USP.set uspPerma spec.param toWrite
        unless (toWrite == spec.default || not spec.include) $
          USP.set uspHistPush spec.param toWrite
      srchHistPush   <- USP.toString uspHistPush
      srchPerma <- USP.toString uspPerma
      bn <- fullPagename
      let genStr str
            | String.null str = ""
            | otherwise       = "?" <> str
      pure { historyPush: bn <> genStr srchHistPush
           , permalink: bn <> genStr srchPerma
           }


reRender
    :: forall o m. MonadEffect m
     => CoronaData
     -> Maybe (Maybe Axis)
     -> H.HalogenM State Action ChildSlots o m Unit
reRender dat initter = do
    loaded <- for initter $ \a -> do
      State.state $ \st ->
        let newLoaded = S.insert a st.loaded
        in  Tuple (S.size newLoaded >= 5) (st { loaded = newLoaded })
    when (and loaded) $ do
      st :: State <- H.get
      -- traceM (show st)
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
      uri <- generateUri
      H.modify_ (_ { permalink = Just uri.permalink })
      liftEffect $ do
        hist <- liftEffect $ Window.history =<< Web.window
        History.pushState
          (Foreign.unsafeToForeign uri.historyPush)
          (History.DocumentTitle "Coronavirus Data Tracker")
          (History.URL uri.historyPush)
          hist


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

serializeSet
    :: Set String
    -> String
serializeSet = String.joinWith "|" <<< A.fromFoldable

parseSet :: P.Parser (Set String)
parseSet = S.fromFoldable <<< String.split (Pattern.Pattern "|")
       <$> Marshal.parse

_scatter :: SProxy "scatter"
_scatter = SProxy

_multiselect :: SProxy "multiselect"
_multiselect = SProxy

_projection :: SProxy "projection"
_projection = SProxy

loaduriRef ∷ H.RefLabel
loaduriRef = H.RefLabel "loaduri-input"

foreign import fullPagename :: Effect String
foreign import execCopy :: Effect Unit
