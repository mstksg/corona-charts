module Corona.Chart.UI where

import Prelude

import Control.Monad.Except
import Control.Monad.Maybe.Trans
import Data.Const
import Control.Monad.State
import Control.Monad.State.Class as State
import Control.Monad.Writer
import Control.MonadZero as MZ
import Corona.Chart
import Corona.Chart.UI.Projection as Projection
import Corona.JHU
import Corona.Marshal as Marshal
import D3.Scatter.Type (SType(..), NType(..), Scale(..), NScale(..))
import D3.Scatter.Type as D3
import Data.Array as A
import Data.Bifunctor
import Data.Date as Date
import Data.Either
import Data.Exists
import Data.Foldable
import Data.Function.Uncurried
import Data.Functor.Compose
import Data.FunctorWithIndex
import Data.Identity as Identity
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
import Effect.Aff
import Effect.Aff.Class
import Effect.Class
import Effect.Class.Console
import Foreign as Foreign
import Foreign.Object as O
import Halogen as H
import Halogen.Aff.Util as HU
import Halogen.Autocomplete as Autocomplete
import Halogen.ChainPicker as ChainPicker
import Halogen.Component.RawHTML as RawHTML
import Halogen.HTML as HH
import Halogen.HTML.Core as HH
import Halogen.HTML.Elements as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.MultiSelect as MultiSelect
import Halogen.Query as HQ
import Halogen.Query.EventSource as ES
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
import Web.DOM.Element as Element
import Web.DOM.Node as Node
import Web.DOM.ParentNode as DOM
import Web.HTML as Web
import Web.HTML.HTMLElement as HTMLElement
import Web.HTML.HTMLInputElement as HTMLInputElement
import Web.HTML.History as History
import Web.HTML.Location as Location
import Web.HTML.Window as Window
import Web.UIEvent.MouseEvent as ME
import Web.URLSearchParams as USP


type AxisState =
    { projection :: DSum SType Projection
    , numScale   :: NScale                   -- ^ date scale is always day
    }

type State =
    { xAxis        :: Projection.State
    , yAxis        :: Projection.State
    , zAxis        :: Projection.State
    , tAxis        :: Projection.State
    , countries    :: Set Country
    , unselected   :: Set Country
    , allCountries :: Set Country
    , permalink    :: Maybe String
    , waiting      :: Set (Maybe Axis)     -- ^ only render when empty. nothing: countries
    , welcomeText  :: Maybe String
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

axisLens :: Axis -> Lens' (Record _) Projection.State
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
      | AddCountry String
      | RemoveCountry String
      | ClearCountries
      | DumpCountries
      | SetProjection Axis Projection.State
      | LoadProjection Axis Projection.State
      | Highlight Country
      | Unhighlight
      | SaveFile
      | Redraw
      | Reset
      | CopyURI
      | LoadURIString String
      | Initialize
      | Linkify

type ChildSlots =
        ( scatter     :: H.Slot Scatter.Query    Void              Unit
        -- , multiselect :: H.Slot (MultiSelect.Query Country) (MultiSelect.Output Country) Unit
        , autocomplete :: H.Slot Autocomplete.Query Autocomplete.Output Unit
        , projection  :: H.Slot Projection.Query Projection.Output Axis
        , postRender  :: H.Slot (Const Void) Unit Unit     -- hm we could integrate this
        , welcomeFrame :: H.Slot (Const Void) Void Unit    -- with this
        )


initialCountries :: Set Country
initialCountries = S.fromFoldable [
    "United States"
  , "Egypt"
  , "Italy"
  , "South Korea"
  , "Russia"
  ]

component :: forall f i o m. MonadAff m => CoronaData -> H.Component HH.HTML f i o m
component dat =
  H.mkComponent
    { initialState: \_ ->
        { xAxis: defaultProjections.xAxis
        , yAxis: defaultProjections.yAxis
        , zAxis: defaultProjections.zAxis
        , tAxis: defaultProjections.tAxis
        , countries: initialCountries
        , unselected: allCountries `S.difference` initialCountries
        , allCountries
        , permalink: Nothing
        , waiting: S.empty
        , welcomeText: Nothing
        }
    , render: render dat
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction dat
        , initialize   = Just Initialize
        }
    }
  where
    allCountries = S.fromFoldable (O.keys dat.counts)

defaultProjections ::
    { xAxis     :: Projection.State
    , yAxis     :: Projection.State
    , zAxis     :: Projection.State
    , tAxis     :: Projection.State
    }
defaultProjections = {
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

  }

render :: forall m. MonadAff m => CoronaData -> State -> H.ComponentHTML Action ChildSlots m
render dat st = HH.div [HU.classProp "ui-wrapper"] [
      -- HH.div [HU.classProp "grid__col grid__col--5-of-5 plot-title"] [
      -- ]
      HH.div [HU.classProp "grid__col grid__col--1-of-5 left-column"] [
        HH.h2_ [HH.text "Coronavirus Data Plotter"]
      , HH.slot _projection YAxis (Projection.component "Y Axis")
          st.yAxis
          (\(Projection.Update s) -> Just (SetProjection YAxis s))
      ]
    , HH.div [HU.classProp "grid__col grid__col--4-of-5 plot"] [
        -- HH.div [HU.classProp "plot-title"] [
        -- ]
        HH.div [HU.classProp "dialog"] [
          HH.h3_ [HH.text title]
        , HH.div [HU.classProp "plot-buttons"] [
            HH.button [
                HP.type_ HP.ButtonButton
              , HE.onClick (\_ -> Just CopyURI)
              , HU.classProp "copy-uri-button"
              , HP.title "Copy Permalink to Chart"
              ]
              [ HH.text "Copy Link" ]
          , HH.button [
                HP.type_ HP.ButtonButton
              , HE.onClick (\_ -> Just SaveFile)
              , HU.classProp "save-image-button"
              , HP.title "Save Chart as Image"
              ]
              [ HH.text "Save Image" ]
          , HH.button [
                HP.type_ HP.ButtonButton
              , HE.onClick (\_ -> Just Reset)
              , HU.classProp "reset-button"
              , HP.title "Reset Projections to Default"
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
        , HH.div [HU.classProp "country-picker"] [
            HH.div [HU.classProp "grid__col grid__col--2-of-8 country-picker-input"] [
              HH.slot _autocomplete unit
                (Autocomplete.component "country-select" "Search for a country...")
                (A.fromFoldable st.unselected)
                (case _ of Autocomplete.Selected str -> Just (AddCountry str))
            ]
          , HH.div [HU.classProp "grid__col grid__col--4-of-8 country-picker-select"]
              [ HH.ul [HU.classProp "picked-countries"] $
                  A.fromFoldable st.countries <#> \c ->
                    HH.li [
                      HE.onClick $ \e ->
                        if ME.buttons e == 0
                          then Just (RemoveCountry c)
                          else Nothing
                    , HE.onMouseOver $ \_ -> Just (Highlight c)
                    , HE.onMouseOut  $ \_ -> Just Unhighlight
                    , HP.title "Remove"
                    ]
                    [ HH.text c ]
              ]
          , HH.div [HU.classProp "grid__col grid__col--2-of-8 country-picker-buttons"] [
              HH.button [
                  HP.type_ HP.ButtonButton
                , HE.onClick (\_ -> Just DumpCountries)
                , HU.classProp "add-all-button"
                ]
                [ HH.text "Add All" ]
            , HH.button [
                  HP.type_ HP.ButtonButton
                , HE.onClick (\_ -> Just ClearCountries)
                , HU.classProp "remove-all-button"
                ]
                [ HH.text "Remove All" ]
            ]
          ]
        ]
      ]
    , HH.div [ HU.classProp "grid__col grid__col--2-of-5 welcome dialog" ]
        case st.welcomeText of
          Nothing -> []
          Just t  -> [ HH.slot _welcomeFrame
              unit
              RawHTML.component
              { html: t, elRef: "welcome-ref" }
              absurd
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
    , HH.div [HU.classProp "grid__col grid__col--1-of-5 axis-x"] [
        HH.slot _projection XAxis (Projection.component "X Axis")
          st.xAxis
          (\(Projection.Update s) -> Just (SetProjection XAxis s))
      ]
    , HH.slot _postRender unit postRender unit (\_ -> Just Linkify)
    ]
  where
    projLabel dp = withDSum dp (\_ -> projectionLabel)
    title = projLabel (st.yAxis.projection) <> " vs. " <> projLabel (st.xAxis.projection)
    hw = { height: 666.6, width: 1000.0 }

type M = H.HalogenM State Action ChildSlots

handleAction
    :: forall o m. MonadAff m
     => CoronaData
     -> Action
     -> M o m Unit
handleAction dat = case _ of
    SetCountries cs -> loadCountries dat cs
    AddCountry c -> do
      H.modify_ $ \st -> st
        { countries  = S.insert c st.countries
        , unselected = S.delete c st.unselected
        }
      reRender dat Nothing
    RemoveCountry c -> do
      H.modify_ $ \st -> st
        { countries  = S.delete c st.countries
        , unselected = S.insert c st.unselected
        }
      reRender dat Nothing
    ClearCountries -> do
      H.modify_ $ \st -> st
        { countries = S.empty :: Set Country
        , unselected = st.allCountries
        }
      reRender dat Nothing
    DumpCountries -> do
      H.modify_ $ \st -> st
        { countries = st.allCountries
        , unselected = S.empty :: Set Country
        }
      reRender dat Nothing
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
      H.modify_ $ \st ->
        st { waiting = S.fromFoldable [Just XAxis, Just YAxis, Just ZAxis, Just TAxis] <> st.waiting }
        -- (_ { waiting = S.singleton Nothing :: Set (Maybe Axis) })
      for_ [XAxis, YAxis, ZAxis, TAxis] $ \a -> do
        loadProj a (defaultProjections ^. axisLens a)
    CopyURI -> void <<< runMaybeT $ do
      e  <- MaybeT $ H.getHTMLElementRef loaduriRef
      ie <- maybe MZ.empty pure $ HTMLInputElement.fromHTMLElement e
      liftEffect $ do
        HTMLElement.setHidden false e
        HTMLInputElement.select ie
        HTMLInputElement.setSelectionRange 0 999999 "none" ie
        execCopy
        HTMLElement.setHidden true e
        toast "Copied Link to Clipboard!"
    Linkify -> do
      -- log "linkify me, captain"
      void $ H.subscribe $ ES.effectEventSource $ \e -> do
        ac <- runFn3 linkify
          (ES.emit e <<< LoadURIString)
          (ES.emit e CopyURI)
          (ES.emit e SaveFile)
        pure mempty
    LoadURIString str -> do
      liftEffect scrollToTop
      loadUriString false str
    Initialize -> do
      welcome     <- liftAff $ HU.selectElement (DOM.QuerySelector "#welcome-text")
      for_ welcome $ \wcm -> do
        welcomeText <- liftEffect $ cutInnerHTML wcm
        H.modify_ $ _ { welcomeText = Just welcomeText }
      wrapper <- liftAff $ HU.selectElement (DOM.QuerySelector "#ui")
      for_ wrapper $ \wrp -> do
        liftEffect $ HTMLElement.setClassName "" wrp

      loadUri
  where
    loadUriString useDef str = do
      usp <- liftEffect $ USP.new str
      launches <- map A.catMaybes <<< for (uriSpecs dat) $ \uspec -> do
         res <- uspec.loader usp
         case res of
           Left  def
             | useDef    -> pure $ Just (Tuple uspec.waiter def)
             | otherwise -> pure Nothing
           Right r -> pure $ Just (Tuple uspec.waiter r)
      H.modify_ $ \st -> st { waiting = S.fromFoldable (map fst launches) <> st.waiting }
      traverse_ snd launches
    loadUri = do
      str <- liftEffect $ Location.search
                      =<< Window.location
                      =<< Web.window
      loadUriString true str

type URISpec m =
        { waiter  :: Maybe Axis
        , default :: String
        , write   :: State -> String
        , param   :: String
        , include :: Boolean
        -- | Left: run if nothing is found, Right: run if thing is found.
        , loader  :: USP.URLSearchParams -> m (Either (m Unit) (m Unit))
        }

uriSpecs :: forall o m. MonadEffect m => CoronaData -> Array (URISpec (M o m))
uriSpecs dat = A.snoc axisSpec (countrySpec dat)
  where
    axisSpec = [XAxis, YAxis, ZAxis, TAxis] <#> \a ->
      let def = defaultProjections ^. axisLens a
      in  { waiter: Just a
          , default: Projection.stateSerialize def
          , write: \st -> Projection.stateSerialize $ st ^. axisLens a
          , param: axisParam a
          , include: true
          , loader: \usp -> bimap (const (loadProj a def)) (loadProj a)
                <$> liftEffect (parseAtKey usp (axisParam a) Projection.stateParser)
          }

loadProj :: forall o m. MonadEffect m => Axis -> Projection.State -> M o m Unit
loadProj a p = do
  res <- H.query _projection a $
    HQ.tell (Projection.QueryPut p)
  when (isNothing res) $
    warn "warning: projection load did not return response"

loadCountries :: forall o m. MonadEffect m => CoronaData -> Set String -> M o m Unit
loadCountries dat ctys = do
   H.modify_ (_ { countries = ctys })
   reRender dat (Just Nothing)

countrySpec :: forall o m. MonadEffect m => CoronaData -> URISpec (M o m)
countrySpec dat =
    { waiter: Nothing
    , default: serializeSet initialCountries
    , write: \st -> serializeSet $ st.countries
    , param
    , include: false
    , loader: \usp -> bimap (const (load initialCountries)) load
            <$> liftEffect (parseAtKey usp param parseSet)
    }
  where
    load = loadCountries dat
    param = "r"

generateUri
    :: forall o m. MonadEffect m
    => CoronaData
    -> M o m { historyPush :: String, permalink :: String }
generateUri dat = do
    st <- H.get
    liftEffect $ do
      uspHistPush <- USP.new ""
      uspPerma <- USP.new ""
      for_ (uriSpecs dat :: Array (URISpec (M o m))) $ \spec -> do
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
    -- log $ show initter
    waiting <- for initter $ \a -> do
      State.state $ \st ->
        let newWaiting = S.delete a st.waiting
        in  Tuple (null newWaiting) (st { waiting = newWaiting })
    when (and waiting) $ do
      st :: State <- H.get
      _ <- H.query _autocomplete unit $ HQ.tell $ Autocomplete.SetOptions $
          A.fromFoldable st.unselected
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
      uri <- generateUri dat
      H.modify_ (_ { permalink = Just uri.permalink })
      liftEffect $ do
        hist <- liftEffect $ Window.history =<< Web.window
        History.pushState
          (Foreign.unsafeToForeign uri.historyPush)
          (History.DocumentTitle "Coronavirus Data Tracker")
          (History.URL uri.historyPush)
          hist

-- really hacky way to have post-render hooks
postRender
    :: forall f i m. H.Component HH.HTML f i Unit m
postRender = H.mkComponent
    { initialState: \_ -> unit
    , render: \_ -> HH.text ""
    , eval: H.mkEval $ H.defaultEval
        { handleAction = \_ -> H.raise unit
        , receive = \_ -> Just unit
        }
    }

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

-- _multiselect :: SProxy "multiselect"
-- _multiselect = SProxy

_autocomplete :: SProxy "autocomplete"
_autocomplete = SProxy

_projection :: SProxy "projection"
_projection = SProxy

_postRender :: SProxy "postRender"
_postRender = SProxy

_welcomeFrame :: SProxy "welcomeFrame"
_welcomeFrame = SProxy

loaduriRef ∷ H.RefLabel
loaduriRef = H.RefLabel "loaduri-input"

foreign import fullPagename :: Effect String
foreign import execCopy :: Effect Unit
foreign import linkify
    :: Fn3 (String -> Effect Unit)
           (Effect Unit)
           (Effect Unit)
           (Effect Unit)
foreign import cutInnerHTML :: HTMLElement.HTMLElement -> Effect String
foreign import toast :: String -> Effect Unit
foreign import scrollToTop :: Effect Unit
-- foreign import moveDiv :: Fn2 HTMLElement.HTMLElement HTMLElement.HTMLElement (Effect Unit)
