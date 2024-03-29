module Corona.Chart.UI where

import Prelude

import Control.Monad.Except
import Control.Monad.Maybe.Trans
import Control.Monad.State
import Control.Monad.State.Class as State
import Control.Monad.Writer
import Control.MonadZero as MZ
import Corona.Chart
import Corona.Chart.Assemble
import Corona.Chart.Model
import Corona.Chart.UI.Projection as Projection
import Corona.Data as Corona
import Corona.Data.Type
import Corona.Marshal as Marshal
import D3.Scatter.Type (SType(..), NType(..), Scale(..), NScale(..), Axis(..), ModelFit(..), allAxis)
import D3.Scatter.Type as D3
import Data.Array as A
import Data.Bifunctor
import Data.Const
import Data.Date as Date
import Data.Either
import Data.Exists
import Data.Foldable
import Data.Function
import Data.Function.Uncurried
import Data.Functor.Compose
import Data.Functor.Product
import Data.FunctorWithIndex
import Data.Identity as Identity
import Data.Int
import Data.Lens
import Data.Lens.Record as LR
import Data.Map (Map)
import Data.Map as M
import Data.Maybe
import Data.ModifiedJulianDay as MJD
import Data.Number as N
import Data.Ord
import Data.Point
import Data.Set (Set)
import Data.Set as S
import Data.String as String
import Data.String.Pattern as Pattern
import Data.Symbol (SProxy(..))
import Data.Traversable
import Data.Tuple
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
import Text.Parsing.StringParser.CodeUnits as P
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
import Web.TouchEvent.Touch as TE
import Web.TouchEvent.TouchEvent as TE
import Web.TouchEvent.TouchList as TE
import Web.UIEvent.MouseEvent as ME
import Web.URLSearchParams as USP

type V4 a = { x :: a, y :: a, z :: a, t :: a }

type AxisState =
    { projection :: DSum SType Projection
    , numScale   :: NScale                   -- ^ date scale is always day
    }

type RegionState =
  { selected   :: Map Corona.Dataset (Set Region)
  , unselected :: Set Region
  , allRegions :: Set Region
  -- , sourceSpec :: Corona.Dataset
  }

type Models =
    { linFit   :: Boolean
    , expFit   :: Boolean
    , logFit   :: Boolean
    , decFit   :: Boolean
    , quadFit  :: Boolean
    , tail     :: Int
    , forecast :: Int
    }

type State =
    { datasetSpec  :: Corona.Dataset
    , axis         :: V4 Projection.Out
    , modelStates  :: Models
    , regionState  :: RegionState
    -- Either (Set Region) RegionState   -- ^ queued up
    , dataset      :: Maybe CoronaData
    , permalink    :: Maybe String
    , waiting      :: Set Aspect     -- ^ do not load unless empty
    , welcomeText  :: Maybe String
    , helptip      :: Maybe HTMLElement.HTMLElement
    }

v4Lens :: forall a. Axis -> Lens' (V4 a) a
v4Lens = case _ of
    XAxis -> LR.prop (SProxy :: SProxy "x")
    YAxis -> LR.prop (SProxy :: SProxy "y")
    ZAxis -> LR.prop (SProxy :: SProxy "z")
    TAxis -> LR.prop (SProxy :: SProxy "t")

axisParam :: Axis -> String
axisParam = case _ of
    XAxis -> "x"
    YAxis -> "y"
    ZAxis -> "z"
    TAxis -> "t"

modelFitLens :: ModelFit -> Lens' Models Boolean
modelFitLens = case _ of
    LinFit  -> LR.prop (SProxy :: SProxy "linFit")
    ExpFit  -> LR.prop (SProxy :: SProxy "expFit")
    LogFit  -> LR.prop (SProxy :: SProxy "logFit")
    DecFit  -> LR.prop (SProxy :: SProxy "decFit")
    QuadFit -> LR.prop (SProxy :: SProxy "quadFit")


data Aspect = ADataset
            | AAxis Axis
            | ARegions
            | AModels
derive instance eqAspect :: Eq Aspect
derive instance ordAspect :: Ord Aspect
instance showAspect :: Show Aspect where
    show = case _ of
      ADataset -> "ADataset"
      AAxis a  -> "AAxis " <> show a
      ARegions -> "ARegions"
      AModels  -> "AModels"


data Action =
        SetRegions Corona.Dataset (Set Region)
      | AddRegion String
      | RemoveRegion String
      | ClearRegions
      | DumpRegions
      | ResetRegions
      | SetProjection Axis Projection.Out
      -- | LoadProjection Axis Projection.Out
      | SetModel ModelFit Boolean
      | SetModelTail Int
      | SetModelForecast Int
      | LoadDataset Corona.Dataset
      | Highlight Region
      | Unhighlight
      | LoadHelptip String { x :: Int, y :: Int }
      | UnloadHelptip
      | SaveFile
      | Redraw
      | Reset
      | CopyURI
      | LoadURIString String
      | Initialize
      | Linkify

type ChildSlots =
        ( scatter     :: H.Slot Scatter.Query    Void              Unit
        -- , multiselect :: H.Slot (MultiSelect.Query Region) (MultiSelect.Output Region) Unit
        , autocomplete :: H.Slot Autocomplete.Query Autocomplete.Output Unit
        , projection  :: H.Slot Projection.Query Projection.Output Axis
        , postRender  :: H.Slot (Const Void) Unit Unit     -- hm we could integrate this
        , welcomeFrame :: H.Slot (Const Void) Void Unit    -- with this
        )


initialRegions :: Corona.Dataset -> Set Region
initialRegions = case _ of
    Corona.WorldData -> S.fromFoldable [
        "United States"
      , "Egypt"
      , "Italy"
      , "South Korea"
      , "Russia"
      ]
    Corona.USData -> S.fromFoldable [
        "California"
      , "New York"
      , "Washington"
      , "North Carolina"
      , "Iowa"
      ]


component :: forall f i o m. MonadAff m => H.Component HH.HTML f i o m
component = H.mkComponent
    { initialState: \_ ->
        { datasetSpec: Corona.WorldData
        , axis: defaultProjections
        , regionState: {
            selected: M.empty
          , unselected: S.empty
          , allRegions: S.empty
          }
        , modelStates: defaultModels
        , dataset: Nothing
        , permalink: Nothing
        , waiting: S.empty
        , welcomeText: Nothing
        , helptip: Nothing
        }
    , render: render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , initialize   = Just Initialize
        }
    }

defaultProjections :: V4 Projection.Out
defaultProjections = {
    x: D3.sDay :=> Product (Tuple
      ( projection
        { base: Time refl
        , operations: C.nil
        }
      ) (D3.Date refl)
    )
  , y: D3.sInt :=> Product (Tuple
      ( projection
        { base: Confirmed refl
        , operations: Delta D3.nInt refl
                 C.:> C.nil
        }
      ) (D3.Log D3.nInt)
    )
  , z: D3.sDays :=> Product (Tuple
      ( projection
        { base: Confirmed refl
        , operations: Restrict D3.sInt refl After (AtLeast 50)
                 C.:> DayNumber refl After
                 C.:> C.nil
        }
      ) (D3.Linear (Left refl) false)
    )
  , t: D3.sDay :=> Product (Tuple
      ( projection
        { base: Time refl
        , operations: C.nil
        }
      ) (D3.Date refl)
    )
  }

defaultModels :: Models
defaultModels = {
      linFit: false
    , quadFit: false
    , expFit: false
    , logFit: true
    , decFit: false
    , forecast: 21
    , tail: 35
    }

render :: forall m. MonadAff m => State -> H.ComponentHTML Action ChildSlots m
render st = HH.div [HU.classProp "ui-wrapper"] [
      HH.div [HU.classProp "grid__col grid__col--1-of-5 left-column"] [
        HH.h2_ [HH.text "Coronavirus Data Plotter"]
      , HH.div [HU.classProp "dataset-picker dialog"] [
          HH.span_ [HH.text "Dataset"]
        , HH.select [HE.onSelectedIndexChange (map LoadDataset <<< (allDatasets A.!! _))] $
            allDatasets <#> \ds ->
              let isSelected = ds == st.datasetSpec
              in  HH.option [HP.selected isSelected] [HH.text (datasetLabel ds)]
        ]
      , HH.div [HU.classProp "axis-y grid__col--m-1-of-2", HP.id_ "axis-y"] [
          HH.slot _projection YAxis (Projection.component YAxis)
            st.axis.y
            (\(Projection.Update s) -> Just (SetProjection YAxis s))
          ]
      , HH.div [HU.classProp "model-picker dialog grid__col--m-1-of-2"] modelPicker
      ]
    , HH.div [HU.classProp "grid__col grid__col--4-of-5 plot"] [
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
        , HH.div [HU.classProp "region-picker"] [
            HH.div [HU.classProp "grid__col grid__col--1-of-4 grid__col--m-1-of-2 region-picker-input"] [
              HH.slot _autocomplete unit
                (Autocomplete.component "region-select" "Search for a region...")
                (A.fromFoldable st.regionState.unselected)
                (case _ of Autocomplete.Selected str -> Just (AddRegion str))
            ]
          , HH.div [HU.classProp "grid__col grid__col--2-of-4 region-picker-select"]
              [ HH.ul [HU.classProp "picked-regions"] $
                  A.fromFoldable (lookupRegions st.datasetSpec st.regionState.selected) <#> \c ->
                    HH.li [
                      HE.onClick $ \e ->
                        if ME.buttons e == 0
                          then Just (RemoveRegion c)
                          else Nothing
                    , HE.onMouseOver $ \_ -> Just (Highlight c)
                    , HE.onMouseOut  $ \_ -> Just Unhighlight
                    , HP.title "Remove"
                    ]
                    [ HH.text c ]
              ]
          , HH.div [HU.classProp "grid__col grid__col--1-of-4 grid__col--m-1-of-2 region-picker-buttons"] [
              HH.button [
                  HP.type_ HP.ButtonButton
                , HE.onClick (\_ -> Just DumpRegions)
                , HU.classProp "add-all-button"
                ]
                [ HH.text "Add All" ]
            , HH.button [
                  HP.type_ HP.ButtonButton
                , HE.onClick (\_ -> Just ClearRegions)
                , HU.classProp "remove-all-button"
                ]
                [ HH.text "Remove All" ]
            , HH.button [
                  HP.type_ HP.ButtonButton
                , HE.onClick (\_ -> Just ResetRegions)
                , HU.classProp "reset-regions-button"
                ]
                [ HH.text "Reset" ]
            ]
          ]
        , HH.div [HU.classProp "axis-links"] [
            HH.ul_ $ [YAxis, XAxis, TAxis, ZAxis] <#> \a ->
              HH.li_
                [ HH.a [HP.href $ "#axis-" <> axisParam a]
                    [HH.text $ "Customize " <> D3.axisLabel a]
                ]
          ]
        ]
      ]
    , HH.div [ HU.classProp "grid__col grid__col--2-of-5 welcome copy dialog" ]
        case st.welcomeText of
          Nothing -> []
          Just t  -> [ HH.slot _welcomeFrame
              unit
              RawHTML.component
              { html: t, elRef: "welcome-ref" }
              absurd
            ]
    , HH.div [HU.classProp "grid__col grid__col--1-of-5 axis-z", HP.id_ "axis-z"] [
        HH.slot _projection ZAxis (Projection.component ZAxis)
          st.axis.z
          (\(Projection.Update s) -> Just (SetProjection ZAxis s))
      ]
    , HH.div [HU.classProp "grid__col grid__col--1-of-5 axis-t", HP.id_ "axis-t"] [
        HH.slot _projection TAxis (Projection.component TAxis)
          st.axis.t
          (\(Projection.Update s) -> Just (SetProjection TAxis s))
      ]
    , HH.div [HU.classProp "grid__col grid__col--1-of-5 axis-x", HP.id_ "axis-x"] [
        HH.slot _projection XAxis (Projection.component XAxis)
          st.axis.x
          (\(Projection.Update s) -> Just (SetProjection XAxis s))
      ]
    , HH.slot _postRender unit postRender unit (\_ -> Just Linkify)
    ]
  where
    title = Projection.outLabel st.axis.y <> " vs. " <> Projection.outLabel st.axis.x
    hw = { height: 666.6, width: 1000.0 }
    allDatasets = [ Corona.WorldData, Corona.USData ]
    datasetLabel = case _ of
      Corona.WorldData -> "World"
      Corona.USData -> "United States"
    anyModelsActive = any (\mfit -> st.modelStates ^. modelFitLens mfit) D3.allModelFit
    modelColor = case _ of
      LinFit -> "p-primary"
      ExpFit -> "p-success"
      LogFit -> "p-info"
      DecFit -> "p-warning"
      QuadFit -> "p-danger"
    modelPicker = fold [
        [ HH.h3_ [HH.text "Analysis"]
        , HH.ul [HU.classProp "model-picker-list"] $ D3.allModelFit <#> \mfit ->
          HH.li [HU.classProp "model-picker-fit"] [
            HH.div [HU.classProp "pretty p-switch p-fill"] [
                  HH.input [
                    HP.type_ HP.InputCheckbox
                  , HP.checked (st.modelStates ^. modelFitLens mfit)
                  , HE.onChecked (Just <<< SetModel mfit)
                  ]
                , HH.div [HU.classProp $ "state " <> modelColor mfit] [
                  HH.label_ [
                    HH.text (D3.modelFitLabel mfit)
                  ]
                ]
              ]
            , helptipLink $ case mfit of
                LinFit -> "linear-fit"
                ExpFit -> "exponential-growth-fit"
                LogFit -> "logistic-fit"
                DecFit -> "exponential-decay-fit"
                QuadFit -> "quadratic-fit"
          ]
        ]
      , if anyModelsActive
          then [
            HH.div [HU.classProp "model-pick-lookback"] [
              HH.span_ [HH.text "Lookback"]
            , HH.input [
                HP.type_ HP.InputNumber
              , HP.value $ show st.modelStates.tail
              , HE.onValueChange (map SetModelTail <<< parsePosInt)
              ]
            ]
          , HH.div [HU.classProp "model-pick-forecast"] [
              HH.span_ [HH.text "Forecast"]
            , HH.input [
                HP.type_ HP.InputNumber
              , HP.value $ show st.modelStates.forecast
              , HE.onValueChange (map SetModelForecast <<< parsePosInt)
              ]
            ]
          ]
          else []
      ]
    parsePosInt = map (max 1 <<< abs <<< round) <<< N.fromString

helptipLink :: forall a c m. String -> H.ComponentHTML a c m
helptipLink id = HH.a [
      HP.href $ "#" <> id
    , HU.classProp "helptip-link noselect"
    ] [HH.text "?"]

firstTouch :: TE.TouchEvent -> Maybe { x :: Int, y :: Int }
firstTouch = map (\t -> { x: TE.pageX t, y: TE.pageY t })
         <<< TE.item 0
         <<< TE.targetTouches

type M = H.HalogenM State Action ChildSlots

handleAction
    :: forall o m. MonadAff m
     => Action
     -> M o m Unit
handleAction = case _ of
    SetRegions dspec cs -> loadRegions dspec cs
    AddRegion c -> do
      H.modify_ $ \st -> st
        { regionState = st.regionState
          { selected   = M.insert st.datasetSpec
              (S.insert c $ lookupRegions st.datasetSpec st.regionState.selected)
              st.regionState.selected
          , unselected = S.delete c st.regionState.unselected
          }
        }
      reRender Nothing
    RemoveRegion c -> do
      H.modify_ $ \st -> st
        { regionState = st.regionState
          { selected   = M.insert st.datasetSpec
              (S.delete c $ lookupRegions st.datasetSpec st.regionState.selected)
              st.regionState.selected
          , unselected = S.insert c st.regionState.unselected
          }
        }
      reRender Nothing
    ClearRegions -> do
      H.modify_ $ \st -> st
        { regionState = st.regionState
            { selected = M.insert st.datasetSpec S.empty st.regionState.selected
            , unselected = st.regionState.allRegions
            }
        }
      reRender Nothing
    DumpRegions -> do
      H.modify_ $ \st -> st
        { regionState = st.regionState
            { selected   = M.insert st.datasetSpec st.regionState.allRegions st.regionState.selected
            , unselected = S.empty :: Set Region
            }
        }
      reRender Nothing
    ResetRegions -> resetRegions
    SetProjection a p -> do
      H.modify_ $ \st -> st
        { axis = set (v4Lens a) p st.axis }
      reRender (Just (AAxis a))
    -- LoadProjection a p -> loadProj a p
    LoadDataset ds -> loadDataset ds
    SetModel mfit b -> do
      H.modify_ $ \st -> st
        { modelStates = set (modelFitLens mfit) b st.modelStates  }
      reRender Nothing
    SetModelTail n -> do
      H.modify_ $ \st -> st
        { modelStates = st.modelStates { tail = n } }
      reRender Nothing
    SetModelForecast n -> do
      H.modify_ $ \st -> st
        { modelStates = st.modelStates { forecast = n } }
      reRender Nothing
    Highlight c -> void $ H.query _scatter unit $ HQ.tell (Scatter.Highlight c)
    Unhighlight -> void $ H.query _scatter unit $ HQ.tell Scatter.Unhighlight
    LoadHelptip id pos -> do
      -- log "hi"
      tip <- liftAff $ HU.selectElement (DOM.QuerySelector id)
      case tip of
        Nothing -> warn $ "helptip not found for " <> id
        Just tp -> do
          H.modify_ $ _ { helptip = Just tp }
          liftEffect $ do
            HTMLElement.setClassName "helptip helptip-active" tp
            runFn2 setPos tp (pos { x = pos.x+5 })
    UnloadHelptip -> do
      tip <- State.state $ \st -> Tuple st.helptip (st { helptip = Nothing })
      for_ tip $ \tp ->
        liftEffect $ HTMLElement.setClassName "helptip" tp
    SaveFile -> do
      r <- H.query _scatter unit $ HQ.request (Scatter.Export "coronavirus-plot.png")
      liftEffect $ toast
        if or r
          then "Exported file!"
          else "No chart to export!"
    Redraw -> reRender Nothing
    Reset  -> do
      H.modify_ $ \st ->
        st { waiting = S.fromFoldable (AAxis <$> allAxis) <> st.waiting }
        -- (_ { waiting = S.singleton Nothing :: Set (Maybe Axis) })
      for_ allAxis $ \a -> do
        loadProj a (defaultProjections ^. v4Lens a)
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
        _ <- runFn3 linkify
          (\str -> scrollToTop $
                ES.emit e (LoadURIString str)
          )
          (ES.emit e CopyURI)
          (ES.emit e SaveFile)
        pure mempty
      void $ H.subscribe $ ES.effectEventSource $ \e -> do
        _ <- runFn2 helpify
          (\x y -> ES.emit e (LoadHelptip x y))
          (ES.emit e UnloadHelptip)
        pure mempty
    LoadURIString str ->
      loadUriString false str
    Initialize -> do
      -- setup welcome
      welcome     <- liftAff $ HU.selectElement (DOM.QuerySelector "#welcome-text")
      for_ welcome $ \wcm -> do
        welcomeText <- liftEffect $ cutInnerHTML wcm
        H.modify_ $ _ { welcomeText = Just welcomeText }

      -- remove loading waiter
      wrapper <- liftAff $ HU.selectElement (DOM.QuerySelector "#ui")
      for_ wrapper $ \wrp -> do
        liftEffect $ HTMLElement.setClassName "" wrp

      -- load uri
      loadUri
  where
    -- useDef: if not found, use the default value
    loadUriString useDef str = do
      usp <- liftEffect $ USP.new str
      launches <- map A.catMaybes <<< for uriSpecs $ \uspec -> do
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

lookupRegions
    :: Corona.Dataset
    -> Map Corona.Dataset (Set Region)
    -> Set Region
lookupRegions ds mp = case M.lookup ds mp of
    Just c  -> c
    Nothing -> initialRegions ds



type URISpec m =
        { waiter  :: Aspect
        , default :: String
        , write   :: State -> String
        , param   :: String
        , include :: Boolean
        -- | Left: run if nothing is found, Right: run if thing is found.
        , loader  :: USP.URLSearchParams -> m (Either (m Unit) (m Unit))
        }

uriSpecs :: forall o m. MonadAff m => Array (URISpec (M o m))
-- uriSpecs = axisSpec <> [datasetSpec, modelSpec]
uriSpecs = axisSpec <> [modelSpec, datasetSpec, regionSpec]
  where
    regionParam = "r"
    datasetParam = "d"
    modelsParam = "m"
    axisSpec = allAxis <#> \a ->
      let def = defaultProjections ^. v4Lens a
      in  { waiter: AAxis a
          , default: Projection.outSerialize def
          , write: \st -> Projection.outSerialize $ st.axis ^. v4Lens a
          , param: axisParam a
          , include: true
          , loader: \usp -> bimap (const (loadProj a def)) (loadProj a)
                <$> liftEffect (parseAtKey usp (axisParam a) Projection.outParser)
          }
    regionSpec =
        { waiter: ARegions
        , default: serializeRegions Corona.WorldData
              (initialRegions Corona.WorldData)
        , write: \st ->
                  serializeRegions
                    st.datasetSpec
                    (lookupRegions st.datasetSpec st.regionState.selected)
        , param: regionParam
        , include: true
        , loader: \usp -> liftEffect (parseAtKey usp regionParam parseRegions) <#> bimap
                  (\_ -> resetRegions)
                  (\d -> loadRegions d.datasetSpec d.regions)
        }
    datasetSpec =
        { waiter: ADataset
        , default: Marshal.serialize Corona.WorldData
        , write: Marshal.serialize <<< _.datasetSpec
        , param: datasetParam
        , include: true
        , loader: \usp -> bimap (const (loadDataset Corona.WorldData)) loadDataset
                <$> liftEffect (parseAtKey usp datasetParam Marshal.parse)
        }
    modelSpec =
        { waiter: AModels
        , default: serializeModels defaultModels
        , write: serializeModels <<< _.modelStates
        , param: modelsParam
        , include: true
        , loader: \usp -> bimap (const (loadModels defaultModels)) loadModels
                <$> liftEffect (parseAtKey usp modelsParam parseModels)
        }


loadProj :: forall o m. MonadEffect m => Axis -> Projection.Out -> M o m Unit
loadProj a p = do
  res <- H.query _projection a $
    HQ.tell (Projection.QueryPut p)
  when (isNothing res) $
    warn "warning: projection load did not return response"

loadRegions :: forall o m. MonadAff m => Corona.Dataset -> Set String -> M o m Unit
loadRegions dspec ctys = do
    H.modify_ $ \st -> st
       { regionState = st.regionState
           { selected = M.insert st.datasetSpec ctys st.regionState.selected
           , unselected = if st.datasetSpec == dspec
               then st.regionState.allRegions `S.difference` ctys
               else st.regionState.unselected
           }
       }
    reRender (Just ARegions)

resetRegions :: forall o m. MonadAff m => M o m Unit
resetRegions = do
    H.modify_ $ \st -> st
      { regionState = st.regionState
          { selected = M.delete st.datasetSpec st.regionState.selected
          , unselected = st.regionState.allRegions
            `S.difference` initialRegions st.datasetSpec
          }
      }
    reRender (Just ARegions)

loadDataset :: forall o m. MonadAff m => Corona.Dataset -> M o m Unit
loadDataset dspec = do
    st0      <- H.get
    oldDspec <- H.gets _.datasetSpec
    currDset <- H.gets _.dataset
    let skip = dspec == oldDspec
            && not (null currDset)
            -- && not (M.member dspec st0.regionState)
    unless skip $ liftAff (Corona.fetchDataset dspec) >>= case _ of
      Left e     -> warn $ "warning: dataset failed to load -- " <> e
      Right dset -> H.modify_ $ \st ->
        let allRegs = S.fromFoldable (O.keys dset.dat)
        in  st { datasetSpec = dspec
               , dataset     = Just dset
               , regionState = st.regionState
                   { allRegions = allRegs
                   , unselected = allRegs
                        `S.difference` lookupRegions dspec st.regionState.selected
                   }
               }
    reRender (Just ADataset)

-- hm maybe make sure source spec matches?
loadModels :: forall o m. MonadAff m => Models -> M o m Unit
loadModels models = do
   H.modify_ $ \st -> st { modelStates = models }
   reRender (Just AModels)



generateUri
    :: forall o m. MonadAff m
    => M o m { historyPush :: String, permalink :: String }
generateUri = do
    st <- H.get
    liftEffect $ do
      uspHistPush <- USP.new ""
      uspPerma <- USP.new ""
      for_ (uriSpecs :: Array (URISpec (M o m))) $ \spec -> do
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
    :: forall o m. MonadAff m
    => Maybe Aspect
    -> H.HalogenM State Action ChildSlots o m Unit
reRender initter = do
    -- log $ show initter
    waiting <- for initter $ \a -> do
      State.state $ \st ->
        let newWaiting = S.delete a st.waiting
        in  Tuple (null newWaiting) (st { waiting = newWaiting })
    -- log <<< show =<< H.gets _.regionState.selected
    -- log <<< show =<< H.gets _.dataset
    when (and waiting) $ do
      st :: State <- H.get
      for_ st.dataset $ \dat -> do
        _ <- H.query _autocomplete unit $ HQ.tell $ Autocomplete.SetOptions $
            A.fromFoldable st.regionState.unselected
        let selected = case M.lookup st.datasetSpec st.regionState.selected of
              Nothing -> initialRegions st.datasetSpec
              Just s  -> s
        withDSum st.axis.x (\tX (Product (Tuple pX sX)) ->
          withDSum st.axis.y (\tY (Product (Tuple pY sY)) ->
            withDSum st.axis.z (\tZ (Product (Tuple pZ sZ)) ->
              withDSum st.axis.t (\tT (Product (Tuple pT sT)) -> void $
                H.query _scatter unit $ HQ.tell $ Scatter.Update
                  (\f -> f tX tY tZ tT (
                        toScatterPlot
                          dat
                          (mkScatterModels st.modelStates)
                          ({ x : PS { projection: pX, scale: sX }
                           , y : PS { projection: pY, scale: sY }
                           , z : PS { projection: pZ, scale: sZ }
                           , t : PS { projection: pT, scale: sT }
                          })
                          selected
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
            (History.DocumentTitle "Coronavirus Data Plotter")
            (History.URL uri.historyPush)
            hist

mkScatterModels :: Models -> Array ModelSpec
mkScatterModels md = flip A.mapMaybe D3.allModelFit $ \mfit ->
    if md ^. modelFitLens mfit
      then Just { fit: mfit, tail: md.tail, forecast: md.forecast }
      else Nothing

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

serializeSet :: Set String -> String
serializeSet = String.joinWith "|" <<< A.fromFoldable

parseSet :: P.Parser (Set String)
parseSet = S.fromFoldable <<< String.split (Pattern.Pattern "|")
       <$> Marshal.parse

serializeRegions
    :: Corona.Dataset
    -> Set String
    -> String
serializeRegions ds reg = Marshal.serialize ds <> serializeSet reg

parseRegions :: P.Parser { datasetSpec :: Corona.Dataset, regions :: Set String }
parseRegions = do
    datasetSpec <- Marshal.parse
    regions     <- parseSet
    pure { datasetSpec, regions }


serializeModels :: Models -> String
serializeModels { linFit, expFit, logFit, decFit, quadFit, tail, forecast } =
       foldMap Marshal.serialize enableds
    <> if or enableds
         then Marshal.serialize tail <> "|" <> Marshal.serialize forecast
         else ""
  where
    enableds = [linFit, expFit, logFit, decFit, quadFit]

parseModels :: P.Parser Models
parseModels = do
    linFit   <- Marshal.parse
    expFit   <- Marshal.parse
    logFit   <- Marshal.parse
    decFit   <- Marshal.parse
    quadFit  <- Marshal.parse
    if or [logFit, expFit, logFit, decFit, quadFit]
      then do
        tail     <- Marshal.parse
        _        <- P.char '|'
        forecast <- Marshal.parse
        pure { linFit, expFit, logFit, decFit, quadFit, tail, forecast }
      else
        pure { linFit, expFit, logFit, decFit, quadFit
             , tail: defaultModels.tail
             , forecast: defaultModels.forecast
             }



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
foreign import helpify
    :: Fn2 (String -> { x :: Int, y :: Int} -> Effect Unit)
           (Effect Unit)
           (Effect Unit)
foreign import cutInnerHTML :: HTMLElement.HTMLElement -> Effect String
foreign import toast :: String -> Effect Unit
foreign import scrollToTop :: Effect Unit -> Effect Unit
foreign import setPos :: Fn2 HTMLElement.HTMLElement { x :: Int, y :: Int } (Effect Unit)
-- foreign import moveDiv :: Fn2 HTMLElement.HTMLElement HTMLElement.HTMLElement (Effect Unit)
