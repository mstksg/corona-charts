
module Halogen.Autocomplete where

import Prelude

import CSS.Geometry as CSS
import CSS.Size as CSS
import Control.Monad.Maybe.Trans
import Control.Monad.State as State
import Control.MonadZero as MZ
import Data.Foldable
import Data.Function.Uncurried
import Data.Int
import Data.Maybe
import Data.Options
import Data.Traversable
import Data.Tuple
import Effect
import Effect.Aff
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class
import Effect.Class.Console
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS as HC
import Halogen.HTML.Properties as HP
import Halogen.Query.EventSource as ES
import Halogen.Util as HU
import Undefined
import Web.HTML as Web
import Web.HTML.HTMLElement as HTMLElement
import Web.HTML.HTMLInputElement as HTMLInputElement

foreign import data Autocomplete :: Type

type State =
    { autocomplete  :: Maybe Autocomplete
    , options :: Array String
    }

data Action =
        Initialize
      | Register Autocomplete
      | Feedback String

data Output
    = Selected String

data Query r = SetOptions (Array String) r

component
    :: forall q i o m. MonadAff m
    => String           -- ^ ident (no #)
    -> String           -- ^ placeholder
    -> H.Component HH.HTML Query (Array String) Output m
component ident place =
  H.mkComponent
  { initialState: \o -> { autocomplete: Nothing, options: o }
  , render: render ident place
  , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction ident place
        , handleQuery  = handleQuery
        , initialize   = Just Initialize
        }
  }


render :: forall m. String -> String -> State -> H.ComponentHTML Action () m
render ident place st = HH.div
    [ HU.classProp "autocomplete-widget"
    ]
    [ HH.input [
        HP.id_ ident
      , HP.type_ HP.InputText
      , HP.placeholder place
      , HP.ref acRef
      ]
    ]

handleAction
    :: forall m. MonadAff m
    => String       -- ^ identifier
    -> String       -- ^ placeholder
    -> Action
    -> H.HalogenM State Action () Output m Unit
handleAction ident placeholder = case _ of
    Initialize -> do
      opts <- H.gets _.options
      void $ H.subscribe $ ES.effectEventSource $ \e -> do
        ac <- runFn4 _initAutocomplete
            opts
            ident
            placeholder
            (ES.emit e <<< Feedback)
        ES.emit e (Register ac)
        pure mempty
    Register ac -> H.modify_ (_ { autocomplete = Just ac })
    Feedback str -> do
       void $ runMaybeT $ do
         e  <- MaybeT $ H.getHTMLElementRef acRef
         ie <- maybe MZ.empty pure $ HTMLInputElement.fromHTMLElement e
         liftEffect $ HTMLInputElement.setValue "" ie
       H.raise $ Selected str

handleQuery
    :: forall m r. MonadEffect m
    => Query r
    -> H.HalogenM State Action () Output m (Maybe r)
handleQuery = case _ of
    SetOptions xs next -> do
      mac <- State.state $ \st ->
        Tuple st.autocomplete (st { options = xs })
      liftEffect $
        for mac $ \ac -> next <$ runFn2 _setOpts ac xs




foreign import _initAutocomplete
    :: Fn4 (Array String)           -- ^ options
           String                   -- ^ selector
           String                   -- ^ placeholder
           (String -> Effect Unit)  -- ^ callback
           (Effect Autocomplete)

foreign import _setOpts
    :: Fn2 Autocomplete
           (Array String)
           (Effect Unit)

-- exports._initAutocomplete = function (sel,place,callback) {
-- exports._setOpts = function(ac, xs) {

acRef ∷ H.RefLabel
acRef = H.RefLabel "ac-ref"

