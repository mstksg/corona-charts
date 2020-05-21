
module Halogen.Scatter where

import Prelude

import CSS.Geometry as CSS
import CSS.Size as CSS
import D3.Scatter
import D3.Scatter.Type
import Data.Foldable
import Data.Int
import Data.Maybe
import Data.Options
import Data.Traversable
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS as HC
import Halogen.HTML.Properties as HP
import Halogen.Query.EventSource as ES
import Halogen.Util as HU

type State  =
    { chart      :: Maybe D3Scatter
    , interactor :: Maybe Interactor
    }
data Action = Initialize {width :: Number, height :: Number}
            | Finalize
data Query a =
      Update SomeScatterPlot a
    | Highlight String a
    | Unhighlight a
    | Export String (Boolean -> a)

component
    :: forall q i o m. MonadEffect m
    => { width :: Number, height :: Number }
    -> H.Component HH.HTML Query i o m
component dim =
  H.mkComponent
  { initialState
  , render: renderH
  , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , handleQuery  = handleQuery
        , initialize   = Just (Initialize dim)
        , finalize     = Just Finalize
        }
  }

initialState :: forall i. i -> State
initialState _ =
    { chart: Nothing
    , interactor: Nothing
    }

renderH :: forall m. State -> H.ComponentHTML Action () m
renderH _ = HH.div [
              HP.ref scatterRef
            ]
            []

handleAction
    :: forall m o. MonadEffect m
    => Action
    -> H.HalogenM State Action () o m Unit
handleAction = case _ of
    Initialize dim -> do
      r     <- H.getRef scatterRef
      for_ r $ \ref -> do
        chart <- liftEffect $ mkSvg ref dim
        H.modify_ (\_ -> { chart: Just chart, interactor: Nothing })
    Finalize -> do
      s <- H.get
      case s.chart of
        Nothing -> pure unit
        Just c  -> liftEffect $ clearSvg c

handleQuery
    :: forall m a o. MonadEffect m
    => Query a
    -> H.HalogenM State Action () o m (Maybe a)
handleQuery = case _ of
    Update update next -> do
      s <- H.get
      case s.chart of
        Nothing -> pure unit
        Just z  -> do
          ixor <- liftEffect $ update (\a b c d -> drawData_ a b c d z)
          H.modify_ (_ { interactor = Just ixor })
      pure (Just next)
    Highlight lbl next -> do
      s <- H.get
      liftEffect case s.interactor of
        Nothing -> pure unit
        Just z  -> highlight z (Just lbl)
      pure (Just next)
    Unhighlight next -> do
      s <- H.get
      liftEffect case s.interactor of
        Nothing -> pure unit
        Just z  -> highlight z Nothing
      pure (Just next)
    Export fn next -> do
      s <- H.get
      map (Just <<< next) $ liftEffect case s.interactor of
        Nothing -> pure false
        Just z  -> saveAsPng z fn

scatterRef ∷ H.RefLabel
scatterRef = H.RefLabel "scatter-plot"

