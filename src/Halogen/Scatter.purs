
module Halogen.Scatter where

import Prelude

import Data.Options
import Effect.Aff.Class (class MonadAff, liftAff)
import Halogen as H
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Effect.Class
import Data.Maybe
import Halogen.Query.EventSource as ES
import D3.Scatter

type State  = { chart :: Maybe D3Scatter }
data Action = Initialize
            | Finalize
newtype Query a = Query { update :: SomeScatterPlot, next :: a }

component
    :: forall q i o m. MonadAff m
     => String
     -> H.Component HH.HTML Query i o m
component d =
  H.mkComponent
  { initialState
  , render: renderH d
  , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction d
        , handleQuery  = handleQuery
        , initialize   = Just Initialize
        , finalize     = Just Finalize
        }
  }

initialState :: forall i. i -> State
initialState _ = { chart: Nothing }

renderH :: forall m. String -> State -> H.ComponentHTML Action () m
renderH d _ = HH.div [ HP.ref (H.RefLabel d) ] []

handleAction
    :: forall m o. MonadAff m
    => String
    -> Action
    -> H.HalogenM State Action () o m Unit
handleAction d = case _ of
    Initialize -> do
      chart <- liftEffect $ mkSvg d
      H.modify_ (\_ -> { chart: Just chart })
    Finalize -> do
      s <- H.get
      case s.chart of
        Nothing -> pure unit
        Just c  -> liftEffect $ clearSvg c

handleQuery
    :: forall m a o. MonadAff m
    => Query a
    -> H.HalogenM State Action () o m (Maybe a)
handleQuery (Query q) = do
    s <- H.get
    liftEffect case s.chart of
      Nothing -> pure unit
      Just c  -> q.update (drawData c)
    pure (Just q.next)
