
module Halogen.Scatter where

import Prelude

import D3.Scatter
import D3.Scatter.Type
import Data.Foldable
import Data.Int
import Data.Maybe
import CSS.Size as CSS
import Data.Options
import CSS.Geometry as CSS
import Halogen.HTML.CSS as HC
import Data.Traversable
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Query.EventSource as ES

type State  = { chart :: Maybe D3Scatter }
data Action = Initialize
            | Finalize
newtype Query a = Query { update :: SomeScatterPlot, next :: a }

component
    :: forall q i o m. MonadEffect m
    => H.Component HH.HTML Query i o m
component =
  H.mkComponent
  { initialState
  , render: renderH
  , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , handleQuery  = handleQuery
        , initialize   = Just Initialize
        , finalize     = Just Finalize
        }
  }

initialState :: forall i. i -> State
initialState _ = { chart: Nothing }

renderH :: forall m. State -> H.ComponentHTML Action () m
renderH _ = HH.div [
              HP.ref scatterRef
            , HC.style $ do
                CSS.height (CSS.px (toNumber 600))
                CSS.width (CSS.px (toNumber 1000))
            ]
            []

handleAction
    :: forall m o. MonadEffect m
    => Action
    -> H.HalogenM State Action () o m Unit
handleAction = case _ of
    Initialize -> do
      r     <- H.getRef scatterRef
      for_ r $ \ref -> do
        chart <- liftEffect $ mkSvg ref
        H.modify_ (\_ -> { chart: Just chart })
    Finalize -> do
      s <- H.get
      case s.chart of
        Nothing -> pure unit
        Just c  -> liftEffect $ clearSvg c

handleQuery
    :: forall m a o. MonadEffect m
    => Query a
    -> H.HalogenM State Action () o m (Maybe a)
handleQuery (Query q) = do
    s <- H.get
    liftEffect case s.chart of
      Nothing -> pure unit
      Just c  -> q.update (\a b -> drawData_ a b c)
    pure (Just q.next)

scatterRef ∷ H.RefLabel
scatterRef = H.RefLabel "scatter-plot"

