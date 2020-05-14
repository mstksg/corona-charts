
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

type State  = { chart :: Maybe D3Scatter }
data Action = Initialize {width :: Number, height :: Number}
            | Finalize
newtype Query a = Query { update :: SomeScatterPlot, next :: a }

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
initialState _ = { chart: Nothing }

renderH :: forall m. State -> H.ComponentHTML Action () m
renderH _ = HH.div [
              HP.ref scatterRef
            -- , HU.classProp "svg-container"
            -- , HC.style $ do
            --     CSS.height (CSS.px (toNumber 600))
            --     CSS.width (CSS.px (toNumber 1000))
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
      Just z  -> q.update (\a b c -> drawData_ a b c z)
    pure (Just q.next)

scatterRef ∷ H.RefLabel
scatterRef = H.RefLabel "scatter-plot"

