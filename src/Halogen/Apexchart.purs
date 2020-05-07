
module Halogen.Apexchart where

import Prelude

import Apexcharts (createChart, render, Apexchart, Apexoptions)
import Apexcharts.Chart as C
import Apexcharts.Chart.Zoom as Z
import Data.Options
import Apexcharts.Common as CC
import Apexcharts.Series as SE
import Apexcharts.Xaxis as X
import Apexcharts.Yaxis as Y
import Effect.Aff.Class (class MonadAff, liftAff)
import Halogen as H
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Effect.Class
import Data.Maybe
import Halogen.Query.EventSource as ES

type State = { chart :: Maybe Apexchart }
type Action = { update :: Options Apexoptions }

component :: forall q i o m. MonadAff m => String -> H.Component HH.HTML q i o m
component d =
  H.mkComponent
  { initialState
  , render: renderH d
  , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction d
        }
  }

initialState :: forall i. i -> State
initialState _ = { chart: Nothing }

renderH :: forall m. String -> State -> H.ComponentHTML Action () m
renderH d _ = HH.div [ HP.ref (H.RefLabel d) ] []

handleAction :: forall m o. MonadAff m => String -> Action -> H.HalogenM State Action () o m Unit
handleAction d a = do
    H.modify_ (\_ -> { chart: Just c })
    liftEffect $ render c
  where
    c = createChart d (a.update)
