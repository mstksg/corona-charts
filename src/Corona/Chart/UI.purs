
module Corona.Chart.Component where

import Prelude

import Corona.Chart
import Corona.JHU
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Query.EventSource as ES
import Halogen.Scatter as D3
import D3.Scatter as D3
import Corona.Chart
import Data.Set (Set)
import Data.Set as Set


type AxisState a =
    { projection :: Projection a
    , scale      :: D3.Scale a
    }

type State a b =
    { xAxis     :: AxisState a
    , yAxis     :: AxisState b
    , countries :: Set Country
    }

type SomeState = forall r. (forall a b. State a b -> r) -> r

-- data Action =
--         AddCountries (Set Country)
--       | RemoveCountry Country
--       | RemoveAllCountries

    


-- component :: forall f i o m. MonadAff m => H.Component HH.HTML f i o m
-- component =
--   H.mkComponent
--     { initialState
--     , render
--     , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
--     }


