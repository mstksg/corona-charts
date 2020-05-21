
module Corona.Data.NYT where

import Prelude

import Affjax as AX
import Affjax.ResponseFormat as ResponseFormat
import Control.Alternative
import Data.Bifunctor
import Control.Monad.Except
import Corona.Data.Type
import Control.Monad.Maybe.Trans
import Data.Array as A
import Data.Date
import Data.Either
import Data.Functor
import Data.HTTP.Method (Method(..))
import Data.Int.Parse
import Data.Int
import Data.JSDate (JSDate)
import Data.JSDate as JSDate
import Data.Map as M
import Data.Maybe
import Data.ModifiedJulianDay (Day, fromJSDate)
import Data.ModifiedJulianDay as MJD
import Data.Traversable
import Data.TraversableWithIndex
import Data.Tuple
import Effect
import Effect.Aff
import Effect.Class
import Foreign.Object as O
import Foreign.Papa
import Undefined

type State = String

dataUrl :: String
dataUrl = "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv"

fetchCoronaData :: Aff (Either String CoronaData)
fetchCoronaData = runExceptT do
    response <- ExceptT $ lmap AX.printError <$> AX.request (AX.defaultRequest
      { url = dataUrl
      , method = Left GET
      , responseFormat = ResponseFormat.string }
      )
    let rows = A.drop 1 (parseCSV response.body).data
        dat  = buildCorona rows
    start <- maybe (throwError ("bad date: " <> show dat.start)) pure $
                MJD.fromJSDate dat.start
    pure
      { start
      , counts: dat.counts <#> \d ->
          { confirmed: d.confirmed
          , deaths: d.deaths
          , recovered: []
          }
      }

type CSVData =
    { start:: JSDate
    , counts:: O.Object { confirmed:: Array Int, deaths:: Array Int }
    }

foreign import buildCorona :: Array (Array String) -> CSVData
