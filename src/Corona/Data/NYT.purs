
module Corona.Data.NYT where

import Prelude

import Affjax as AX
import Affjax.ResponseFormat as ResponseFormat
import Control.Alternative
import Control.Monad.Except
import Control.Monad.Maybe.Trans
import Control.MonadZero as MZ
import Corona.Data.Type
import Data.Array as A
import Data.Bifunctor
import Data.Date
import Data.Either
import Data.Functor
import Data.HTTP.Method (Method(..))
import Data.Int
import Data.Int.Parse
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

popUrl :: String
popUrl = "https://raw.githubusercontent.com/mstksg/corona-charts/master/public/data/us_pop.csv"

fetchCoronaData :: Aff (Either String CoronaData)
fetchCoronaData = runExceptT do
    response <- ExceptT $ lmap AX.printError <$> AX.request (AX.defaultRequest
      { url = dataUrl
      , method = Left GET
      , responseFormat = ResponseFormat.string }
      )
    popsData <- ExceptT $ lmap AX.printError <$> AX.request (AX.defaultRequest
      { url = popUrl
      , method = Left GET
      , responseFormat = ResponseFormat.string }
      )
    let rows = A.drop 1 (parseCSV response.body).data
        dat  = buildCorona rows
        counts = dat.counts <#> \d ->
          { confirmed: d.confirmed
          , deaths: d.deaths
          , recovered: []
          }
        pops = buildPops (parseCSV popsData.body).data
    start <- maybe (throwError ("bad date: " <> show dat.start)) pure $
                MJD.fromJSDate dat.start
    pure
      { start
      , dat: combineDat pops counts
      }

type CSVData =
    { start:: JSDate
    , counts:: O.Object { confirmed:: Array Int, deaths:: Array Int }
    }

foreign import buildCorona :: Array (Array String) -> CSVData

buildPops :: Array (Array String) -> O.Object Int
buildPops = O.union corePops
        <<< O.fromFoldable
        <<< A.mapMaybe go
        <<< A.drop 1
  where
    go xs = do
      level <- A.index xs 0
      MZ.guard $ level == "40"
      st  <- A.index xs 4
      pop <- flip parseInt (toRadix 10) =<< A.index xs 5
      pure $ Tuple st pop

corePops :: O.Object Int
corePops = O.fromFoldable [
    Tuple "Virgin Islands" 104437               -- https://www.worldometers.info/world-population/united-states-virgin-islands-population/
  , Tuple "Guam" 168661                         -- https://www.worldometers.info/world-population/guam-population/
  , Tuple "Northern Mariana Islands" 168661     -- https://www.worldometers.info/world-population/northern-mariana-islands-population/
  ]

