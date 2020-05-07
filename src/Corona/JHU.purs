
module Corona.JHU where

import Prelude

import Affjax as AX
import Data.JSDate (JSDate)
import Data.Tuple
import Effect.Class
import Data.JSDate as JSDate
import Effect
import Data.Traversable
import Data.Functor
import Affjax.ResponseFormat as ResponseFormat
import Data.Int.Parse
import Data.Date
import Data.Either
import Data.HTTP.Method (Method(..))
import Data.Array as A
import Data.Map as M
import Foreign.Object as O
import Data.Int.Parse
import Data.Maybe
import Foreign.Papa
import Effect.Aff

type Country = String

type CoronaData =
    { dates  :: Array JSDate
    , counts :: O.Object CoronaCounts
    }

type CoronaCounts = { confirmed :: Array Int }

addCounts :: CoronaCounts -> CoronaCounts -> CoronaCounts
addCounts x y = { confirmed: A.zipWith (+) x.confirmed y.confirmed }

confirmedUrl :: String
confirmedUrl = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"

buildData :: Array (Array String) -> Effect (Maybe CoronaData)
buildData = traverse process <<< A.uncons
  where
    process :: { head :: Array String, tail :: Array (Array String) } -> Effect CoronaData
    process ht = do
      dates <- traverse JSDate.parse (A.drop 4 (ht.head))
      let vals = A.mapMaybe go ht.tail
          counts = O.fromFoldableWith addCounts vals
      pure { dates, counts }
    go :: Array Country -> Maybe (Tuple Country CoronaCounts)
    go val = do
       country <- val A.!! 1
       valnum  <- traverse (flip parseInt (toRadix 10)) (A.drop 4 val)
       pure (Tuple country { confirmed: valnum })


fetchData :: Aff (Either String CoronaData)
fetchData = do
    result <- AX.request (AX.defaultRequest
      { url = confirmedUrl
      , method = Left GET
      , responseFormat = ResponseFormat.string }
      )
    case result of
      Left err       -> pure (Left (AX.printError err))
      Right response -> liftEffect do
         dat <- buildData (parseCSV response.body).data
         pure case dat of
           Nothing -> Left "data not accumulated"
           Just d  -> Right d

lookupData
    :: CoronaData
    -> String
    -> Array ({ date :: JSDate, confirmed :: Int })
lookupData cd c = case O.lookup c cd.counts of
    Nothing -> []
    Just cs -> A.zipWith (\d c -> { date: d, confirmed: c }) cd.dates cs.confirmed

    
-- type CoronaData =
--     { dates  :: Array JSDate
--     , counts :: O.Object CoronaCounts
--     }

