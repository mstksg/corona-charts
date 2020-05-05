
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

-- type Region = { country  :: String
--               , province :: Maybe String
--               }

type Data = O.Object (M.Map JSDate Int)

confirmedUrl :: String
confirmedUrl = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"

buildData :: Array (Array String) -> Effect (Maybe Data)
buildData = traverse process <<< A.uncons
  where
    process :: { head :: Array String, tail :: Array (Array String) } -> Effect Data
    process ht = do
      dates <- traverse JSDate.parse (A.drop 4 (ht.head))
      let vals = A.mapMaybe go ht.tail
          bigObj = O.fromFoldableWith (A.zipWith (+)) vals
          dataRaw = map (M.filter (_ > 0) <<< M.fromFoldable <<< A.zip dates) bigObj
      pure dataRaw
    go :: Array String -> Maybe (Tuple String (Array Int))
    go val = do
       country <- val A.!! 1
       valnum  <- traverse (flip parseInt (toRadix 10)) (A.drop 4 val)
       pure (Tuple country valnum)
    -- merge :: O.Object Int -> O.Object Int -> O.Object Int


fetchData :: Aff (Either String Data)
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
