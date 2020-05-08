
module Corona.JHU where

import Prelude

import Affjax as AX
import Affjax.ResponseFormat as ResponseFormat
import Control.Alternative
import Control.Monad.Except
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

type Country = String

type CoronaData =
    { start  :: Day
    , counts :: O.Object (Array (Counts Number))
    }

type Counts a =
    { confirmed :: a
    , deaths    :: a
    , recovered :: a
    }

confirmedUrl :: String
confirmedUrl = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"

deathsUrl :: String
deathsUrl = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv"

recoveredUrl :: String
recoveredUrl = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv"


fetchCoronaData :: Aff (Either String CoronaData)
fetchCoronaData = runExceptT do
    confirmed <- ExceptT $ fetchData confirmedUrl
    deaths    <- ExceptT $ fetchData deathsUrl
    recovered <- ExceptT $ fetchData recoveredUrl
    unless (confirmed.start == deaths.start && deaths.start == recovered.start) $
      throwError "non-matching dates for time serieses"
    let newCounts = forWithIndex confirmed.counts $ \k c -> do
          d <- O.lookup k deaths.counts
          r <- O.lookup k recovered.counts
          pure $
            A.zipWith ($) (A.zipWith (\c' d' r' ->
                    { confirmed : c'
                    , deaths: d'
                    , recovered: r'
                    }
                ) c d) r
    case newCounts of
      Nothing -> throwError "missing data"
      Just c  -> pure
        { start: confirmed.start
        , counts: c
        }

type CSVData =
    { start  :: Day
    , counts :: O.Object (Array Number)
    }


buildData :: Array (Array String) -> Effect (Maybe CSVData)
buildData xs = case A.uncons xs of
                Nothing -> pure Nothing
                Just ht -> process ht
  where
    process :: { head :: Array String, tail :: Array (Array String) } -> Effect (Maybe CSVData)
    process ht = runMaybeT $ do
      d0    <- maybe empty pure $ A.index ht.head 4
      start <- MaybeT $ MJD.fromJSDate <$> JSDate.parse d0
      let vals = A.mapMaybe go ht.tail
          counts = O.fromFoldableWith (A.zipWith (+)) vals
      pure { start, counts }
    go :: Array Country -> Maybe (Tuple Country (Array Number))
    go val = do
       country <- val A.!! 1
       valnum  <- traverse (map toNumber <<< flip parseInt (toRadix 10)) (A.drop 4 val)
       pure (Tuple country valnum)

fetchData :: String -> Aff (Either String CSVData)
fetchData url = do
    result <- AX.request (AX.defaultRequest
      { url = url
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

-- lookupData
--     :: CoronaData
--     -> String
--     -> Array ({ date :: JSDate, confirmed :: Int })
-- lookupData cd c = case O.lookup c cd.counts of
--     Nothing -> []
--     Just cs -> A.zipWith (\d c -> { date: d, confirmed: c }) cd.dates cs.confirmed

    
-- type CoronaData =
--     { dates  :: Array JSDate
--     , counts :: O.Object Counts
--     }

