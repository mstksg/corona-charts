
module Corona.Data.JHU where

import Prelude

import Affjax as AX
import Affjax.ResponseFormat as ResponseFormat
import Control.Alternative
import Control.Monad.Except
import Control.Monad.Maybe.Trans
import Control.MonadZero as MZ
import Corona.Data.Type
import Data.Array as A
import Data.Date
import Data.Either
import Data.Functor
import Data.FunctorWithIndex
import Data.HTTP.Method (Method(..))
import Data.Int
import Data.Int.Parse
import Data.JSDate (JSDate)
import Data.JSDate as JSDate
import Data.Map as M
import Data.Maybe
import Data.ModifiedJulianDay (Day, fromJSDate)
import Data.ModifiedJulianDay as MJD
import Data.Number as Number
import Data.Traversable
import Data.TraversableWithIndex
import Data.Tuple
import Debug
import Effect
import Effect.Aff
import Effect.Class
import Foreign.Object as O
import Foreign.Papa

type Country = String

confirmedUrl :: String
confirmedUrl = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"

deathsUrl :: String
deathsUrl = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv"

recoveredUrl :: String
recoveredUrl = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv"

popUrl :: String
popUrl = "https://raw.githubusercontent.com/mstksg/corona-charts/master/public/data/country_pop.csv"


fetchCoronaData :: Aff (Either String CoronaData)
fetchCoronaData = runExceptT do
    confirmed <- ExceptT $ fetchData confirmedUrl
    deaths    <- ExceptT $ fetchData deathsUrl
    recovered <- ExceptT $ fetchData recoveredUrl
    pops      <- ExceptT $ fetchPops popUrl
    unless (confirmed.start == deaths.start && deaths.start == recovered.start) $
      throwError "non-matching dates for time serieses"
    let newCounts = O.filter noEmpty <<< flip mapWithIndex confirmed.counts $ \k c ->
          let d = case O.lookup k deaths.counts of
                    Nothing -> trace ("bad d: " <> k) \_ -> []
                    Just q  -> q
              r = case O.lookup k recovered.counts of
                    Nothing -> trace ("bad r: " <> k) \_ -> []
                    Just q  -> q
          in  { confirmed: c
              , deaths: d
              , recovered: r
              , active: A.zipWith (-) c (A.zipWith (+) d r)
              }
        res = { start: confirmed.start
              , dat: combineDat pops newCounts
              }
    trace res (\_ -> pure res)
  where
      noEmpty o = not (A.null o.active)

type CSVData =
    { start  :: Day
    , counts :: O.Object (Array Int)
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
    go :: Array Country -> Maybe (Tuple Country (Array Int))
    go val = do
       country <- filterCountry <$> (val A.!! 1)
       valnum  <- traverse (flip parseInt (toRadix 10)) (A.drop 4 val)
       pure (Tuple country valnum)

filterCountry :: Country -> Country
filterCountry c = fromMaybe c $ O.lookup c countryFilters

countryFilters :: O.Object Country
countryFilters = O.fromFoldable [
    Tuple "US"           "United States"
  , Tuple "Korea, South" "South Korea"
  , Tuple "Taiwan*"      "Taiwan"
  , Tuple "Burma"        "Myanmar"
  , Tuple "Bosnia and Herzegovina" "Bosnia-Herzegovina"
  , Tuple "Palestinian Territory" "West Bank and Gaza" -- arbitrarily use WHO designation
  , Tuple "eSwatini" "Eswatini"
  , Tuple "Cape Verde" "Cabo Verde"
  , Tuple "St. Vincent and the Grenadines" "Saint Vincent and the Grenadines"
  , Tuple "St. Kitts-Nevis" "Saint Kitts and Nevis"
  , Tuple "Congo" "Congo (Brazzaville)"
  , Tuple "Congo, Dem. Rep." "Congo (Kinshasa)"
  ]

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

fetchPops :: String -> Aff (Either String (O.Object Int))
fetchPops url = do
    result <- AX.request (AX.defaultRequest
      { url = url
      , method = Left GET
      , responseFormat = ResponseFormat.string }
      )
    pure  case result of
      Left err       -> Left (AX.printError err)
      Right response -> Right $ buildPops (parseCSV response.body).data

buildPops :: Array (Array String) -> O.Object Int
buildPops = O.union corePops
        <<< O.fromFoldable
        <<< A.mapMaybe go
        <<< A.drop 5
  where
    go xs = do
      tp  <- A.index xs 2
      MZ.guard $ tp == "Country"
      cty <- A.index xs 1
      pop <- Number.fromString =<< A.index xs 4
      pure $ Tuple (filterCountry cty) (round (pop * 1e6))

corePops :: O.Object Int
corePops = O.fromFoldable [
    Tuple "Holy See" 825              -- https://en.wikipedia.org/wiki/Vatican_City
  , Tuple "Diamond Princess" 3711     -- https://en.wikipedia.org/wiki/Diamond_Princess_(ship)
  , Tuple "MS Zaandam" 1829           -- https://en.wikipedia.org/wiki/MS_Zaandam
  ]
