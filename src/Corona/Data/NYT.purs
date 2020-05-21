
module Corona.Data.NYT where

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

type State = String

dataUrl :: String
dataUrl = "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv"

-- fetchCoronaData :: Aff (Either String CoronaData)
-- fetchCoronaData = runExceptT do
--     confirmed <- ExceptT $ fetchData confirmedUrl
--     deaths    <- ExceptT $ fetchData deathsUrl
--     recovered <- ExceptT $ fetchData recoveredUrl
--     unless (confirmed.start == deaths.start && deaths.start == recovered.start) $
--       throwError "non-matching dates for time serieses"
--     let newCounts = forWithIndex confirmed.counts $ \k c -> do
--           d <- O.lookup k deaths.counts
--           r <- O.lookup k recovered.counts
--           pure $
--             A.zipWith ($) (A.zipWith (\c' d' r' ->
--                     { confirmed : c'
--                     , deaths: d'
--                     , recovered: r'
--                     }
--                 ) c d) r
--     case newCounts of
--       Nothing -> throwError "missing data"
--       Just c  -> pure
--         { start: confirmed.start
--         , counts: c
--         }

-- type CoronaData =
--     { start  :: Day
--     , counts :: O.Object (Array (Counts Int))
--     }

-- type Counts a =
--     { confirmed :: a
--     , deaths    :: a
--     , recovered :: a
--     }

