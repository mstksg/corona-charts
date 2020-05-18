
module Data.ModifiedJulianDay where

import Data.Array as A
import Data.Boolean
import Data.Date as D
import Data.DateTime as DT
import Data.Enum
import Data.Function.Uncurried
import Data.Int
import Data.JSDate (JSDate)
import Data.JSDate as JSDate
import Data.Lens.Prism
import Data.List as L
import Data.Maybe
import Data.Newtype
import Data.Time as T
import Data.Time.Duration as D
import Prelude

newtype Day = Day Int

derive instance eqDay :: Eq Day
derive instance ordDay :: Ord Day
derive instance ntDay :: Newtype Day _

instance showDay :: Show Day where
    show (Day x) = "fromModifiedJulianDay " <> show x

fromModifiedJulianDay :: Int -> Day
fromModifiedJulianDay = Day

addDays :: Int -> Day -> Day
addDays x (Day y) = Day (x + y)

-- | diffDays x y = x - y
diffDays :: Day -> Day -> Int
diffDays (Day x) (Day y) = x - y

fromISO8601 :: String -> Maybe Day
fromISO8601 = fromJSDate <=< runFn3 _parseIso Just (const Nothing)

toISO8601 :: Day -> String
toISO8601 = _toIso <<< toJSDate

iso8601 :: Prism' String Day
iso8601 = prism' toISO8601 fromISO8601

fromGregorian :: Int -> D.Month -> Int -> Day
fromGregorian y m d = fromDate $ D.canonicalDate
    (toEnumWithDefaults bottom top y)
    m
    (toEnumWithDefaults bottom top d)

date0 :: D.Date
date0 = D.canonicalDate
    (toEnumWithDefaults bottom top 1858)
    D.November
    (toEnumWithDefaults bottom top 17)

toDate :: Day -> D.Date
toDate (Day n) = fromMaybe date0 (D.adjust (D.Days (toNumber n)) date0)

toJSDate :: Day -> JSDate
toJSDate = JSDate.fromDateTime <<< flip DT.DateTime bottom <<< toDate

fromDate :: D.Date -> Day
fromDate d = Day (round n)
  where
    D.Days n = D.diff d date0

fromJSDate :: JSDate -> Maybe Day
fromJSDate = map fromDate <<< JSDate.toDate

foreign import _parseIso :: forall r. Fn3 (JSDate -> r) (Unit -> r) String r
foreign import _toIso :: JSDate -> String
