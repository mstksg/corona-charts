
module Data.ModifiedJulianDay where

import Prelude
import Data.Maybe
import Data.Newtype
import Data.Time as T
import Data.Time.Duration as D
import Data.Enum
import Data.DateTime as DT
import Data.Int
import Data.Boolean
import Data.JSDate (JSDate)
import Data.Array as A
import Data.Date as D
import Data.List as L
import Data.JSDate as JSDate

newtype Day = Day Int

derive instance eqDay :: Eq Day
derive instance ordDay :: Ord Day
derive instance ntDay :: Newtype Day _

instance showDay :: Show Day where
    show (Day x) = "Day " <> show x

addDays :: Int -> Day -> Day
addDays x (Day y) = Day (x + y)

-- | diffDays x y = x - y
diffDays :: Day -> Day -> Int
diffDays (Day x) (Day y) = x - y


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
