
module Data.ModifiedJulianDay where

import Prelude
import Data.Maybe
import Data.Int
import Data.Boolean
import Data.JSDate (JSDate)
import Data.Array as A
import Data.List as L
import Data.JSDate as JSDate

newtype Day = Day Int

derive instance eqDay :: Eq Day
derive instance ordDay :: Ord Day

addDays :: Int -> Day -> Day
addDays x (Day y) = Day (x + y)

toJSDate :: Day -> JSDate
toJSDate d = JSDate.jsdate
    ({ millisecond : toNumber 0
     , second : toNumber 0
     , minute : toNumber 0
     , hour : toNumber 0
     , day : toNumber greg.day
     , month : toNumber greg.month
     , year : toNumber greg.year
     }
    )
  where
    greg = toGregorian d

toGregorian :: Day -> { year :: Int, month :: Int, day :: Int }
toGregorian d = { year: od.year
                , month: md.month
                , day: md.day
                }
  where
    od = toOrdinalDate d
    md = dayOfYearToMonthAndDay (isLeapYear od.year) od.yearday


toOrdinalDate :: Day -> { year :: Int, yearday :: Int }
toOrdinalDate (Day mjd) = { year, yearday }
  where
    a = mjd + 678575
    quadcent = div a 146097
    b = mod a 146097
    cent = min (div b 36524) 3
    c = b - (cent * 36524)
    quad = div c 1461
    d = mod c 1461
    y = min (div d 365) 3
    yearday = d - (y * 365) + 1
    year = quadcent * 400 + cent * 100 + quad * 4 + y + 1

isLeapYear :: Int -> Boolean
isLeapYear year = (mod year 4 == 0) && ((mod year 400 == 0) || not (mod year 100 == 0))

dayOfYearToMonthAndDay :: Boolean -> Int -> { month :: Int, day :: Int }
dayOfYearToMonthAndDay isLeap yd =
    findMonthDay
        (monthLengths isLeap)
        (clip
             1
             (if isLeap
                  then 366
                  else 365)
             yd)

findMonthDay :: L.List Int -> Int -> { month :: Int, day :: Int }
findMonthDay (L.Cons n ns) yd
  | yd > n = (\md -> md { month = md.month + 1 } ) (findMonthDay ns (yd - n))
findMonthDay _ yd = { month: 1, day: yd }

-- | The length of a given month in the Gregorian or Julian calendars.
-- First arg is leap year flag.
monthLength :: Boolean -> Int -> Int
monthLength isLeap month' = monthLength' isLeap (clip 1 12 month')

monthLength' :: Boolean -> Int -> Int
monthLength' isLeap month' = case L.index (monthLengths isLeap) (month' - 1) of
    Nothing -> -1
    Just x  -> x


monthLengths :: Boolean -> L.List Int
monthLengths isleap = A.toUnfoldable
    [ 31
    , if isleap
          then 29
          else 28
    , 31
    , 30
    , 31
    , 30
    , 31
    , 31
    , 30
    , 31
    , 30
    , 31
    ]

clip :: forall t. Ord t => t -> t -> t -> t
clip a _ x
    | x < a = a
clip _ b x
    | x > b = b
clip _ _ x = x
