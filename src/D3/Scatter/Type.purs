
module D3.Scatter.Type where

import Prelude

import Data.JSDate (JSDate)
import Data.BSum
import Type.Handler
import Data.Semiring
import Data.Either
import Data.JSDate as JSDate
import Data.Maybe
import Data.ModifiedJulianDay (Day)
import Data.Tuple
import Effect
import Foreign.Object as O
import Type.Equality
import Type.Equiv
import Web.DOM.Element (Element)

-- | interval, days since
newtype Days = Days Day

newtype Percent  = Percent Number

unPercent :: Percent -> Number
unPercent (Percent n) = n

instance percentSemiring :: Semiring Percent where
    add (Percent x) (Percent y) = Percent (x + y)
    zero = Percent zero
    mul (Percent x) (Percent y) = Percent (x * y)
    one = Percent one
instance percentRing :: Ring Percent where
    sub (Percent x) (Percent y) = Percent (x - y)

data SType a =
        SDay     (a ~ Day)
      | SDays    (a ~ Days)
      | SInt     (a ~ Int)
      | SNumber  (a ~ Number)
      | SPercent (a ~ Percent)

sDay :: SType Day
sDay = SDay refl
sDays :: SType Days
sDays = SDays refl
sInt :: SType Int
sInt = SInt refl
sNumber :: SType Number
sNumber = SNumber refl
sPercent :: SType Percent
sPercent = SPercent refl

class STypeable a where
    sType :: SType a
instance stDay :: STypeable Day where
    sType = sDay
instance stDays :: STypeable Days where
    sType = sDays
instance stInt :: STypeable Int where
    sType = sInt
instance stNumber :: STypeable Number where
    sType = sNumber
instance stPercent :: STypeable Percent where
    sType = sPercent

-- | subset of numeric stypes
data NType a =
        NInt     (a ~ Int)
      | NNumber  (a ~ Number)
      | NPercent (a ~ Percent)

numericSType :: forall a. NType a -> SType a
numericSType = case _ of
    NInt     refl -> SInt refl
    NNumber  refl -> SNumber refl
    NPercent refl -> SPercent refl

nInt :: NType Int
nInt = NInt refl
nNumber :: NType Number
nNumber = NNumber refl
nPercent :: NType Percent
nPercent = NPercent refl

class NTypeable a where
    nType :: NType a
instance ntInt :: NTypeable Int where
    nType = nInt
instance ntNumber :: NTypeable Number where
    nType = nNumber
instance ntPercent :: NTypeable Percent where
    nType = nPercent

-- numericRing :: forall a. NType a -> (forall r. (Ring a => r) -> r)
-- numericRing = case _ of
--     NInt     refl -> \x -> withEquiv refl x
--     NNumber  refl -> \x -> withEquiv refl x
--     NPercent refl -> \x -> withEquiv refl x

type Point a b = { x :: a, y :: b }

type SeriesData a b =
    { name   :: String
    , values :: Array (Point a b)
    }

infixr 1 type Either as ||

data Scale a = Date   (a ~ Day)
             | Linear (NType a)
             | Log    (NType a)

sDate :: Scale Day
sDate = Date refl

sLinear :: forall a. NTypeable a => Scale a
sLinear = Linear nType

sLog :: forall a. NTypeable a => Scale a
sLog = Log nType

type AxisConf a = { scale :: Scale a, label :: String }

type ScatterPlot a b =
        { xAxis  :: AxisConf a
        , yAxis  :: AxisConf b
        , series :: Array (SeriesData a b)
        }

type SomeScatterPlot =
        forall r.
          (forall a b. SType a
                  -> SType b
                  -> ScatterPlot a b
                  -> r
          )
          -> r

newtype OnScale a = OnScale
    (forall r.
        { date   :: a ~ Day -> r
        , linear :: NType a -> r
        , log    :: NType a -> r
        } -> r
    )

instance handleScale :: Handle (Scale a) (OnScale a) where
    handle   = handle1
    unHandle = unHandle1
instance handle1Scale :: Handle1 Scale OnScale where
    handle1 = case _ of
      Date   refl -> OnScale (\h -> h.date   refl)
      Linear nt   -> OnScale (\h -> h.linear nt  )
      Log    nt   -> OnScale (\h -> h.log    nt  )
    unHandle1 (OnScale f) = f { date: Date, linear: Linear, log: Log }

newtype OnSType a = OnSType (forall r.
      { day     :: a ~ Day    -> r
      , days    :: a ~ Days   -> r
      , int     :: a ~ Int    -> r
      , number  :: a ~ Number -> r
      , percent :: a ~ Percent -> r
      } -> r
    )

instance handleSType :: Handle (SType a) (OnSType a) where
    handle   = handle1
    unHandle = unHandle1
instance handle1SType :: Handle1 SType OnSType where
    handle1 = case _ of
      SDay     refl -> OnSType (\h -> h.day    refl)
      SDays    refl -> OnSType (\h -> h.days   refl)
      SInt     refl -> OnSType (\h -> h.int   refl)
      SNumber  refl -> OnSType (\h -> h.number refl)
      SPercent refl -> OnSType (\h -> h.percent refl)
    unHandle1 (OnSType f) = f
      { day:     SDay
      , days:    SDays
      , int:    SInt
      , number:  SNumber
      , percent: SPercent
      }

