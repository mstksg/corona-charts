
module D3.Scatter where

import Prelude

import Effect
import Type.Equality
import Data.JSDate (JSDate)
import Data.JSDate as JSDate
import Foreign.Object as O
import Data.Maybe
import Data.Tuple

foreign import data D3Scatter :: Type

foreign import mkSvg    :: String -> Effect D3Scatter
foreign import clearSvg :: D3Scatter -> Effect Unit

type Point a b = { x :: a, y :: b }

type SeriesData a b =
    { name   :: String
    , values :: Array (Point a b)
    }

type Equiv a b = forall p. (TypeEquals a b => p) -> p

refl :: forall a. Equiv a a
refl x = x

data Scale a = Date   (Equiv a JSDate)
             | Linear (Equiv a Number)
             | Log    (Equiv a Number)

sDate :: Scale JSDate
sDate = Date refl

sLinear :: Scale Number
sLinear = Linear refl

sLog :: Scale Number
sLog = Log refl

type AxisConf a = { scale :: Scale a, label :: String }

type ScatterPlot a b =
        { xAxis  :: AxisConf a
        , yAxis  :: AxisConf b
        , series :: Array (SeriesData a b)
        }

type SomeScatterPlot = forall r. (forall a b. ScatterPlot a b -> r) -> r

foreign import _drawData :: forall a b. ForAny ScaleHandler -> D3Scatter -> ScatterPlot a b -> Effect Unit

drawData :: forall a b. D3Scatter -> ScatterPlot a b -> Effect Unit
drawData = _drawData onScale

type OnScale a = forall r.
        { date   :: Equiv a JSDate -> r
        , linear :: Equiv a Number -> r
        , log    :: Equiv a Number -> r
        } -> r

onScale :: forall a. Scale a -> OnScale a
onScale = case _ of
    Date   refl -> \h -> h.date   refl
    Linear refl -> \h -> h.linear refl
    Log    refl -> \h -> h.log    refl

type ForAny f = forall a. f a

type ScaleHandler a = Scale a -> OnScale a
