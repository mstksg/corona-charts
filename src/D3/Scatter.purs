
module D3.Scatter where

import Prelude

import Effect
import Data.JSDate (JSDate)
import Data.JSDate as JSDate
import Foreign.Object as O
import Data.Tuple

data D3Scatter

foreign import mkSvg    :: String -> Effect D3Scatter

type Point a b = { x :: a, y :: b }

type SeriesData a b =
    { name   :: String
    , values :: Array (Point a b)
    }

data NumberScale = Linear | Log

data Scale = Date
           | Count NumberScale

type AxisConf = { scale :: Scale, label :: String }

type ScatterPlot a b =
        { xAxis  :: AxisConf
        , yAxis  :: AxisConf
        , series :: Array (SeriesData a b)
        }

foreign import _drawData :: ScaleHandlers -> D3Scatter -> ScatterPlot JSDate Number -> Effect Unit

drawData :: D3Scatter -> ScatterPlot JSDate Number -> Effect Unit
drawData = _drawData
    { scale      : case _ of Date  -> \h -> h.date unit
                             Count c -> \h -> h.count c
    , numberScale: case _ of Linear -> \h -> h.linear unit
                             Log    -> \h -> h.log unit
    }

-- onNumberScale :: forall a. { linear:: Unit -> a, log :: Unit -> a } -> NumberScale -> a
-- onNumberScale h = case _ of
--     Linear -> h.linear unit
--     Log    -> h.log unit

-- onScale :: forall a. { date:: Unit -> a, count:: NumberScale -> a } -> Scale -> a
-- onScale h = case _ of
--     Date    -> h.date unit
--     Count c -> h.count c


type OnNumberScale = forall a. { linear:: Unit -> a, log :: Unit -> a } -> a
type OnScale       = forall a. { date:: Unit -> a, count:: NumberScale -> a } -> a

type ScaleHandlers = { scale       :: Scale -> OnScale
                     , numberScale :: NumberScale -> OnNumberScale
                     }

