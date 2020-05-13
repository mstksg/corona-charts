
module D3.Scatter where

import Prelude

import D3.Scatter.Type
import Data.JSDate (JSDate)
import Data.JSDate as JSDate
import Data.Either
import Data.Maybe
import Data.ModifiedJulianDay (Day)
import Data.Tuple
import Effect
import Foreign.Object as O
import Type.Equality
import Type.Handler
import Data.Function.Uncurried
import Type.Equiv
import Web.DOM.Element (Element)

foreign import data D3Scatter :: Type

foreign import _mkSvg   :: Fn2 Element { width :: Number, height :: Number } (Effect D3Scatter)
foreign import clearSvg :: D3Scatter -> Effect Unit

foreign import _drawData
    :: forall a b.
     Fn6 (HandleFunc1 SType OnSType) 
         (HandleFunc1 Scale OnScale)
         (SType a)
         (SType b)
         D3Scatter
         (ScatterPlot a b)
         (Effect Unit)

drawData_ :: forall a b. SType a -> SType b -> D3Scatter -> ScatterPlot a b -> Effect Unit
drawData_ = runFn6 _drawData handle1 handle1 

drawData :: forall a b. STypeable a => STypeable b => D3Scatter -> ScatterPlot a b -> Effect Unit
drawData = drawData_ sType sType

mkSvg :: Element -> { width :: Number, height :: Number } -> Effect D3Scatter
mkSvg = runFn2 _mkSvg

--         const width = 1000;
--         const height = 600;
