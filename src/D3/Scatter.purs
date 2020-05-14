
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
foreign import data Interactor :: Type

foreign import _mkSvg   :: Fn2 Element { width :: Number, height :: Number } (Effect D3Scatter)
foreign import clearSvg :: D3Scatter -> Effect Unit

foreign import _drawData
    :: forall a b c.
     Fn7 (HandleFunc1 SType OnSType) 
         (HandleFunc1 Scale OnScale)
         (SType a)
         (SType b)
         (SType c)
         D3Scatter
         (ScatterPlot a b c)
         (Effect Interactor)

foreign import _highlight
    :: Fn3 (HandleFunc1 Maybe OnMaybe)
           Interactor
           (Maybe String)
           (Effect Unit)

drawData_
    :: forall a b c.
       SType a
    -> SType b
    -> SType c
    -> D3Scatter
    -> ScatterPlot a b c
    -> Effect Interactor
drawData_ = runFn7 _drawData handle1 handle1 

drawData
    :: forall a b c. STypeable a => STypeable b => STypeable c
    => D3Scatter
    -> ScatterPlot a b c
    -> Effect Interactor
drawData = drawData_ sType sType sType

mkSvg :: Element -> { width :: Number, height :: Number } -> Effect D3Scatter
mkSvg = runFn2 _mkSvg

highlight :: Interactor -> Maybe String -> Effect Unit
highlight = runFn3 _highlight handle1
