
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
    :: forall a b c d.
     Fn8 (HandleFunc1 SType OnSType)
         (HandleFunc1 Scale OnScale)
         (SType a)
         (SType b)
         (SType c)
         (SType d)
         D3Scatter
         (ScatterPlot a b c d)
         (Effect Interactor)

foreign import _highlight
    :: Fn3 (HandleFunc1 Maybe OnMaybe)
           Interactor
           (Maybe String)
           (Effect Unit)

foreign import _saveFile
    :: Fn2 Interactor
           String
           (Effect Boolean)

drawData_
    :: forall a b c d.
       SType a
    -> SType b
    -> SType c
    -> SType d
    -> D3Scatter
    -> ScatterPlot a b c d
    -> Effect Interactor
drawData_ = runFn8 _drawData handle1 handle1

drawData
    :: forall a b c d. STypeable a => STypeable b => STypeable c => STypeable d
    => D3Scatter
    -> ScatterPlot a b c d
    -> Effect Interactor
drawData = drawData_ sType sType sType sType

mkSvg :: Element -> { width :: Number, height :: Number } -> Effect D3Scatter
mkSvg = runFn2 _mkSvg

highlight :: Interactor -> Maybe String -> Effect Unit
highlight = runFn3 _highlight handle1

saveAsPng :: Interactor -> String -> Effect Boolean         -- ^ true if success
saveAsPng = runFn2 _saveFile
