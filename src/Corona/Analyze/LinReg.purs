module Corona.Analyze.LinReg where

import Prelude

import Corona.Analyze.Transform
import Data.Array as A
import Data.Function.Uncurried

type LinReg a =
    { alpha :: a
    , beta  :: a
    }

linRegWith
    :: forall a.
       (forall r. a -> (Number -> Number -> r) -> r)
    -> Array a
    -> { linReg :: LinReg Number, r2 :: Number }
linRegWith = runFn2 _linReg

applyLinReg
    :: forall a. Semiring a
    => LinReg a
    -> a
    -> a
applyLinReg { alpha, beta } x = alpha + x * beta

linReg
    :: Array { x :: Number, y :: Number }
    -> { linReg :: LinReg Number, r2 :: Number }
linReg = linRegWith (\xy f -> f xy.x xy.y)

linRegTrans
    :: Transform Number
    -> Array {x :: Number, y :: Number}
    -> { linReg :: LinReg Number, r2 :: Number }
linRegTrans tr = linReg
             <<< A.mapMaybe (\xy -> (xy { y = _ }) <$> unTrans tr xy.y)

applyLinRegTrans
    :: forall a. Semiring a
    => Transform a
    -> LinReg a
    -> a
    -> a
applyLinRegTrans tr lr = runTrans tr <<< applyLinReg lr

foreign import _linReg
    :: forall a.
       Fn2 (forall r. a -> (Number -> Number -> r) -> r)
           (Array a)
           { linReg :: LinReg Number, r2 :: Number }
