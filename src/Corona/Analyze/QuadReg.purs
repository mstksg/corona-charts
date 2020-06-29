module Corona.Analyze.QuadReg where

import Prelude

import Corona.Analyze.Transform
import Data.Array as A
import Data.Function.Uncurried

type QuadReg a =
    { alpha :: a
    , beta  :: a
    , gamma :: a
    }

quadRegWith
    :: forall a.
       (forall r. a -> (Number -> Number -> r) -> r)
    -> Array a
    -> { quadReg :: QuadReg Number, r2 :: Number }
quadRegWith = runFn2 _quadReg

applyQuadReg
    :: forall a. Semiring a
    => QuadReg a
    -> a
    -> a
applyQuadReg { alpha, beta, gamma } x = alpha * x * x + beta * x + gamma

quadReg
    :: Array { x :: Number, y :: Number }
    -> { quadReg :: QuadReg Number, r2 :: Number }
quadReg = quadRegWith (\xy f -> f xy.x xy.y)

foreign import _quadReg
    :: forall a.
       Fn2 (forall r. a -> (Number -> Number -> r) -> r)
           (Array a)
           { quadReg :: QuadReg Number, r2 :: Number }
