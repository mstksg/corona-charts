
module Corona.Analyze.Transform where

import Prelude

import Control.Monad
import Data.Traversable
import Data.Bifunctor
import Data.Array as A
import Data.DivisionRing
import Data.EuclideanRing
import Data.Maybe
import Data.Ring
import Data.Semiring
import Math

newtype Transform a = Tr
    { to   :: a -> a
    , from :: a -> Maybe a
    }

runTrans :: forall a. Transform a -> a -> a
runTrans (Tr x) = x.to

unTrans :: forall a. Transform a -> a -> Maybe a
unTrans (Tr x) = x.from

compose
    :: forall a.
       Transform a
    -> Transform a
    -> Transform a
compose (Tr tr) (Tr tr') = Tr
    { to : tr.to <<< tr'.to
    , from : tr'.from <=< tr.from
    }
infixr 9 compose as .%

idTrans :: forall a. Transform a
idTrans = Tr
    { to: identity
    , from: Just
    }

expTrans :: Transform Number
expTrans = Tr
    { to: exp
    , from: \x ->
        if x >= zero
          then Just (log x)
          else Nothing
    }

recipTrans :: forall a. DivisionRing a => Transform a
recipTrans = Tr
    { to: recip
    , from: Just <<< recip
    }

mulTrans :: forall a. EuclideanRing a => a -> Transform a
mulTrans x = Tr
    { to: (_ * x)
    , from: Just <<< (_ / x)
    }

addTrans :: forall a. Ring a => a -> Transform a
addTrans a = Tr
    { to: (_ + a)
    , from: Just <<< (_ - a)
    }

quadTrans :: Transform Number
quadTrans = Tr
    { to: \x -> x * x
    , from: \x ->
        if x >= zero
          then Just (sqrt x)
          else Nothing
    }


-- | parameterized by the maximum cap
--
-- f(x) = C / (1 + e^x)
logisticTrans :: Number -> Transform Number
logisticTrans m = mulTrans m
               .% recipTrans
               .% addTrans one
               .% expTrans

-- | parameterized by the maximum cap
--
-- f(x) = C - exp(x)
decayTrans :: Number -> Transform Number
decayTrans m = addTrans m
            .% mulTrans (-1.0)
            .% expTrans


untransSeries :: forall f a. Traversable f => Transform a -> Array (f a) -> Array (f a)
untransSeries = A.mapMaybe <<< traverse <<< unTrans

transSeries :: forall f g a. Functor f => Functor g => Transform a -> f (g a) -> f (g a)
transSeries = (map <<< map) <<< runTrans
