
module Type.Equiv where

import Prelude

import Type.Equality
import Data.BSum

newtype Equiv a b = Equiv (forall p. (TypeEquals a b => p) -> p)

infixr 2 type Equiv as ~

withEquiv :: forall a b c. a ~ b -> (TypeEquals a b => c) -> c
withEquiv (Equiv f) = f

overEquiv :: forall a b. Equiv a b -> (b -> b) -> (a -> a)
overEquiv (Equiv f) g = f (from <<< g <<< to)

underEquiv :: forall a b. Equiv a b -> (a -> a) -> (b -> b)
underEquiv (Equiv f) g = f (to <<< g <<< from)

equivTo :: forall a b. a ~ b -> a -> b
equivTo e = withEquiv e to

equivFrom :: forall a b. a ~ b -> b -> a
equivFrom e = withEquiv e from

refl :: forall a. a ~ a
refl = Equiv \x -> x

type OneOf a = BSum (Equiv a)
