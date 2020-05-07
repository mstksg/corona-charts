
module Type.Equiv where

import Type.Equality

type Equiv a b = forall p. (TypeEquals a b => p) -> p

refl :: forall a. Equiv a a
refl x = x

