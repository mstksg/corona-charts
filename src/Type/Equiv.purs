
module Type.Equiv where

import Prelude

import Data.Maybe
import Type.Equality
import Unsafe.Coerce

newtype Equiv a b = Equiv (forall p. (TypeEquals a b => p) -> p)

infixr 2 type Equiv as ~

withEquiv :: forall a b c. a ~ b -> (TypeEquals a b => c) -> c
withEquiv (Equiv f) = f

overEquiv :: forall a b. Equiv a b -> (b -> b) -> (a -> a)
overEquiv _ = unsafeCoerce

underEquiv :: forall a b. Equiv a b -> (a -> a) -> (b -> b)
underEquiv _ = unsafeCoerce

equivTo :: forall a b. a ~ b -> a -> b
equivTo _ = unsafeCoerce

equivToF :: forall f a b. (a ~ b) -> f a -> f b
equivToF _ = unsafeCoerce

equivFrom :: forall a b. a ~ b -> b -> a
equivFrom _ = unsafeCoerce

equivFromF :: forall f a b. a ~ b -> f b -> f a
equivFromF _ = unsafeCoerce

refl :: forall a. a ~ a
refl = Equiv \x -> x

class Decide f where
    decide :: forall a b. f a -> f b -> Maybe (a ~ b)

instance eqEquiv :: Eq (Equiv a b) where
    eq _ _ = true

instance ordEquiv :: Ord (Equiv a b) where
    compare _ _ = EQ
