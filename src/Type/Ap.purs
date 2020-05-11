
module Type.Ap where

newtype Ap f g a b = Ap (forall r. (forall x. f a x -> g x b -> r) -> r)

mkAp :: forall f g a b c. f a b -> g b c -> Ap f g a c
mkAp x y = Ap (\f -> f x y)

withAp :: forall f g a b r. Ap f g a b -> (forall x. f a x -> g x b -> r) -> r
withAp (Ap f) = f
