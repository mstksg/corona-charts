
module Type.Chain where

import Prelude

import Type.Equiv
import Type.Equality

data Chain f a b =
      Nil (Equiv a b)
    | Cons (forall r. (forall c. f c b -> Chain f a c -> r) -> r)

nil :: forall f a. Chain f a a
nil = Nil refl

cons :: forall f a b c. f b c -> Chain f a b -> Chain f a c
cons x xs = Cons (\f -> f x xs)

runChainF
    :: forall f p a b. Functor p
    => (forall r s. f r s -> p r -> p s)
    -> Chain f a b
    -> p a
    -> p b
runChainF f = go
  where
    go :: forall c d. Chain f c d -> p c -> p d
    go = case _ of
      Nil refl -> withEquiv refl $ map to
      Cons g   -> \x -> g (\y c -> f y (go c x))
