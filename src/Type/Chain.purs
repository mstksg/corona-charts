
module Type.Chain where

import Prelude

import Type.Equiv
import Type.Equality

data Chain f a b =
      Nil (Equiv a b)
    | Cons (forall r. (forall c. f a c -> Chain f c b -> r) -> r)

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
      Nil refl -> refl $ map to
      Cons g   -> \x -> g (\y c -> go c (f y x))
