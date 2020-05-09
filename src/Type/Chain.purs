
module Type.Chain where

import Prelude

import Type.Equiv
import Type.Equality

data Chain f a b =
      Nil (a ~ b)
    | Cons (forall r. (forall c. f c b -> Chain f a c -> r) -> r)

nil :: forall f a. Chain f a a
nil = Nil refl

cons :: forall f a b c. f b c -> Chain f a b -> Chain f a c
cons x xs = Cons (\f -> f x xs)

runChain
    :: forall f p a b.
       (forall r s. f r s -> p r -> p s)
    -> Chain f a b
    -> p a
    -> p b
runChain f = go
  where
    go :: forall c d. Chain f c d -> p c -> p d
    go = case _ of
      Nil refl -> equivToF refl
      Cons g   -> \x -> g (\y c -> f y (go c x))

-- data Last f a b =
--         CLNil  (a ~ b)
--       | CLLast (forall r. (forall c. f a c -> r) -> r)

-- last :: forall f a b. Chain f a b -> Last f a b
-- last = case _ of
--     Nil  refl -> CLNil refl
--     Cons f    -> f (\x xs -> case xs of
--       Nil refl   -> CLLast (\g -> g x)
--       _          -> last xs
--     )
--   -- runChain go c (CLNil refl :: Last f a a)
--   -- where
--   --   go :: forall r s. f r s -> Last f a r -> Last f a s
--   --   go x _ = CLLast (\f -> f ?k)




