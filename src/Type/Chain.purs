
module Type.Chain where

import Prelude

import Data.Either
import Data.Newtype
import Data.Const
import Type.Equiv
import Type.Equality
import Undefined
import Type.Ap

data Chain f a b =
      Nil (a ~ b)
    | Cons (Ap f (Chain f) a b)

nil :: forall f a. Chain f a a
nil = Nil refl

cons :: forall f a b c. f a b -> Chain f b c -> Chain f a c
cons x xs = Cons (mkAp x xs)

singleton :: forall f a b. f a b -> Chain f a b
singleton x = cons x nil

snoc :: forall f a b c. Chain f a b -> f b c -> Chain f a c
snoc = case _ of
    Nil refl -> \x -> cons (equivFromF2 refl x) nil
    Cons f   -> \y -> withAp f (\x xs -> cons x (snoc xs y))

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
      Cons g   -> \x -> withAp g (\y c -> go c (f y x))

hoistChain
    :: forall f g a b.
       (forall r s. f r s -> g r s)
    -> Chain f a b
    -> Chain g a b
hoistChain f = go
  where
    go :: forall r s. Chain f r s -> Chain g r s
    go = case _ of
      Nil refl -> Nil refl
      Cons a   -> withAp a (\x xs -> cons (f x) (go xs))

hoistChainA
  :: forall f g m a b. Applicative m =>
       (forall r s. f r s -> m (g r s))
    -> Chain f a b
    -> m (Chain g a b)
hoistChainA f = go
  where
    go :: forall r s. Chain f r s -> m (Chain g r s)
    go = case _ of
      Nil refl -> pure (Nil refl)
      Cons a   -> withAp a (\x xs -> cons <$> f x <*> go xs)

runChainM
    :: forall f p a b m. Monad m
    => (forall r s. f r s -> p r -> m (p s))
    -> Chain f a b
    -> p a
    -> m (p b)
runChainM f = go
  where
    go :: forall c d. Chain f c d -> p c -> m (p d)
    go = case _ of
      Nil refl -> pure <<< equivToF refl
      Cons g   -> \x -> withAp g (\y c -> go c =<< f y x)

foldMapChain
    :: forall f a b m. Monoid m
    => (forall r s. f r s -> m)
    -> Chain f a b
    -> m
foldMapChain f = unwrap <<< hoistChainA (Const <<< f)

type Snoc f a b = Ap (Chain f) f a b

unsnoc
    :: forall f a b.
       Chain f a b
    -> Either (a ~ b) (Snoc f a b)
unsnoc = case _ of
    Nil refl -> Left refl
    Cons f   -> withAp f (\xs x -> Right (go xs x))
    -- (\g -> f (\x xs -> go x xs g))
  where
    go  :: forall r s. f r s -> Chain f s b -> Snoc f r b
    go x = case _ of
      Nil refl -> mkAp nil (equivToF refl x)
      Cons f   -> withAp f (\y ys ->
          withAp (go y ys) (\zs z ->
            mkAp (cons x zs) z
          )
        )

splitAt
    :: forall f a b.
       Int
    -> Chain f a b
    -> Ap (Chain f) (Chain f) a b
splitAt n xs
    | n <= 0    = mkAp nil xs
    | otherwise = case xs of
        Nil refl -> equivToF refl (mkAp (nil :: Chain f a a) (nil :: Chain f a a))
        Cons f   -> withAp f (\x xs ->
          withAp (splitAt (n - 1) xs) (\ys zs ->
            mkAp (cons x ys) zs
          )
        )

