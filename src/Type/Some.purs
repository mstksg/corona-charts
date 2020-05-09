
module Type.Some where

import Prelude

import Data.Maybe
import Type.GCompare

newtype Some f = Some (forall r. (forall a. f a -> r) -> r)

some :: forall f a. f a -> Some f
some x = Some (\f -> f x)

withSome :: forall f r. Some f -> (forall a. f a -> r) -> r
withSome (Some f) = f

newtype Some2 f = Some2 (forall r. (forall a b. f a b -> r) -> r)

some2 :: forall f a b. f a b -> Some2 f
some2 x = Some2 (\f -> f x)

withSome2 :: forall f r. Some2 f -> (forall a b. f a b -> r) -> r
withSome2 (Some2 f) = f

instance eqSome :: GEq f => Eq (Some f) where
    eq sx sy = withSome sx (\x ->
                 withSome sy (\y ->
                   isJust (geq x y)
                 )
               )

instance ordSome :: GOrd f => Ord (Some f) where
    compare sx sy = withSome sx (\x ->
                      withSome sy (\y ->
                        toOrdering (gcompare x y)
                      )
                    )
