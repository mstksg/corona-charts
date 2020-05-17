
module Type.DSum where

import Prelude

import Data.Maybe
import Type.GCompare
import Type.Equiv
import Type.Some as Some

newtype DSum tag f = DSum
  (forall r. (forall a. tag a -> f a -> r) -> r)

dsum :: forall tag f a. tag a -> f a -> DSum tag f
dsum k v = DSum (\f -> f k v)

infix 1 dsum as :=>

withDSum :: forall tag f r. DSum tag f -> (forall a. tag a -> f a -> r) -> r
withDSum (DSum f) = f


instance eqDSum :: (Decide tag, Decide f) => Eq (DSum tag f) where
    eq x y = withDSum x (\xA xB ->
               withDSum y (\yA yB ->
                 isJust (decide xA yA) && isJust (decide xB yB)
               )
             )
instance ordDSum :: (GOrd tag, GOrd f) => Ord (DSum tag f) where
    compare x y = withDSum x (\xA xB ->
                    withDSum y (\yA yB ->
                      toOrdering (gcompare xA yA)
                        <> toOrdering (gcompare xB yB)
                    )
                  )
instance showDSum :: (GShow tag, GShow f) => Show (DSum tag f) where
    show ds = withDSum ds (\x y -> gshow x <> " :=> " <> gshow y)
    -- x y = withDSum x (\xA xB ->
    --            withDSum y (\yA yB ->
    --              isJust (decide xA yA) && isJust (decide xB yB)
    --            )
    --          )

newtype DSum2 tag f = DSum2
  (forall r. (forall a b. tag a -> tag b -> f a b -> r) -> r)

dsum2 :: forall tag f a b. tag a -> tag b -> f a b -> DSum2 tag f
dsum2 k1 k2 v = DSum2 (\f -> f k1 k2 v)

withDSum2
    :: forall tag f r.
       DSum2 tag f
    -> (forall a b. tag a -> tag b -> f a b -> r)
    -> r
withDSum2 (DSum2 f) = f
