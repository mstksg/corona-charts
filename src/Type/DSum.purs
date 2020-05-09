
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
