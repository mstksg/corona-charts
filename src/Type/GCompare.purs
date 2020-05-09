
module Type.GCompare where

import Prelude

import Type.Equiv
import Data.Maybe

class Decide f <= GEq f where
    geq :: forall a b. f a -> f b -> Maybe (a ~ b)

data GOrdering a b =
      GLT
    | GEQ (a ~ b)
    | GGT

toOrdering :: forall a b. GOrdering a b -> Ordering
toOrdering = case _ of
    GLT   -> LT
    GEQ _ -> EQ
    GGT   -> GT

class GEq f <= GOrd f where
    gcompare :: forall a b. f a -> f b -> GOrdering a b

