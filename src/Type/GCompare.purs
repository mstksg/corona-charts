
module Type.GCompare where

import Prelude

import Data.Exists
import Data.Functor.Product
import Data.Maybe
import Data.Tuple
import Type.Equiv

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

class GShow f where
    gshow :: forall a. f a -> String

class GShow2 f where
    gshow2 :: forall a b. f a b -> String

newtype WrEx k = WrEx (Exists k)

mkWrEx :: forall k a. k a -> WrEx k
mkWrEx = WrEx <<< mkExists

instance eqSome :: GEq f => Eq (WrEx f) where
    eq (WrEx sx) (WrEx sy) = runExists (\x ->
        runExists (\y ->
          isJust (geq x y)
        ) sy
      ) sx

instance ordSome :: GOrd f => Ord (WrEx f) where
    compare (WrEx sx) (WrEx sy) = runExists (\x ->
        runExists (\y ->
          toOrdering (gcompare x y)
        ) sy
      ) sx

instance showSome :: GShow f => Show (WrEx f) where
    show (WrEx sx) = runExists gshow sx

instance gshowProd :: (GShow f, GShow g) => GShow (Product f g) where
    gshow (Product (Tuple f g)) =
        "Product (Tuple (" <> gshow f <> ") (" <> gshow g <>"))"
