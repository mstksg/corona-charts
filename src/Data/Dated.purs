
module Data.Dated where

import Prelude

import Data.ModifiedJulianDay (Day(..))
import Data.FunctorWithIndex as FWI
import Data.Array as A
import Data.ModifiedJulianDay as MJD
import Data.Functor

newtype Dated a = Dated
    { start  :: Day
    , values :: Array a
    }

instance functorDated :: Functor Dated where
    map f (Dated d) = Dated { start: d.start, values: map f d.values }

instance functorWithIndexDated :: FWI.FunctorWithIndex Day Dated where
    mapWithIndex = mapWithIndex

instance applyDated :: Apply Dated where
    apply = zipDated ($)

datedValues :: forall a. Dated a -> Array a
datedValues (Dated d) = d.values

datedStart :: forall a. Dated a -> Day
datedStart (Dated d) = d.start

zipDated
    :: forall a b c.
       (a -> b -> c)
    -> Dated a
    -> Dated b
    -> Dated c
zipDated f (Dated xs) (Dated ys) = Dated case compare dx dy of
    LT -> { start: ys.start, values: A.zipWith f xs.values (A.drop (dy - dx) ys.values) }
    EQ -> { start: xs.start, values: A.zipWith f xs.values ys.values }
    GT -> { start: xs.start, values: A.zipWith f (A.drop (dx - dy) xs.values) ys.values }
  where
    Day dx = xs.start
    Day dy = ys.start

mapWithIndex
    :: forall a b. (Day -> a -> b)
    -> Dated a
    -> Dated b
mapWithIndex f (Dated x) = Dated
    { start: x.start
    , values: A.mapWithIndex (\i -> f (MJD.addDays i x.start)) x.values
    }

