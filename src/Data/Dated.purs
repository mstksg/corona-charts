
module Data.Dated where

import Prelude

import Data.Tuple
import Data.Array as A
import Data.Sequence as Seq
import Data.Functor
import Data.FunctorWithIndex as FWI
import Data.Lazy as Lazy
import Data.List as L
import Data.List.Lazy as LL
import Data.Maybe
import Data.ModifiedJulianDay (Day(..))
import Data.ModifiedJulianDay as MJD
import Data.Newtype

newtype Dated a = Dated
    { start  :: Day
    , values :: Array a
    }

instance showDated :: Show a => Show (Dated a) where
    show (Dated x) = "Dated " <> show x


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
    LT -> { start: ys.start, values: A.zipWith f (A.drop (dy - dx) xs.values) ys.values }
    EQ -> { start: xs.start, values: A.zipWith f xs.values ys.values }
    GT -> { start: xs.start, values: A.zipWith f xs.values (A.drop (dx - dy) ys.values) }
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

findIndex
    :: forall a.
       (a -> Boolean)
    -> Dated a
    -> Maybe Day
findIndex p (Dated x) = flip MJD.addDays x.start <$> A.findIndex p x.values

mapMaybe
    :: forall a b.
       (a -> Maybe b)
    -> Dated a
    -> LL.List (Dated b)
mapMaybe f (Dated d0) = chompFalse (unwrap d0.start) (L.fromFoldable d0.values)
  where
    chompFalse :: Int -> L.List a -> LL.List (Dated b)
    chompFalse i = case _ of
      L.Nil -> LL.nil
      L.Cons x xs -> case f x of
        Nothing -> chompFalse (i + 1) xs
        Just y  -> chompTrue i y xs
    chompTrue :: Int -> b -> L.List a -> LL.List (Dated b)
    chompTrue i0 y = go 1 [y]
      where
        go :: Int -> Array b -> L.List a -> LL.List (Dated b)
        go i ys = case _ of
          L.Nil -> LL.singleton $ Dated
            { start: Day i0
            , values: ys
            }
          L.Cons x xs -> case f x of
            Just z  -> go (i + 1) (ys <> [z]) xs
            Nothing -> LL.cons
                (Dated { start: Day i0, values: ys })
                (chompFalse (i0 + i + 1) xs)

toArray :: forall a. Dated a -> Array (Tuple Day a)
toArray (Dated xs) = A.mapWithIndex
        (\i x -> Tuple (MJD.addDays i xs.start) x)
        xs.values
