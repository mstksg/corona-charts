
module Data.Dated where

import Prelude

import Data.Tuple
import Data.Array as A
import Data.Sequence as Seq
import Data.Functor
import Data.FunctorWithIndex as FWI
import Data.Lazy as Lazy
import Data.List as L
import Data.Foldable as Foldable
import Data.List.Lazy as LL
import Data.Maybe
import Data.ModifiedJulianDay (Day(..), addDays, diffDays)
import Data.ModifiedJulianDay as MJD
import Data.Newtype

newtype Dated a = Dated
    { start  :: Day
    , values :: Array a
    }

derive instance newtypeDated :: Newtype (Dated a) _

instance showDated :: Show a => Show (Dated a) where
    show (Dated x) = "Dated " <> show x

instance functorDated :: Functor Dated where
    map f (Dated d) = Dated { start: d.start, values: map f d.values }

instance functorWithIndexDated :: FWI.FunctorWithIndex Day Dated where
    mapWithIndex = mapWithIndex

instance applyDated :: Apply Dated where
    apply = zipDated ($)

instance foldedableDated :: Foldable.Foldable Dated where
    foldr f z = Foldable.foldr f z <<< (_.values) <<< unwrap
    foldl f z = Foldable.foldl f z <<< (_.values) <<< unwrap
    foldMap f = Foldable.foldMap f <<< (_.values) <<< unwrap


datedValues :: forall a. Dated a -> Array a
datedValues (Dated d) = d.values

datedStart :: forall a. Dated a -> Day
datedStart (Dated d) = d.start

datedEnd :: forall a. Dated a -> Day
datedEnd (Dated d) = addDays (A.length (d.values) - 1) d.start

datedLength :: forall a. Dated a -> Int
datedLength = A.length <<< datedValues

-- | delete all items but leave the start date
clearDated :: forall a b. Dated a -> Dated b
clearDated (Dated d) = Dated $ d { values = [] }


generate
    :: forall a.
       Day      -- ^ start
    -> Int      -- ^ timespan (length - 1)
    -> (Day -> a)   -- ^ generator
    -> Dated a
generate start tspan f = Dated { start, values }
  where
    Day i  = start
    values = map (f <<< Day) (A.range i (i + tspan))
                

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
    , values: A.mapWithIndex (\i -> f (addDays i x.start)) x.values
    }

findIndex
    :: forall a.
       (a -> Boolean)
    -> Dated a
    -> Maybe Day
findIndex p (Dated x) = flip addDays x.start <$> A.findIndex p x.values

findLastIndex
    :: forall a.
       (a -> Boolean)
    -> Dated a
    -> Maybe Day
findLastIndex p (Dated x) = flip addDays x.start <$> A.findLastIndex p x.values

drop :: forall a.  Int -> Dated a -> Dated a
drop i (Dated x) = Dated
    { start: addDays i x.start
    , values: A.drop i x.values
    }

dropEnd :: forall a.  Int -> Dated a -> Dated a
dropEnd i (Dated x) = Dated $
    x { values = A.dropEnd i x.values }

take :: forall a.  Int -> Dated a -> Dated a
take i (Dated x) = Dated $
    x { values = A.take i x.values }

takeEnd :: forall a.  Int -> Dated a -> Dated a
takeEnd i (Dated x) = Dated
    { start: addDays (A.length x.values - i) x.start
    , values: A.takeEnd i x.values
    }

dropBefore :: forall a.  Day -> Dated a -> Dated a
dropBefore d0 (Dated x) = Dated
    { start: max d0 x.start
    , values: A.drop (diffDays d0 x.start) x.values
    }

dropAfter :: forall a.  Day -> Dated a -> Dated a
dropAfter d0 (Dated x) = Dated $ x
    { values = A.take (diffDays d0 x.start) x.values
    }

findHead :: forall a. Dated a -> Maybe { day :: Day, value :: a }
findHead (Dated x) = A.head x.values <#> \value ->
                        { day: x.start, value }

findLast :: forall a. Dated a -> Maybe { day :: Day, value :: a }
findLast (Dated x) = A.last x.values <#> \value ->
    { day: addDays (A.length x.values) x.start
    , value
    }

dropUntil
    :: forall a.
       (a -> Boolean)
    -> Dated a
    -> Dated a
dropUntil f (Dated xs) = case findIndex f (Dated xs) of
    Nothing -> Dated (xs { values = [] })
    Just i  -> dropBefore i (Dated xs)

dropUntilEnd
    :: forall a.
       (a -> Boolean)
    -> Dated a
    -> Dated a
dropUntilEnd f (Dated xs) = case findLastIndex f (Dated xs) of
    Nothing -> Dated (xs { values = [] })
    Just i  -> dropAfter i (Dated xs)

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
        (\i x -> Tuple (addDays i xs.start) x)
        xs.values
