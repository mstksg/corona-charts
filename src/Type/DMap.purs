
module Type.DMap where

import Prelude

import Data.Map (Map)
import Data.Map as M
import Data.Maybe
import Type.DSum
import Type.Equiv
import Type.GCompare
import Type.Some (Some)
import Type.Some as Some
import Unsafe.Coerce

newtype DMap k f = DMap (Map (Some k) (Some f))

empty :: forall k f. DMap k f
empty = DMap M.empty

singleton :: forall k f a. k a -> f a -> DMap k f
singleton k v = DMap (M.singleton (Some.some k) (Some.some v))

insert :: forall k f a. GOrd k => k a -> f a -> DMap k f -> DMap k f
insert k v (DMap mp) = DMap (M.insert (Some.some k) (Some.some v) mp)

insertWith
    :: forall k f a. GOrd k
     => (f a -> f a -> f a)
     -> k a
     -> f a
     -> DMap k f
     -> DMap k f
insertWith f k v (DMap mp) = DMap (M.insertWith go (Some.some k) (Some.some v) mp)
  where
    go x y = Some.withSome x (\x' ->
               Some.withSome y (\y' ->
                 Some.some (f (unsafeCoerce x') (unsafeCoerce y'))
               )
             )

lookup :: forall k f a. GOrd k => k a -> DMap k f -> Maybe (f a)
lookup k (DMap mp) = map (\x -> Some.withSome x unsafeCoerce)
                         (M.lookup (Some.some k) mp)
