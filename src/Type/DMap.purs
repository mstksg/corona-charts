
module Type.DMap where

import Prelude

import Data.Exists
import Data.Map (Map)
import Data.Map as M
import Data.Maybe
import Type.DSum
import Type.Equiv
import Type.GCompare
import Unsafe.Coerce

newtype DMap k f = DMap (Map (WrEx k) (Exists f))

empty :: forall k f. DMap k f
empty = DMap M.empty

singleton :: forall k f a. k a -> f a -> DMap k f
singleton k v = DMap (M.singleton (mkWrEx k) (mkExists v))

insert :: forall k f a. GOrd k => k a -> f a -> DMap k f -> DMap k f
insert k v (DMap mp) = DMap (M.insert (mkWrEx k) (mkExists v) mp)

insertWith
    :: forall k f a. GOrd k
     => (f a -> f a -> f a)
     -> k a
     -> f a
     -> DMap k f
     -> DMap k f
insertWith f k v (DMap mp) = DMap (M.insertWith go (mkWrEx k) (mkExists v) mp)
  where
    go x y = runExists (\x' ->
        runExists (\y' ->
          mkExists (f (unsafeCoerce x') (unsafeCoerce y'))
        ) y
      ) x

lookup :: forall k f a. GOrd k => k a -> DMap k f -> Maybe (f a)
lookup k (DMap mp) = map (runExists unsafeCoerce)
                         (M.lookup (mkWrEx k) mp)
