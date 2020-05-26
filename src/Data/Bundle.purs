
module Data.Bundle where

import Prelude

import Data.Newtype

-- | used for transforming model predictions alongside normal data
newtype Bundle h f a = Bundle
    { principal :: f a
    , mirrored  :: h (f a)
    }

derive instance newtypeBundle :: Newtype (Bundle h f a) _

hoistBundle :: forall h f g a b. Functor h => (f a -> g b) -> Bundle h f a -> Bundle h g b
hoistBundle f (Bundle pm) = Bundle
    { principal: f pm.principal
    , mirrored: map f pm.mirrored
    }

bundlePrincipal :: forall h f a. Bundle h f a -> f a
bundlePrincipal (Bundle pm) = pm.principal

bundleMirrored :: forall h f a. Bundle h f a -> h (f a)
bundleMirrored (Bundle pm) = pm.mirrored

mkBundle :: forall h f a. f a -> h (f a) -> Bundle h f a
mkBundle x y = Bundle { principal: x, mirrored: y }

mapPrincipal :: forall h f a. (f a -> f a) -> Bundle h f a -> Bundle h f a
mapPrincipal f (Bundle pm) = Bundle $ pm { principal = f pm.principal }
