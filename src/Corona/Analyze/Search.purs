module Corona.Analyze.Search where

import Prelude

import Math
import Data.Function.Uncurried
import Data.Maybe

-- | bisection search to find the local maximum or minimum, as long as the
-- function only has one and it is not at either endpoint.
bisectionExtreme
    :: Number                   -- ^ epsilon
    -> Number                   -- ^ half-difference to sample at to estimate f'
    -> (Number -> Number)       -- ^ function to find max/min of
    -> Number                   -- ^ min
    -> Number                   -- ^ max
    -> Maybe Number
bisectionExtreme eps h f mn mx = bisectionSearch eps go mn mx
  where
    go x = (f (x+h) - f (x-h)) / h


-- TODO: add epsilon by x, as well, to quit early maybe?  nah
bisectionSearch
    :: Number                   -- ^ epsilon
    -> (Number -> Number)       -- ^ function to find root of
    -> Number                   -- ^ min
    -> Number                   -- ^ max
    -> Maybe Number
bisectionSearch = runFn5 _bisectionSearch
    { nothing: const Nothing
    , just: Just
    }

foreign import _bisectionSearch
    :: forall r.
       Fn5 { nothing :: Unit -> r, just :: Number -> r }
           Number
           (Number -> Number)
           Number
           Number
           r
