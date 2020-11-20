
module Corona.Data.Type where

import Prelude

import Affjax as AX
import Affjax.ResponseFormat as ResponseFormat
import Control.Alternative
import Control.Monad.Except
import Control.Monad.Maybe.Trans
import Data.Array as A
import Data.Date
import Data.Either
import Data.Function.Uncurried
import Data.Functor
import Data.HTTP.Method (Method(..))
import Data.Int
import Data.Int.Parse
import Data.JSDate (JSDate)
import Data.JSDate as JSDate
import Data.Map as M
import Data.Maybe
import Data.ModifiedJulianDay (Day, fromJSDate)
import Data.ModifiedJulianDay as MJD
import Data.Traversable
import Data.TraversableWithIndex
import Data.Tuple
import Effect
import Effect.Aff
import Effect.Class
import Foreign.Object as O
import Foreign.Papa

type Region = String

type CoronaData =
    { start  :: Day
    , dat    :: O.Object { pop :: Int, counts :: Counts (Array Int) }
    }

type Counts a =
    { confirmed :: a
    , deaths    :: a
    , recovered :: a
    , active    :: a
    }

mapCounts :: forall a b. (a -> b) -> Counts a -> Counts b
mapCounts f c =
    { confirmed: f c.confirmed
    , deaths: f c.deaths
    , recovered: f c.recovered
    , active: f c.active
    }

combineDat
    :: forall a b.
       O.Object a
    -> O.Object b
    -> O.Object { pop :: a, counts :: b }
combineDat = runFn3 _intersectionWith (\pop counts -> { pop, counts })


foreign import _intersectionWith
    :: forall a b c.
       Fn3 (a -> b -> c)
           (O.Object a)
           (O.Object b)
           (O.Object c)

