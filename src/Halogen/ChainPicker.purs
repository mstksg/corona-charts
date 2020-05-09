
module Halogen.ChainPicker where

import CSS as CSS
import Control.Alternative
import Control.Monad.Maybe.Trans
import Control.Monad.State.Class
import Control.MonadZero as MZ
import Data.Array as A
import Data.Boolean
import Data.Either
import Data.Foldable
import Data.FunctorWithIndex
import Data.Int.Parse
import Data.List as L
import Data.Map (Map)
import Data.Map as M
import Data.Maybe
import Data.Set (Set)
import Data.Set as S
import Data.String as String
import Data.String.Pattern as String
import Data.String.Regex as Regex
import Data.String.Regex.Flags as Regex
import Data.Traversable
import Data.Tuple
import Effect.Class
import Effect.Class.Console (log)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS as HC
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Web.DOM.Element as W
import Web.DOM.HTMLCollection as HTMLCollection
import Web.HTML.HTMLOptionElement as Option
import Web.HTML.HTMLSelectElement as Select
import Web.UIEvent.MouseEvent as ME
import Type.Chain

        -- , multiselect :: H.Slot (MultiSelect.Query Country) (MultiSelect.Output Country) Unit

-- type ChainSlot a b = H.Slot Unit b Unit

type SomeChain f = forall r. (forall a b. Chain f a b -> r) -> r

-- newtype WrappedComponent f a b = CC

-- type State f =
--       { currChain :: Array (Option a)
--       }

-- data Action =
--         AddValues
--       | RemoveValue Int
--       | RemoveAll
--       | SetFilter String


data Query f r =
        AskSelected (SomeChain f -> r)
--         | SetState (State a -> { new :: State a, next :: r })

-- data Output a = SelectionChanged (Array a)


-- pickNext :: p a ->

