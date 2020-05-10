
module Halogen.ChainPicker where

import Prelude

import CSS as CSS
import Control.Alternative
import Control.Monad.Maybe.Trans
import Control.Monad.State.Class
import Control.MonadZero as MZ
import Data.Array as A
import Data.Boolean
import Data.Either
import Data.Exists
import Data.Foldable
import Data.Functor.Compose
import Data.FunctorWithIndex
import Data.FunctorWithIndex
import Data.Int.Parse
import Data.List as L
import Data.Map (Map)
import Data.Map as M
import Data.Maybe
import Data.Sequence (Seq)
import Data.Sequence as Seq
import Data.Set (Set)
import Data.Set as S
import Data.String as String
import Data.String.Pattern as String
import Data.String.Regex as Regex
import Data.String.Regex.Flags as Regex
import Data.Symbol (SProxy(..))
import Data.Traversable
import Data.Tuple
import Effect.Class
import Effect.Class.Console (log)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS as HC
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Type.Chain
import Type.DMap (DMap)
import Type.DMap as DM
import Type.DProd
import Web.DOM.Element as W
import Web.DOM.HTMLCollection as HTMLCollection
import Web.HTML.HTMLOptionElement as Option
import Web.HTML.HTMLSelectElement as Select
import Web.UIEvent.MouseEvent as ME

-- type SomeChain f = Some2 (Chain f)

data SubQuery f a r = SQ (Exists (f a) -> r)

newtype SomeSubQuery f r = SSQ (forall q. (forall a. SubQuery f a r -> q) -> q)


-- | Picker of chain link @f@ with input @a@
newtype Picker f m a = Picker (H.Component HH.HTML (SubQuery f a) Unit Unit m)

-- | A way to select an appropriate 'Picker' based on a given type tag
-- type PickerMap tag f q m = DMap tag (Picker f q m)
type PickerMap tag f q m = DProd tag (Picker f m)

type ChildSlots f =
        ( chainLink :: H.Slot (SomeSubQuery f) Unit Int
        )

        -- Scatter.Query               Void                         Unit
        -- ( scatter     :: H.Slot Scatter.Query               Void                         Unit
        -- , multiselect :: H.Slot (MultiSelect.Query Country) (MultiSelect.Output Country) Unit

-- component :: forall a m. MonadEffect m => H.Component HH.HTML (Query a) (State a) (Output a) m

-- type SubIx = Int

-- newtype TagIn tag a b = TagIn (tag a)

type State tag =
      { tagChain :: Seq (Exists tag)
      -- chainLength :: SubIx
      -- , endTag      :: Some tag     -- ^ used to know what to allocate next
      }

data Action =
          AddLink
        | RemoveLink
        | TriggerUpdate

data Query f a r =
        AskSelected (Exists (Chain f a) -> r)

-- render
--     :: forall tag f q m a. MonadEffect m
--     => PickerMap tag f q m
--     -> tag a            -- ^ first type
--     -> State tag
--     -> H.ComponentHTML Action (ChildSlots f) m
-- render pickerMap t0 s = HH.div_ [
--       HH.div_ $ flip mapWithIndex tList $ \i sT -> withSome sT (\t ->
--           let Picker comp = runDProd pickerMap t
--           in  HH.slot _chainLink i comp unit (const (Just TriggerUpdate))
--       )
--     ]
--   where
--     tList = A.cons (some t0) (A.fromFoldable s.tagChain)

-- runDProd :: forall tag f a. DProd tag f -> tag a -> f a
-- runDProd (DProd f) = f


-- render dat st = HH.main_ [
--       HH.h1_ [HH.text "COVID-19 Data"]
--     , HH.div [classProp "ui"] [
--         HH.div [classProp "plot"] [
--           HH.h2_ [HH.text title]
--         , HH.div [classProp "d3"] [
--             HH.slot _scatter unit (Scatter.component) unit absurd

-- data Output a = SelectionChanged (Array a)


-- pickNext :: p a ->

_chainLink :: SProxy "chainLink"
_chainLink = SProxy

