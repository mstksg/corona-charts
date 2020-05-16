
module Halogen.ChainPicker2 where

import Prelude

import CSS as CSS
import Control.Alternative
import Control.Monad.Except
import Control.Monad.Maybe.Trans
import Control.Monad.State
import Control.Monad.State.Class
import Control.MonadZero as MZ
-- import Control.Monad.Free
import Data.Array as A
import Data.Bifunctor as BF
import Data.Boolean
import Data.Either
import Data.Exists
import Data.Foldable
import Data.Functor
import Data.Functor.Compose
import Data.FunctorWithIndex
import Data.FunctorWithIndex
import Data.Int.Parse
import Data.List as L
import Data.Map (Map)
import Data.Map as M
import Data.Maybe
import Data.Newtype
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
import Debug.Trace
import Effect.Class
import Effect.Exception (throw)
import Effect.Class.Console (log, error)
import Halogen as H
import Halogen.HTML as HH
import Halogen.Query as HQ
import Halogen.HTML.CSS as HC
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Util as HU
import Type.Ap
import Type.Chain (Chain)
import Type.Chain as C
import Type.DMap (DMap)
import Type.DMap as DM
import Type.DProd
import Type.DSum
import Type.Equiv
import Type.GCompare
import Type.Some
import Undefined
import Web.DOM.Element as W
import Web.DOM.HTMLCollection as HTMLCollection
import Web.HTML.HTMLOptionElement as Option
import Web.HTML.HTMLSelectElement as Select
import Web.UIEvent.MouseEvent as ME

-- -----------------------
-- | * Picker
-- -----------------------

-- | A picker is a /configuration/ for a possible link in the chain.  Each
-- link in the chain may be one of many choices of pickers.  The chain itself
-- will help manage switching between potential pickers, but the picker
-- manages its own configuration, once it has been chosen.
--
-- Its type contains the link (@f a b@ is a link transforing @a@ to @b@).  Its
-- input-output is necessarily statically known.
newtype Picker f m a b = Picker
    { label     :: String
    , component :: H.Component HH.HTML (PickerQuery f a b) Unit Unit m
    }

-- | The only thing a picker outputs is an "update me" signal.
-- data PickerOutput = PickerUpdate
-- data PickerOutput f a b = PickerUpdate (f a b)

-- | The query nakedly exposes the state @f a b@ that the component keeps
-- track of.  Get the component, and also set/replace it.
--
-- The "put" option returns a 'Boolean' which is 'False' if the input is
-- incompatible with the picker.  A "different constructor" of @f@.
data PickerQuery f a b r =
    PQState (f a b -> (f a b -> Boolean) -> Tuple r (f a b))

pqGet :: forall f a b. PickerQuery f a b (f a b)
pqGet = PQState (\x _ -> Tuple x x)

pqPut :: forall f a b. f a b -> PickerQuery f a b Boolean
pqPut x = PQState (\x test -> Tuple (test x) x)



-- -----------------------------
-- | ** Existential Picker
-- -----------------------------

-- | A picker where the output type is unknown.  When we are appending to a
-- chain, we will always know what possible /input/ type we can add next, and
-- this lets us restrict on that and that alone, while also leaving the output
-- type up to the user's choice.
--
-- The tag type is the singleton we use to match on to unveil what the @b@ is.
type SomePicker tag f m a = DSum tag (Picker f m a)

-- type SomePickerOutput tag f m a = DSum tag PickerOutput

-- | You have to be able to handle any output type this may give you.  Also,
-- you can only /put/ things of the same output type
data SomePickerQuery tag f a r =
    SomePQState (forall b. tag b -> f a b -> Tuple r (f a b))
            -- you gotta be able to handle any @b@, and this also restricts
            -- what it can accept.

somePqGet :: forall tag f a. SomePickerQuery tag f a (DSum tag (f a))
somePqGet = SomePQState (\t x -> Tuple (t :=> x) x)

somePqPut
    :: forall tag f a b. Decide tag
    => tag b
    -> f a b
    -> SomePickerQuery tag f a Boolean
somePqPut t0 x = SomePQState (\t1 y ->
        case decide t0 t1 of
          Nothing -> Tuple false y
          Just r  -> Tuple true  (equivToF r x)
    )

-- | Given a type, return all possible options for pickers that take that tpye
-- as input.
type PickerMap tag f m = forall a. tag a -> Array (SomePicker tag f m a)

-- -----------------------------
-- | * Chain Component
-- -----------------------------

-- | Query for the whole chain.  You have to be able to handle any output type
-- this may give you.  But, you can also /put/ any type and it will
-- reconfigure the chain to accept it.
data Query tag f a r =
      QGet (forall b. tag b -> Chain f a b -> r)    -- gotta handle any @b@
    | QPut (DSum tag (Chain f a)) r                 -- you can give any @b@

-- | The only thing a chain outputs is an "update me" signal.
data Output = ChainUpdate

newtype LinkState tag a b = LinkState
    { tagIn  :: tag a
    , tagOut :: tag b
    , option :: Int
    }

type State tag a =
    { chain :: DSum tag (Chain (LinkState tag) a)
    }

type PickerIx tag =
    { tagIn    :: WrEx tag   -- ^ type of input
    , chainIx  :: Int        -- ^ position in the chain
    , optionIx :: Int        -- ^ which option in that link?
    }

type ChildSlots tag f =
    ( pickerLink :: H.Slot (SubQuery tag f) Unit (PickerIx tag)
    )

data Action tag =
        PickerUpdate (PickerIx tag)
      | PickerRemove (PickerIx tag)
      | PickerAdd    (Exists tag) Int

component
    :: forall f tag a m i. GOrd tag => GShow tag => MonadEffect m
    => tag a
    -> PickerMap tag f m
    -> H.Component HH.HTML (Query tag f a) i Output m
component t0 pMap = H.mkComponent
    { initialState: \_ -> { chain: t0 :=> C.nil }
    , render: render pMap
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction pMap
        , handleQuery  = handleQuery pMap
        }
    }

data SubQuery tag f r =
    SubQueryState (forall a b. tag a -> tag b -> f a b -> Tuple r (f a b))

render
    :: forall tag f m a. GOrd tag
    => PickerMap tag f m
    -> State tag a
    -> H.ComponentHTML (Action tag) (ChildSlots tag f) m
render pMap st = HH.div_ [
      HH.ul [HU.classProp "op-list"] $
        A.snoc
          (mapWithIndex mkOpSlot chainList)
          (withDSum st.chain (\tOut _ -> mkPendingSlot tOut))


    ]
  where
    mkOpSlot i link = runExists go link.tagIn
      where
        go :: forall b. tag b -> H.ComponentHTML (Action tag) (ChildSlots tag f) m
        go tIn = case pMap tIn A.!! link.option of
          Nothing -> HH.li [HU.classProp "chain-error"] [
            HH.text $ "chain picker error: index out of bounds"
          ]
          Just dsc -> withDSum dsc (\tOut (Picker c) ->
            HH.li [HU.classProp "picker-slot"] [
              HH.div [HU.classProp "picker-slot-title"] [
                HH.span [HU.classProp "picker-slot-name"] [HH.text c.label]
              , HH.a [ HU.classProp "picker-slot-delete"
                     , HE.onClick $ \e ->
                         if ME.buttons e == 0
                            then Just (PickerRemove pIx)
                            else Nothing
                     ]
                     [HH.text "x"]
              ]
            , HH.div [HU.classProp "picker-slot-opts"] [
                HH.slot _pickerLink pIx
                  (HU.hoistQuery (sq2pq tIn tOut) c.component)
                  unit
                  (const $ Just (PickerUpdate pIx))
              ]
            ]
          )
        pIx = { tagIn: WrEx link.tagIn, chainIx: i, optionIx: link.option }
    mkPendingSlot :: forall b. tag b -> H.ComponentHTML (Action tag) (ChildSlots tag f) m
    mkPendingSlot tOut = HH.div [HU.classProp "pending-picker"] [
          HH.select [ HE.onSelectedIndexChange (commitPicker <<< (_ - 1)) ] $
            A.cons (HH.option_ []) $
              flip mapWithIndex options $ \i dsp ->
                withDSum dsp (\tOut (Picker po) -> HH.option [] [ HH.text po.label ])
        ]
      where
        options = pMap tOut
        commitPicker i
            | i < A.length options = Just (PickerAdd (mkExists tOut) i)
            | otherwise            = Nothing
    chainList :: Array { tagIn :: Exists tag, option :: Int }
    chainList = withDSum st.chain (\_ ->
        C.foldMapChain (\(LinkState tl) ->
          [{ tagIn: mkExists tl.tagIn, option: tl.option }]
        )
      )

handleAction
    :: forall f tag m a. Decide tag => GShow tag => MonadEffect m
    => PickerMap tag f m
    -> Action tag
    -> H.HalogenM (State tag a) (Action tag) (ChildSlots tag f) Output m Unit
handleAction pMap act = do
    case act of
      PickerUpdate _ -> pure unit
      PickerRemove pIx -> runExists (\tOutNew -> do
        st <- H.get
        withDSum st.chain (\tOut chain ->
          withAp (C.splitAt pIx.chainIx chain) (\xs ys -> do
            case ys of
              C.Nil _ -> error $ "error: removed link at ix " <> show pIx.chainIx
                              <> " but there's nothing here?"
              C.Cons cap -> withAp cap (\(LinkState ls) zs -> do
                case decide tOutNew ls.tagIn of
                  Nothing -> log $ "warning: removed link claims input " <> gshow tOutNew
                                 <> " at " <> show pIx.chainIx <> " but the chain shows "
                                 <> gshow ls.tagIn
                  Just _ -> pure unit
                H.put { chain: ls.tagIn :=> xs }
              )
          )
        )
      ) (unwrap (pIx.tagIn))
      PickerAdd wtOutOld i -> runExists (\tOutOld -> do
        st <- H.get
        withDSum st.chain (\tOut chain -> do
          case decide tOutOld tOut of
            Nothing -> log $ "warning: added link claims input " <> gshow tOutOld
                           <> " but the chain shows " <> gshow tOut
            Just _  -> pure unit
          case pMap tOut A.!! i of
            Nothing -> error $ "error: invalid added index: " <> show i <> ", but with "
                            <> gshow tOut <> ", only " <> show (A.length (pMap tOut))
                            <> " options"
            Just dlink -> withDSum dlink (\newOut link -> do
              let lsNew = LinkState
                    { tagIn: tOut
                    , tagOut: newOut
                    , option: i
                    }
              H.put { chain: newOut :=> C.snoc chain lsNew }
            )
        )
      ) wtOutOld
    H.raise ChainUpdate

type M tag f m a = H.HalogenM (State tag a) (Action tag) (ChildSlots tag f) Output m

handleQuery
    :: forall tag f m a r. GOrd tag => GShow tag => MonadEffect m =>
       PickerMap tag f m
    -> Query tag f a r
    -> M tag f m a (Maybe r)
handleQuery pMap = case _ of
    QGet f -> do
      res <- assembleChain pMap
      pure $ Just (withDSum res f)
--     | QPut (DSum tag (Chain f a)) r                 -- you can give any @b@
    QPut _ _ -> do
      log "unimplemented"
      pure Nothing


assembleChain
    :: forall tag f m a. GOrd tag => GShow tag => MonadEffect m
    => PickerMap tag f m
    -> M tag f m a (DSum tag (Chain f a))
assembleChain pMap = do
    st <- H.get
    withDSum st.chain (\tOut chain ->
      flip evalStateT 0 $ dsum tOut <$> C.hoistChainA go chain
    )
  where
    go  :: forall r s. LinkState tag r s -> StateT Int (M tag f m a) (f r s)
    go (LinkState{ tagIn, tagOut, option }) = do
      i <- get
      modify_ (_ + 1)
      res <- lift $ H.query
        _pickerLink
        { tagIn: mkWrEx tagIn, chainIx: i, optionIx: option }
        (SubQueryState (\tX tY x ->
            let outValue :: Either String (f r s)
                outValue = do
                  r1 <- case decide tX tagIn of
                    Nothing -> Left $ "subquery mismatch: expected "
                                 <> gshow tagIn <> " input but it was actually "
                                 <> gshow tX <> " at ix " <> show i
                    Just tt -> pure tt
                  r2 <- case decide tY tagOut of
                    Nothing -> Left $ "subquery mismatch: expected "
                                 <> gshow tagOut <> " output but it was actually "
                                 <> gshow tY <> " at ix " <> show i
                    Just tt -> pure tt
                  pure $ equivToF2 r1 (equivToF r2 x)
           in  Tuple outValue x
        ))
      case res of
        Nothing -> liftEffect $ throw $ "picker for " <> gshow tagIn <> " at ix " <> show i
                        <> " not responding."
        Just (Left e) -> liftEffect $ throw $ "picker for " <> gshow tagIn <> " at ix " <> show i
                <> " returned error: " <> e
        Just (Right x) -> pure x

-- -----------------------------
-- | * Helper
-- -----------------------------

sq2pq
    :: forall tag f a b r.
       tag a
    -> tag b
    -> SubQuery tag f r
    -> PickerQuery f a b r
sq2pq tIn tOut (SubQueryState f) = PQState $ \s0 _ -> f tIn tOut s0

_pickerLink :: SProxy "pickerLink"
_pickerLink = SProxy

