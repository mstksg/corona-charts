
module Halogen.ChainPicker where

import Prelude

import CSS as CSS
import Control.Alternative
import Control.Monad.Except
import Control.Monad.Maybe.Trans
import Control.Monad.RWS
import Control.Monad.State
import Control.Monad.State.Class
import Control.MonadZero as MZ
import Data.Array as A
import Data.Bifunctor as BF
import Data.Boolean
import Data.Const
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
import Effect.Class.Console (log, error, warn)
import Effect.Exception (throw)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS as HC
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Query as HQ
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
--
-- It also provides a "handles" query that checks if a given @f a b@ is what
-- is "generated" by that picker.
newtype Picker f m a b = Picker
    { label     :: String
    -- yeah we ened to be able to give initial part as state
    , component :: H.Component HH.HTML (PickerQuery f a b) (Maybe (f a b)) Unit m
    , handles   :: f a b -> Boolean
    }

-- | The query nakedly exposes the state @f a b@ that the component keeps
-- track of.  Get the component, and also set/replace it.
--
-- The "put" option returns a 'Boolean' which is 'False' if the input is
-- incompatible with the picker.  A "different constructor" of @f@.
data PickerQuery f a b r =
    PQState (f a b -> Tuple r (f a b))

pqGet :: forall f a b. PickerQuery f a b (f a b)
pqGet = PQState (\x -> Tuple x x)

pqPut :: forall f a b. f a b -> PickerQuery f a b Unit
pqPut x = PQState (\_ -> Tuple unit x)



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
    | QPut (DSum tag (Chain (TaggedLink tag f) a)) r                 -- you can give any @b@

-- | The only thing a chain outputs is an "update me" signal.
data Output = ChainUpdate

newtype LinkState tag f a b = LinkState
    { tagIn  :: tag a
    , tagOut :: tag b
    , value  :: Maybe (f a b)     -- used to initialize
    , option :: Int
    }

instance gshowLinkState :: (GShow tag, GShow2 f) => GShow2 (LinkState tag f) where
    gshow2 (LinkState { tagIn, tagOut, value, option }) =
      "LinkState " <> gshow tagIn <> " " <> gshow tagOut <> " " <> showval <> " " <> show option
      where
        showval = case value of
          Nothing -> "Nothing"
          Just x  -> "(Just (" <> gshow2 x <> "))"

type State tag f a =
    { chain :: DSum tag (Chain (LinkState tag f) a)
    }

type PickerIx tag =
    { tagIn    :: WrEx tag   -- ^ type of input
    , chainIx  :: Int        -- ^ position in the chain
    , optionIx :: Int        -- ^ which option in that link?
    }

data SubQuery tag f r =
    SubQueryState (forall a b. tag a -> tag b -> f a b -> Tuple r (f a b))

type ChildSlots tag f =
    ( pickerLink :: H.Slot (SubQuery tag f) Unit (PickerIx tag)
    )

data Action tag =
        PickerUpdate (PickerIx tag)
      | PickerRemove (PickerIx tag)
      | PickerAdd    (Exists tag) Int

type M tag f m a = H.HalogenM (State tag f a) (Action tag) (ChildSlots tag f) Output m

component
    :: forall f tag a m i. GOrd tag => GShow tag => GShow2 f => MonadEffect m
    => tag a            -- ^ initial input type
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

-- -----------------------------
-- | ** Existential Component Wrapper
-- -----------------------------

data WrappedQuery tag f r =
      WQGet (forall a b. tag a -> tag b -> Chain f a b -> r) -- gotta handle any @a@ and @b@
    | WQPut (DSum2 tag (Chain (TaggedLink tag f))) (Maybe (Exists tag) -> r)
    -- | WQPut (forall q. (forall a b. tag a -> tag b -> Chain (TaggedLink tag f) a b -> Maybe q) -> q)
    --         r                 -- you can give any @b@

wrappedComponent
  :: forall tag f a m i. GOrd tag => GShow tag => MonadEffect m => GShow2 f
    => tag a                   -- ^ initial initial type
    -> PickerMap tag f m
    -> H.Component HH.HTML (WrappedQuery tag f) i Output m
wrappedComponent t0 pMap = HU.hoistQuery
    (unwrapQuery t0)
    (component t0 pMap)

unwrapQuery
    :: forall tag f a r. Decide tag
    => tag a
    -> WrappedQuery tag f r
    -> Query tag f a r
unwrapQuery tA = case _ of
    WQGet f        -> QGet (f tA)
    WQPut ds2 next -> withDSum2 ds2 (\tA' tB chain ->
      case decide tA tA' of
        Just r  -> QPut (tB :=> equivFromF2 r chain) (next Nothing)
        Nothing -> QGet (\_ _ -> next (Just (mkExists tA')))
    )



-- -----------------------------
-- | ** Implementation
-- -----------------------------

data P2 a b = P2

render
    :: forall tag f m a. GOrd tag
    => PickerMap tag f m
    -> State tag f a
    -> H.ComponentHTML (Action tag) (ChildSlots tag f) m
render pMap st = HH.div_ [
      HH.ul [HU.classProp "op-list"] $
        A.snoc
          -- (mapWithIndex mkOpSlot chainList)
          slotList
          (withDSum st.chain (\tOut _ -> mkPendingSlot tOut))
    ]
  where
    slotList
        :: Array (H.ComponentHTML (Action tag) (ChildSlots tag f) m)
    slotList = withDSum st.chain (\_ chain ->
        snd (evalRWS (C.hoistChainA go chain) unit 0)
      )
      where
        go  :: forall r s.
               LinkState tag f r s
            -> RWS Unit (Array (H.ComponentHTML (Action tag) (ChildSlots tag f) m)) Int
                  (P2 r s)
        go (LinkState tl) = do
          i <- get
          modify_ (_ + 1)
          tell <<< A.singleton $ case pMap tl.tagIn A.!! tl.option of
            Nothing -> HH.li [HU.classProp "chain-error"] [
              HH.text "chain picker error: index out of bounds"
            ]
            Just dsc -> withDSum dsc (\tOut (Picker c) ->
              let pIx = { tagIn: mkWrEx tl.tagIn, chainIx: i, optionIx: tl.option }
              in  case decide tl.tagOut tOut of
                    Nothing -> undefined
                    Just r  -> HH.li [HU.classProp "picker-slot"] [
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
                           (HU.hoistQuery (sq2pq tl.tagIn tOut) c.component)
                           (map (equivToF r) tl.value)
                           (const $ Just (PickerUpdate pIx))
                       ]
                     ]
            )
          pure P2
    mkPendingSlot :: forall b. tag b -> H.ComponentHTML (Action tag) (ChildSlots tag f) m
    mkPendingSlot tOut = HH.div [HU.classProp "pending-picker"] [
          HH.select [ HE.onSelectedIndexChange (commitPicker <<< (_ - 1)) ] $
            A.cons (HH.option_ [HH.text "(Add)"]) $
              flip mapWithIndex options $ \i dsp ->
                withDSum dsp (\tOut (Picker po) -> HH.option [] [ HH.text po.label ])
        ]
      where
        options = pMap tOut
        commitPicker i
            | i < A.length options = Just (PickerAdd (mkExists tOut) i)
            | otherwise            = Nothing

handleAction
    :: forall f tag m a. Decide tag => GShow tag => MonadEffect m
    => PickerMap tag f m
    -> Action tag
    -> M tag f m a Unit
handleAction pMap act = do
    case act of
      PickerUpdate _ -> pure unit
      PickerRemove pIx -> runExists (\tOutNew -> do
        st <- H.get
        withDSum st.chain (\tOut chain ->
          withAp (C.splitAt pIx.chainIx chain) (\xs ys -> do
            case ys of
              C.Nil _ -> liftEffect <<< throw $
                    "error: removed link at ix " <> show pIx.chainIx
                 <> " but there's nothing here?"
              C.Cons cap -> withAp cap (\(LinkState ls) zs -> do
                case decide tOutNew ls.tagIn of
                  Nothing -> warn $
                    "warning: removed link claims input " <> gshow tOutNew
                      <> " at " <> show pIx.chainIx <> " but the chain shows "
                      <> gshow ls.tagIn
                  Just _ -> pure unit
                H.put { chain: ls.tagIn :=> xs }
              )
          )
        )
      ) (unwrap (pIx.tagIn))
      PickerAdd wtOutOld i -> runExists (\tOutOld -> do
        st :: State tag f a <- H.get
        withDSum st.chain (\tOut chain -> do
          case decide tOutOld tOut of
            Nothing -> warn $
                  "warning: added link claims input " <> gshow tOutOld
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
                    , value: Nothing
                    , option: i
                    }
              H.put { chain: newOut :=> C.snoc chain lsNew }
            )
        )
      ) wtOutOld
    H.raise ChainUpdate

handleQuery
    :: forall tag f m a r. GOrd tag => GShow tag => MonadEffect m => GShow2 f =>
       PickerMap tag f m
    -> Query tag f a r
    -> M tag f m a (Maybe r)
handleQuery pMap = case _ of
    QGet f -> do
      st <- H.get
      withDSum st.chain (\tOut chain ->
        Just <<< f tOut <$> assembleChain pMap chain
      )
--     | QPut (DSum tag (Chain f a)) r                 -- you can give any @b@
    QPut dschain next -> withDSum dschain (\newTOut newChain -> do
      -- log "putting new chain"
      -- log $ show newChain
      st <- writeChain pMap newChain
      -- log $ show st
      H.put { chain: newTOut :=> st }
      -- H.raise ChainUpdate
      pure (Just next)
    )

assembleChain
    :: forall tag f m a b. GOrd tag => GShow tag => MonadEffect m
    => PickerMap tag f m
    -> Chain (LinkState tag f) a b
    -> M tag f m a (Chain f a b)
assembleChain pMap = flip evalStateT 0 <<< C.hoistChainA go
  where
    go  :: forall r s. LinkState tag f r s -> StateT Int (M tag f m a) (f r s)
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

newtype TaggedLink tag f a b = TaggedLink
    { tagIn  :: tag a
    , tagOut :: tag b
    , link   :: f a b
    }

instance gshowTaggedLink :: (GShow2 f, GShow tag) => GShow2 (TaggedLink tag f) where
    gshow2 (TaggedLink { tagIn, tagOut, link }) =
      "TaggedLink " <> gshow tagIn <> " " <> gshow tagOut <> " " <> gshow2 link

writeChain
    :: forall tag f m a b. GShow tag => GOrd tag => MonadEffect m => GShow2 f
    => PickerMap tag f m
    -> Chain (TaggedLink tag f) a b
    -> M tag f m a (Chain (LinkState tag f) a b)
writeChain pMap = flip evalStateT 0 <<< C.hoistChainA go
  where
    go  :: forall r s.
           TaggedLink tag f r s
        -> StateT Int (M tag f m a) (LinkState tag f r s)
    go (TaggedLink {tagIn, tagOut, link}) = do
        i <- get
        modify_ (_ + 1)
        optIx <- case picked of
          Nothing -> liftEffect $ throw $
                  "writeChain: no handler found for link of type "
               <> gshow tagIn <> " -> " <> gshow tagOut
          Just oi -> pure oi
        -- TODO: the bug is here. you watn to 'set' it to be the state you
        -- want but  it might not exist
        res <- lift $ H.query
          _pickerLink
          { tagIn: mkWrEx tagIn, chainIx: i, optionIx: optIx }
          (SubQueryState (\tX tY l0 ->
            let inValue = do
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
                  pure $ equivFromF2 r1 (equivFromF r2 link)
             in  case inValue of
                    Left  e -> Tuple (Just e) l0
                    Right x -> Tuple Nothing  x
            )
          )
        case res of
          -- here is what happens
          -- we need to somehow maybe cache this to be given as it is
          -- initialized.  maybe as Input/state?
          Nothing -> pure unit    -- we let the defualt value do the trick
          -- Nothing -> liftEffect $ throw $
          --   "writeChain: link at " <> show i <> " returned no response"
          Just Nothing -> pure unit
          Just (Just e) -> liftEffect $ throw $
            "writeChain: error when writing link: " <> e
        pure $ LinkState { tagIn, tagOut, option: optIx, value: Just link }
      where
        picked = flip A.findIndex (pMap tagIn) $ \ds ->
          withDSum ds (\tagOut' (Picker pk) -> isJust do
            r <- decide tagOut tagOut'
            MZ.guard $ pk.handles (equivToF r link)
          )

-- chainToState
--     :: forall tag f m a b. GShow tag => Decide tag
--     => PickerMap tag f m
--     -> Chain (TaggedLink tag f) a b
--     -> Either String (Chain (LinkState tag) a b)
-- chainToState pMap = go
--   where
--     go :: forall r. Chain (TaggedLink tag f) r b -> Either String (Chain (LinkState tag) r b)
--     go = case _ of
--       C.Nil  r   -> Right (C.Nil r)
--       C.Cons rap -> withAp rap (\(TaggedLink {tagIn, tagOut, link}) xs ->
--           let picked = flip A.findIndex (pMap tagIn) $ \ds ->
--                 withDSum ds (\tagOut' (Picker pk) -> isJust do
--                   r <- decide tagOut tagOut'
--                   MZ.guard $ pk.handles (equivToF r link)
--                 )
--           in  case picked of
--                 Nothing -> Left $ "no handler found for link of type " <> gshow tagIn
--                               <> " -> " <> gshow tagOut
--                 Just i  -> C.cons (LinkState { tagIn, tagOut, option: i })
--                        <$> go xs
--       )


    -- -> Chain (LinkState tag) a b
    -- -> M tag f m a (Chain f a b)
-- assembleChain pMap = flip evalStateT 0 <<< C.hoistChainA go
  -- where
    -- go  :: forall r s. LinkState tag r s -> StateT Int (M tag f m a) (f r s)

-- writeChain pMap = C.hoistChainA go
--   where
--     go :: forall r s. TaggedLink tag f r s -> M tag f m a (LinkState tag r s)
--   -- where
--     -- go :: forall r. Chain (TaggedLink tag f) r b -> Either String (Chain (LinkState tag) r b)
--     -- go = case _ of
--     --   C.Nil  r   -> Right (C.Nil r)
--     --   C.Cons rap -> withAp rap (\(TaggedLink {tagIn, tagOut, link}) xs ->
--     --       let picked = flip A.findIndex (pMap tagIn) $ \ds ->
--     --             withDSum ds (\tagOut' (Picker pk) -> isJust do
--     --               r <- decide tagOut tagOut'
--     --               MZ.guard $ pk.handles (equivToF r link)
--     --             )
--     --       in  case picked of
--     --             Nothing -> Left $ "no handler found for link of type " <> gshow tagIn
--     --                           <> " -> " <> gshow tagOut
--     --             Just i  -> C.cons (LinkState { tagIn, tagOut, option: i })
--     --                    <$> go xs
--     --   )


-- type PickerIx tag =
--     { tagIn    :: WrEx tag   -- ^ type of input
--     , chainIx  :: Int        -- ^ position in the chain
--     , optionIx :: Int        -- ^ which option in that link?
--     }


-- -----------------------------
-- | * Helper
-- -----------------------------

sq2pq
    :: forall tag f a b r.
       tag a
    -> tag b
    -> SubQuery tag f r
    -> PickerQuery f a b r
sq2pq tIn tOut (SubQueryState f) = PQState $ \s0 -> f tIn tOut s0

_pickerLink :: SProxy "pickerLink"
_pickerLink = SProxy

