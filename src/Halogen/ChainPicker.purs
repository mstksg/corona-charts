
module Halogen.ChainPicker where

import Prelude

import CSS as CSS
import Control.Alternative
import Control.Monad.Except
import Control.Monad.Maybe.Trans
import Control.Monad.State
import Control.Monad.State.Class
import Control.MonadZero as MZ
import Data.Array as A
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
import Effect.Class.Console (log)
import Halogen as H
import Halogen.HTML as HH
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

-- | returns an @f a b@ with the @b@ hidden
data SubQuery f tag a r = SQ (DSum tag (f a) -> r)

instance functorSubQuery :: Functor (SubQuery f tag a) where
    map f = case _ of
      SQ    g -> SQ (f <<< g)
instance applySubQuery :: Apply (SubQuery f tag a) where
    apply (SQ f) (SQ g) = SQ $ \x -> (f x) (g x)
instance applicativeSubQuery :: Applicative (SubQuery f tag a) where
    pure x = SQ (\_ -> x)

newtype SomeQuery f tag r = SomeQuery
    { typeMismatch :: Exists tag -> r
    , typeMatch    :: forall q. (forall a. tag a -> f a r -> q) -> q
    }

type SomeSubQuery f tag = SomeQuery (SubQuery f tag) tag

-- | Picker of chain link @f@ with input @a@
newtype Picker f tag m a = Picker
    { component :: H.Component HH.HTML (SubQuery f tag a) Unit (Exists tag) m
    , initialOut :: Exists tag
    }

-- | A way to select an appropriate 'Picker' based on a given type tag
-- type PickerMap tag f q m = DMap tag (Picker f q m)
type PickerMap f tag m = DProd tag (Compose Maybe (Picker f tag m))

type ChildIx tag = { ix :: Int, tagIn :: WrEx tag }

type ChildSlots f tag =
    ( chainLink :: H.Slot (SomeSubQuery f tag) (Exists tag) (ChildIx tag)
    )

newtype TagLink tag a b = TagLink
    { tagIn  :: tag a
    , tagOut :: tag b
    }

type State tag a =
    { tagChain :: DSum tag (Chain (TagLink tag) a)
    }

data Action tag =
          AddLink (Exists tag)      -- ^ field: initial output of new field link
        | RemoveLink (Exists tag)   -- ^ field: new final output
        | TriggerUpdate { ix :: Int, outputType :: Exists tag }

data Query f tag a r =
        AskSelected (DSum tag (Chain f a) -> r)

instance functorQuery :: Functor (Query f tag a) where
    map f = case _ of
      AskSelected    g -> AskSelected (f <<< g)
instance applyQuery :: Apply (Query f tag a) where
    apply (AskSelected f) (AskSelected g) = AskSelected $ \x -> (f x) (g x)
instance applicativeQuery :: Applicative (Query f tag a) where
    pure x = AskSelected (\_ -> x)

data Output tag = ChainUpdate (Exists tag)      -- ^ new output type

component
    :: forall f tag a m i. GOrd tag => GShow tag
    => PickerMap f tag m
    -> tag a
    -> H.Component HH.HTML (Query f tag a) i (Output tag) m
component picker t0 =
  H.mkComponent
    { initialState: initialState t0
    , render: render picker t0
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction t0
        , handleQuery  = handleQuery t0
        }
    }

type WrappedQuery f tag = SomeQuery (Query f tag) tag

wrappedComponent
    :: forall f tag a m i. GOrd tag => GShow tag
    => PickerMap f tag m
    -> tag a                   -- ^ initial initial type
    -> H.Component HH.HTML (WrappedQuery f tag) i (Output tag) m
wrappedComponent picker t0 = HU.hoistQuery
    (unSomeQuery t0)
    (component picker t0)


initialState :: forall tag a i. tag a -> i -> State tag a
initialState t0 _ = { tagChain: t0 :=> C.nil }

render
    :: forall tag f m a. GOrd tag => GShow tag
    => PickerMap f tag m
    -> tag a            -- ^ first type
    -> State tag a
    -> H.ComponentHTML (Action tag) (ChildSlots f tag) m
render pickerMap t0 s = HH.div_ [
      HH.div [HU.classProp "op-list"] <<< A.catMaybes <<< flip mapWithIndex inList $ \i sT -> runExists (\t ->
          unwrap (runDProd pickerMap t) <#> \(Picker { component }) ->
            HH.slot _chainLink { ix: i, tagIn: mkWrEx t }
              (HU.hoistQuery (unSomeQuery t) component)
              unit
              $ \tOut -> Just (TriggerUpdate { ix: i, outputType: tOut })
      ) sT
    , HH.div [HU.classProp "op-buttons"] $ A.catMaybes $ [
        runExists (\lastOut ->
          unwrap (runDProd pickerMap lastOut) <#> \(Picker { initialOut }) ->
            HH.button [
              HP.type_ HP.ButtonButton
            , HE.onClick (\_ -> Just (AddLink initialOut))
            , HU.classProp "add-op"
            ]
            [ HH.text "Add Operation" ]
        ) (fromMaybe (mkExists t0) (A.last outList))
      , A.last inList <#> \tLast ->
          HH.button [
            HP.type_ HP.ButtonButton
          , HE.onClick (\_ -> Just (RemoveLink tLast))
          , HU.classProp "remove-op"
          ]
          [ HH.text "Remove Operation" ]
      ]
    ]
  where
    inList  = withDSum s.tagChain (\_ ->
                C.foldMapChain (\(TagLink t) -> [mkExists t.tagIn])
              )
    outList = withDSum s.tagChain (\_ ->
                C.foldMapChain (\(TagLink t) -> [mkExists t.tagOut])
              )

-- | output type of final component
lastOutput :: forall tag a. tag a -> Seq (Exists tag) -> Exists tag
lastOutput t0 chain = case Seq.last chain of
    Nothing -> mkExists t0
    Just t  -> t

handleAction
    :: forall f tag m a. Decide tag => GShow tag
    => tag a    -- ^ initial type
    -> Action tag
    -> H.HalogenM (State tag a) (Action tag) (ChildSlots f tag) (Output tag) m Unit
handleAction t0 act = do
    case act of
      AddLink newInitialOutput -> runExists (\tNewOut ->
        H.modify_ $ \st ->
          { tagChain: withDSum st.tagChain (\tOldOut c -> tNewOut :=> C.snoc c
              (TagLink { tagIn: tOldOut, tagOut: tNewOut})
            )
          }
      ) newInitialOutput
      RemoveLink _ -> H.modify_ $ \st ->
        { tagChain : withDSum st.tagChain (\_ c ->
            case C.unsnoc c of
              Left _    -> t0 :=> C.Nil refl       -- this shouldn't ever happen, type error
              Right us  -> withAp us (\xs (TagLink tl) ->   -- ^ this should be equal to the removelink field
                tl.tagIn :=> xs
              )
          )
        }
      TriggerUpdate { ix, outputType } -> do
        st <- H.get
        withDSum st.tagChain (\_ c -> withAp (C.splitAt ix c) (\xs ys ->
            -- A -> B -> C -> D -> E
            -- | 0  | 1  | 2  | 3  |
            -- { xs | ys           }   <- let's say component 1 changes
            -- { xs | z  : zs      }
            --
            -- if its new output is not C (the output of ix 1), then we
            -- replace with:
            --
            -- A -> B -> C'
            -- | 0  | 1  |
            --
            -- and keep only the head of ys (z)
            --
            -- so everything depends on the head of ys, aka z
            case ys of
              C.Nil refl -> pure unit       -- <- hmm.... this shouldn't ever happen
              C.Cons ays -> withAp ays (\(TagLink z) zs ->
                  runExists (\tNew ->
                    case decide tNew z.tagOut of
                      Nothing ->
                        let zNew = TagLink { tagIn: z.tagIn, tagOut: tNew }
                        in  H.put { tagChain: tNew :=> C.snoc xs zNew }
                      Just refl -> pure unit        -- we're all good
                  ) outputType
                )
          )
        )
    s <- H.get
    -- traceM ( A.fold $
    --     withDSum s.tagChain (\_ ->
    --             C.foldMapChain (\(TagLink t) -> ["{", gshow t.tagIn, " -> ",
    --                            gshow t.tagOut, "}"])
    --           )
    --   )
    H.raise <<< ChainUpdate =<< lastTag t0

data AssembleError tag =
      AEOutType
        { ix       :: Int
        , expected :: Exists tag
        , actual   :: Exists tag
        }
    | AEInType
        { ix       :: Int
        , expected :: Exists tag
        , actual   :: Exists tag
        }
    | AENoResponse
        { ix       :: Int
        , expected :: Exists tag
        }

instance showAssembleError :: GShow tag => Show (AssembleError tag) where
    show = case _ of
      AEOutType r -> A.fold
        [ "Output type mismatch at index " , show r.ix
        , "; Expected " , runExists gshow r.expected
        , ", got " , runExists gshow r.expected
        ]
      AEInType r -> A.fold
        [ "Input type mismatch at index " , show r.ix
        , "; Expected " , runExists gshow r.expected
        , ", got " , runExists gshow r.expected
        ]
      AENoResponse r -> A.fold
        [ "Component at index " , show r.ix
        , " outputted no result; expecting ", runExists gshow r.expected
        ]

handleQuery
    :: forall f tag m a o r. Decide tag => GOrd tag => GShow tag
    => tag a
    -> Query f tag a r
    -> PhatMonad f tag a o m (Maybe r)
handleQuery t0 = case _ of
    AskSelected f -> assembleChain t0 <#> case _ of
      Left  e  -> trace (show e) (const Nothing)     -- how to log error?
      Right xs -> Just (f xs)

type PhatMonad f tag a o m = H.HalogenM (State tag a) (Action tag) (ChildSlots f tag) o m

assembleChain
    :: forall f tag a m o. GOrd tag
    => tag a
    -> PhatMonad f tag a o m (Either (AssembleError tag) (DSum tag (Chain f a)))
assembleChain t0 = do
    s0 <- H.get
    withDSum s0.tagChain (\tLast chain ->
      runExceptT <<< flip evalStateT 0 $ dsum tLast <$> C.hoistChainA go chain
    )
  where
    go  :: forall r s.
           TagLink tag r s
        -> StateT Int (ExceptT (AssembleError tag) (PhatMonad f tag a o m)) (f r s)
    go (TagLink { tagIn, tagOut }) = do
      i <- get
      modify_ (_ + 1)
      res <- lift $ lift $ H.query
        _chainLink
        { ix: i, tagIn: mkWrEx tagIn }
        (someQuery tagIn (SQ identity))
      case res of
        Nothing       -> throwError $ AENoResponse { ix: i, expected: mkExists tagIn }
        Just (Left e) -> throwError $ AEInType { ix: i, expected: mkExists tagIn, actual: e }
        Just (Right dsr) -> withDSum dsr (\t2 r ->
            case decide tagOut t2 of
              Nothing -> throwError $
                AEOutType { ix: i, expected: mkExists tagOut, actual: mkExists t2 }
              Just refl -> pure $ equivFromF refl r
          )

-- | last output
lastTag :: forall f tag a o m. tag a -> PhatMonad f tag a o m (Exists tag)
lastTag t0 = H.gets $ \s0 -> withDSum s0.tagChain (\tLast _ -> mkExists tLast)

someQuery
    :: forall f tag r a. Functor (f a)
    => tag a
    -> f a r
    -> SomeQuery f tag (Either (Exists tag) r)
someQuery t q = SomeQuery
    { typeMismatch: Left
    , typeMatch: \f -> f t (Right <$> q)
    }

unSomeQuery
    :: forall f tag a r. Applicative (f a) => Decide tag
    => tag a
    -> SomeQuery f tag r
    -> f a r
unSomeQuery tExpected (SomeQuery {typeMatch, typeMismatch}) = typeMatch (\tActual sq ->
    case decide tExpected tActual of
      Nothing   -> pure (typeMismatch (mkExists tExpected))
      Just refl -> equivFromF2 refl sq
  )


_chainLink :: SProxy "chainLink"
_chainLink = SProxy

-- foreign import logMe :: forall a. a -> String
