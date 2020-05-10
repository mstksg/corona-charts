
module Halogen.ChainPicker where

import Prelude

import CSS as CSS
import Control.Alternative
import Control.Monad.Except
import Control.Monad.Maybe.Trans
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
import Halogen.Util as HU
import Type.Chain (Chain)
import Type.Chain as C
import Type.DMap (DMap)
import Type.DMap as DM
import Type.DProd
import Type.DSum
import Type.Equiv
import Type.Some
import Undefined
import Web.DOM.Element as W
import Web.DOM.HTMLCollection as HTMLCollection
import Web.HTML.HTMLOptionElement as Option
import Web.HTML.HTMLSelectElement as Select
import Web.UIEvent.MouseEvent as ME


-- returns an @f a b@ with the @b@ hidden
data SubQuery f tag a r = SQ    (DSum tag (f a) -> r)
                        | SWhat r

instance functorSubQuery :: Functor (SubQuery f tag a) where
    map f = case _ of
      SQ    g -> SQ (f <<< g)
      SWhat x -> SWhat (f x)

newtype SomeSubQuery f tag r
  = SSQ { typeMismatch :: Exists tag -> r
        , typeMatch    :: forall q. (forall a. tag a -> SubQuery f tag a r -> q) -> q
        }
      -- ^ should re really enforce?

someSubQuery
    :: forall f tag r a.
       tag a
    -> SubQuery f tag a r
    -> SomeSubQuery f tag (Either (Exists tag) r)
someSubQuery t q = SSQ
    { typeMismatch: Left
    , typeMatch: \f -> f t (Right <$> q)
    }

-- | Picker of chain link @f@ with input @a@
newtype Picker f tag m a = Picker (H.Component HH.HTML (SubQuery f tag a) Unit Unit m)

-- | A way to select an appropriate 'Picker' based on a given type tag
-- type PickerMap tag f q m = DMap tag (Picker f q m)
type PickerMap f tag m = DProd tag (Picker f tag m)

type ChildSlots f tag =
        ( chainLink :: H.Slot (SomeSubQuery f tag) Unit Int
        )

type State tag =
      { tagChain :: Seq (Exists tag)    -- ^ the *input* types of each link
      }

data Action =
          AddLink
        | RemoveLink
        | TriggerUpdate

data Query f tag a r =
        AskSelected (DSum tag (Chain f a) -> r)

data Output = ChainUpdate

component
     :: forall f tag a m i. Decide tag
     => PickerMap f tag m
     -> tag a
     -> H.Component HH.HTML (Query f tag a) i Output m
component picker t0 =
  H.mkComponent
    { initialState
    , render: render picker t0
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction t0
        , handleQuery  = handleQuery t0
        }
    }

initialState :: forall i tag. i -> State tag
initialState _ = { tagChain: Seq.empty }

render
    :: forall tag f m a. Decide tag
    => PickerMap f tag m
    -> tag a            -- ^ first type
    -> State tag
    -> H.ComponentHTML Action (ChildSlots f tag) m
render pickerMap t0 s = HH.div_ [
      HH.div [HU.classProp "op-list"] $ flip mapWithIndex tList $ \i sT -> runExists (\t ->
          let Picker comp = runDProd pickerMap t
          in  HH.slot _chainLink i (HU.hoistQuery (unSomeSubQuery t) comp) unit (const (Just TriggerUpdate))
      ) sT
    , HH.div [HU.classProp "op-buttons"] $ A.catMaybes $ [
        Just $ HH.button [
            HP.type_ HP.ButtonButton
          , HE.onClick (\_ -> Just AddLink)
          , HU.classProp "add-op"
          ]
          [ HH.text "Add Operation" ]
      , if Seq.null s.tagChain
          then Nothing
          else Just $ HH.button [
              HP.type_ HP.ButtonButton
            , HE.onClick (\_ -> Just RemoveLink)
            , HU.classProp "remove-op"
            ]
            [ HH.text "Remove Operation" ]
      ]
    ]
  where
    tList = A.cons (mkExists t0) (A.fromFoldable s.tagChain)

unSomeSubQuery
    :: forall f tag a r. Decide tag
    => tag a
    -> SomeSubQuery f tag r
    -> SubQuery f tag a r
unSomeSubQuery tExpected (SSQ ssq) = ssq.typeMatch (\tActual sq ->
    case decide tExpected tActual of
      Nothing   -> SWhat (ssq.typeMismatch (mkExists tExpected))
      Just refl -> equivFromF2 refl sq
  )

handleAction
    :: forall f tag m a.
       tag a    -- ^ initial type
    -> Action
    -> H.HalogenM (State tag) Action (ChildSlots f tag) Output m Unit
handleAction t0 = case _ of
    AddLink -> do
      st <- H.get
      runExists (\tLast -> do
          res <- H.query _chainLink (length st.tagChain) $ someSubQuery
            tLast
            (SQ identity)
          case res of
            Nothing         -> pure unit    -- ^ component returned nothing?
            Just (Left _)   -> pure unit    -- ^ type mismatch error...should probably show
            Just (Right sx) -> withDSum sx (\tX _ ->
                H.put $ st {
                    tagChain = Seq.snoc st.tagChain (mkExists tX)
                  }
              )
        ) (lastTag st.tagChain)
    RemoveLink -> H.modify_ $ \st -> st
      { tagChain = fold (Seq.init (st.tagChain)) }
    TriggerUpdate -> H.raise ChainUpdate
  where
    lastTag chain = case Seq.last chain of
      Nothing -> mkExists t0
      Just t  -> t

data AssembleError tag =
      AECompType
        { ix       :: Int
        , expected :: Exists tag
        , actual   :: Exists tag
        }
    | AEChainType
        { ix       :: Int
        , expected :: Exists tag
        , actual   :: Exists tag
        }
    | AENoResponse
        { ix       :: Int
        , expected :: Exists tag
        }

handleQuery
    :: forall f tag m a o r. Decide tag
    => tag a
    -> Query f tag a r
    -> H.HalogenM (State tag) Action (ChildSlots f tag) o m (Maybe r)
handleQuery t0 = case _ of
    AskSelected f -> assembleChain t0 <#> case _ of
      Left  _  -> Nothing     -- how to log error?
      Right xs -> Just (f xs)

assembleChain
    :: forall f tag a m o. Decide tag
    => tag a
    -> H.HalogenM (State tag) Action (ChildSlots f tag) o m (Either (AssembleError tag) (DSum tag (Chain f a)))
assembleChain t0 = do
    s0 <- H.get
    runExceptT $ go t0 0 s0.tagChain
  where
    go  :: forall b.
           tag b
        -> Int
        -> Seq (Exists tag)
        -> ExceptT (AssembleError tag) (H.HalogenM (State tag) Action (ChildSlots f tag) o m) (DSum tag (Chain f b))
    go t1 i tags = do
      res <- lift $ H.query _chainLink i $ someSubQuery t1 (SQ identity)
      case res of
        Nothing       -> throwError $ AENoResponse { ix: i, expected: mkExists t1 }
        Just (Left e) -> throwError $ AECompType { ix: i, expected: mkExists t1, actual: e }
        Just (Right dsr) -> withDSum dsr (\t2 r ->
            case Seq.uncons tags of
              Nothing -> pure $ dsum t2 (C.singleton r)
              Just (Tuple st2' ts) -> runExists (\t2' ->
                  case decide t2 t2' of   -- TODO: i don't even need to check, do i?
                    Nothing -> throwError $
                      AEChainType { ix: i, expected: mkExists t2, actual: mkExists t2' }
                    Just refl -> do
                      drest <- go t2 (i + 1) ts
                      withDSum drest (\tLast rest ->
                        pure $ dsum tLast (C.cons r rest)
                      )
                ) st2'
          )

_chainLink :: SProxy "chainLink"
_chainLink = SProxy

