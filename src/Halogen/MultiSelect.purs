
module Halogen.MultiSelect where

import Prelude

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
import Halogen.Util
import Web.DOM.Element as W
import Web.DOM.HTMLCollection as HTMLCollection
import Web.HTML.HTMLOptionElement as Option
import Web.HTML.HTMLSelectElement as Select
import Web.UIEvent.MouseEvent as ME

type Option a = { value :: a, label :: String }

type State a =
      { options  :: Array (Option a)
      , selected :: Set Int
      , filter   :: String
      }

data Action =
        AddValues
      | RemoveValue Int
      | RemoveAll
      | SetFilter String


data Query a r =
        AskSelected (Array a -> r)
        | SetState (State a -> { new :: State a, next :: r })

data Output a = SelectionChanged (Array a)

component :: forall a m. MonadEffect m => H.Component HH.HTML (Query a) (State a) (Output a) m
component =
  H.mkComponent
    { initialState: identity
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , handleQuery  = handleQuery
        }
    }

render :: forall a m. State a -> H.ComponentHTML Action () m
render st =
    HH.div [classProp "multiselect"] [
        HH.div [classProp "select-options"] [
          HH.input [
            HP.type_ HP.InputText
          , HP.placeholder "type to filter"
          , HE.onValueInput (Just <<< SetFilter)
          ]
        , HH.select [HP.multiple true, HP.ref selRef] $
            A.catMaybes $ mapWithIndex
              (\i o -> do
                  MZ.guard $ not (S.member i st.selected)
                  MZ.guard $ applyFilter st.filter o.label
                  pure     $ HH.option [HP.value (show i)] [HH.text o.label]
              )
              st.options
        , HH.button [
              HP.type_ HP.ButtonButton
            , HE.onClick (\_ -> Just AddValues)
            , classProp "add-button"
            ]
            [ HH.text "Add" ]
        ]
      , HH.div [classProp "selected"] $ 
          if null st.selected
            then [HH.text "(nothing selected yet)"]
            else renderSelected
      ]
  where
    renderSelected = [
        HH.ol [classProp "selected-list"] $
          map (\il -> HH.li [classProp "selected-item"] [
                  HH.span_ [HH.text il.label]
                , HH.text " "
                , HH.a [
                    HE.onClick $ \e ->
                      if ME.buttons e == 0
                        then Just (RemoveValue il.ix)
                        else Nothing
                  , classProp "remove-selected"
                  , HC.style $
                      CSS.key (CSS.Key (CSS.Plain "cursor")) "pointer"
                  ] [ HH.text "x" ]
                ]
              )
            selectedItems
      , HH.button [
          HE.onClick (\_ -> Just RemoveAll)
        , classProp "clear-button"
        ] [HH.text "Clear Selection"]
      ]
    selectedItems  = A.fromFoldable $ L.mapMaybe
      (\i -> map (\lv -> { ix: i, label: lv.label }) $ A.index st.options i) 
      (L.fromFoldable st.selected)

handleAction :: forall a o m. MonadEffect m => Action -> H.HalogenM (State a) Action () (Output a) m Unit
handleAction = case _ of
    AddValues      -> void <<< runMaybeT $ do
       e  <- MaybeT $ H.getRef selRef
       se <- maybe empty pure $ Select.fromElement e
       es <- liftEffect $ HTMLCollection.toArray =<< Select.selectedOptions se
       let opts = A.mapMaybe Option.fromElement es
       vals <- liftEffect $ traverse Option.value opts
       let valInts = S.fromFoldable (A.mapMaybe (flip parseInt (toRadix 10)) vals)
       modify_ $ \s -> s { selected = S.union valInts s.selected }
       lift raiseChange
    RemoveValue i  -> do
       modify_ $ \s -> s { selected = S.delete i s.selected }
       raiseChange
    RemoveAll      -> do
       modify_ $ \s -> { options: s.options, selected: S.empty, filter: s.filter}
       raiseChange
    SetFilter t    -> modify_ $ \s -> s { filter = t }

handleQuery :: forall a o m r. Query a r -> H.HalogenM (State a) Action () o m (Maybe r)
handleQuery = case _ of
    AskSelected f -> 
      Just <<< f <$> getSelected
    SetState f -> state $ \s -> let sr = f s in Tuple (Just sr.next) sr.new

getSelected :: forall a m. MonadState (State a) m => m (Array a)
getSelected = do
    st <- get
    pure $ A.fromFoldable $ L.mapMaybe
        (\i -> map (\lv -> lv.value) $ A.index st.options i) 
        (L.fromFoldable st.selected)
                                  

raiseChange :: forall a r c m. H.HalogenM (State a) r c (Output a) m Unit
raiseChange = H.raise <<< SelectionChanged =<< getSelected

selRef ∷ H.RefLabel
selRef = H.RefLabel "multiselect-sel"

applyFilter
    :: String     -- ^ filter
    -> String     -- ^ value
    -> Boolean
applyFilter s v
    | String.null s = true
    | otherwise     = String.contains (String.Pattern (norm s)) (norm v)
  where
    norm = case Regex.regex "\\W" Regex.global of
      Left  _ -> identity
      Right r -> Regex.replace r "" <<< String.toLower

