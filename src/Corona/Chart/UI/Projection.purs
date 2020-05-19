
module Corona.Chart.UI.Projection where

import Prelude

import Control.Monad.State.Class as State
import Corona.Chart
import Corona.Chart.UI.Op as Op
import Corona.JHU
import Corona.Marshal as Marshal
import D3.Scatter.Type (SType(..), NType(..), Scale(..), NScale(..))
import D3.Scatter.Type as D3
import Data.Array as A
import Data.Either
import Data.Exists
import Data.Functor.Compose
import Data.Maybe
import Data.Set (Set)
import Data.Set as S
import Data.Symbol (SProxy(..))
import Data.Tuple
import Debug.Trace
import Effect.Class
import Effect.Class.Console
import Foreign.Object as O
import Halogen as H
import Halogen.ChainPicker as ChainPicker
import Halogen.HTML as HH
import Halogen.HTML.Core as HH
import Halogen.HTML.Elements as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Scatter as Scatter
import Halogen.Util as HU
import Text.Parsing.StringParser as P
import Type.Ap
import Type.Chain as C
import Type.DProd
import Type.DSum
import Type.Equiv
import Type.GCompare
import Undefined


type State =
    { projection :: DSum SType Projection
    , numScale   :: NScale                   -- ^ date scale is always day
    }

type OpIx = { tagIn :: WrEx D3.SType }

type ChildSlots =
        ( opselect    :: H.Slot (ChainPicker.WrappedQuery SType Operation)
                            ChainPicker.Output
                            OpIx
        )

data Action =
        SetBase      (Exists BaseProjection)
      | SetOps
      | SetNumScale  NScale

data Output = Update State  -- meh because this could change by the time you get it

data Query r = 
      QueryAsk (State -> r)
    | QueryPut State r        -- ^ always re-raises

  -- QueryOp (State -> Tuple r State)

-- TODO: need a way to restrict output, and then maybe wrap it.  like the good
-- old days
component
    :: forall m. MonadEffect m
    => String
    -> H.Component HH.HTML Query State Output m
component label =
  H.mkComponent
    { initialState: identity
    , render: render label
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , handleQuery  = handleQuery
        }
    }

render
    :: forall m. MonadEffect m
    => String     -- ^ axis label
    -> State
    -> H.ComponentHTML Action ChildSlots m
render label aState = HH.div [HU.classProp "axis-options dialog"] [
      HH.h3_ [HH.text label]
    , HH.div [HU.classProp "base-projection"] [
        HH.span_ [HH.text "Base Projection"]
      , HH.select [HE.onSelectedIndexChange (map SetBase <<< indexToBase)] $
          allBaseProjections <#> \sbp -> runExists (\bp ->
            let isSelected = withDSum aState.projection (\_ pr ->
                    WrEx sbp == WrEx (baseProjection pr)
                  )
            in  HH.option [HP.selected isSelected] [HH.text (baseProjectionLabel bp)]
          ) sbp
      ]
    , HH.div [HU.classProp "axis-op-chain"] [
        HH.span_ [HH.text "Transformations"] 
      -- , HH.div [HU.classProp "possible-op-chain"] $ D3.allSType <#> \t ->
      --     HH.slot _opSelect
      --       {tagIn: mkWrEx tBase}
      --       (chainPicker tBase)
      --       unit
      --       $ \_ -> Just SetOps
          


      , withDSum aState.projection (\_ spr ->
          withProjection spr (\pr ->
            let tBase = baseType pr.base
            in  HH.slot _opselect
                  {tagIn: mkWrEx tBase}
                  (chainPicker tBase)
                  unit
                  $ \_ -> Just SetOps
          )
        )
      ]
    , HH.div [HU.classProp "axis-scale"] [
        HH.span_ [HH.text "Scale"]
      , withDSum aState.projection (\t _ ->
          case D3.toNType t of
            Left (Left _) ->
              HH.select_ [ HH.option [HP.selected true] [HH.text "Date"] ]
            Left (Right _) ->
              HH.select_ [ HH.option [HP.selected true] [HH.text "Days"] ]
            Right _ ->
              HH.select [HE.onSelectedIndexChange (map SetNumScale <<< indexToNScale)]
              [ HH.option_ [HH.text "Linear"]
              , HH.option  [HP.selected true] [HH.text "Log"]
              ]
        )
      ]
    ]
  where
    indexToBase :: Int -> Maybe (Exists BaseProjection)
    indexToBase = case _ of
      0 -> Just (mkExists bTime)
      1 -> Just (mkExists bConfirmed)
      2 -> Just (mkExists bDeaths)
      3 -> Just (mkExists bRecovered)
      _ -> Nothing
    indexToNScale :: Int -> Maybe NScale
    indexToNScale = case _ of
      0 -> Just (NScale (DProd (Linear <<< Right)))
      1 -> Just (NScale (DProd Log))
      _ -> Nothing

chainPicker
    :: forall a i m. MonadEffect m
    => SType a  -- ^ initial initial type
    -> H.Component HH.HTML (ChainPicker.WrappedQuery SType Operation) i ChainPicker.Output m
chainPicker t0 = ChainPicker.wrappedComponent t0 Op.pickerMap

handleAction
    :: forall m. MonadEffect m
    => Action
    -> H.HalogenM State Action ChildSlots Output m Unit
handleAction act = do
    case act of
      SetBase sb -> H.modify_ $ \st ->
          st { projection = runExists (flip setBase st.projection) sb }
      SetOps -> do
        -- log "heyo"
        dsp <- H.gets (_.projection)
        withDSum dsp (\tOut proj -> withProjection proj (\pr -> do
            let tIn = baseType pr.base
            qres <- H.query _opselect { tagIn: mkWrEx tIn } $
              ChainPicker.WQGet (\tA tB chain ->
                case decide tA tIn of
                  Nothing -> Left (mkExists tA)
                  Just r  -> Right (tB :=> equivToF2 r chain)
              )
            case qres of
              Nothing       -> log "no response from component"
              Just (Left e) -> log $ "type mismatch: " <> runExists show e
              Just (Right dsc) -> withDSum dsc (\tNewOut chain -> do
                log $ show chain
                H.modify_ $ \st ->
                  st { projection = tNewOut :=> projection
                         { base: pr.base, operations: chain }
                     }
              )
        )
      )
      SetNumScale s -> H.modify_ $ _ { numScale = s }
    -- log <<< show =<< H.get
    H.raise <<< Update =<< H.get

handleQuery
    :: forall a c o m r. MonadEffect m
    => Query r
    -> H.HalogenM State a ChildSlots Output m (Maybe r)
handleQuery = case _ of 
    QueryAsk f -> Just <$> State.gets f
    QueryPut s next -> do
      H.put s   -- we are double-putting?
      withDSum s.projection (\tOut sp -> withProjection sp (\p -> do
          let tBase = baseType p.base
              decorated = decorateOpChain tBase p.operations
          res <- H.query _opselect { tagIn: mkWrEx tBase } $
            ChainPicker.WQPut (dsum2 tBase tOut decorated) identity
            -- i think it isn't finding the index bc it hasn't been allocated
            -- yet.
          case res of
            Nothing -> warn "no response from ChainPicker"
            Just (Just t) -> runExists (\t' ->
              log $ "chainpicker of is the wrong input type (" <> gshow t' <> ") according to its index " <> gshow tBase
            ) t
            Just Nothing -> pure unit
        )
      )
      H.raise (Update s)
      pure (Just next)

decorateOpChain
    :: forall a b.
       SType a
    -> C.Chain Operation a b
    -> C.Chain (ChainPicker.TaggedLink SType Operation) a b
decorateOpChain tagIn = case _ of
    C.Nil r -> C.Nil r
    C.Cons rap -> withAp rap (\link xs ->
      let tagOut = operationType link tagIn
      in  C.cons (ChainPicker.TaggedLink {tagIn, tagOut, link})
                 (decorateOpChain tagOut xs)
    )

stateParser :: P.Parser State
stateParser = do
    proj  <- Marshal.parse1
    scale <- Marshal.parse
    pure
      { projection: proj
      , numScale: scale
      }

stateSerialize :: State -> String
stateSerialize p = Marshal.serialize p.projection
                <> Marshal.serialize p.numScale


_opselect :: SProxy "opselect"
_opselect = SProxy

