
module Corona.Chart.UI.Projection where

import Prelude

import Control.Monad.State.Class as State
import Corona.Chart
import Corona.Chart.UI.Op as Op
import Corona.JHU
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
import Halogen.MultiSelect as MultiSelect
import Halogen.Scatter as Scatter
import Halogen.Util as HU
import Type.Chain as C
import Type.DProd
import Type.DSum
import Type.Equiv
import Type.GCompare


-- type AxisState =
--     { projection :: DSum SType Projection
--     , numScale   :: NScale                   -- ^ date scale is always day
--     }

-- type Input =
--     { state       :: State
--     , defaultBase :: Exists BaseProjection
--     }

type State =
    { projection :: DSum SType Projection
    , numScale   :: NScale                   -- ^ date scale is always day
    }

type OpIx = { tagIn :: WrEx D3.SType }

type ChildSlots =
        ( multiselect :: H.Slot (MultiSelect.Query Country) (MultiSelect.Output Country) Unit
        , opselect    :: H.Slot (ChainPicker.WrappedQuery Operation SType)
                            (ChainPicker.Output SType)
                            OpIx
        )

data Action =
        SetBase      (Exists BaseProjection)
      | SetOps       (Exists SType)
      | SetNumScale  NScale

data Output = Update State

data Query r = QueryOp (State -> Tuple r State)

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
render label aState = HH.div [HU.classProp "axis-options"] [
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
      , withDSum aState.projection (\_ spr ->
          withProjection spr (\pr ->
            let tBase = baseType pr.base
            in  HH.slot _opselect
                  {tagIn: mkWrEx tBase}
                  (chainPicker tBase)
                  unit
                  $ \(ChainPicker.ChainUpdate u) ->
                      Just (SetOps u)
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

-- component
--     :: forall a i m. MonadEffect m
--     => D3.SType a
--     -> H.Component HH.HTML (Query a) i Output m

chainPicker
    :: forall a i m. MonadEffect m
    => SType a  -- ^ initial initial type
    -> H.Component HH.HTML (ChainPicker.WrappedQuery Operation SType) i (ChainPicker.Output SType) m
chainPicker = ChainPicker.wrappedComponent $ DProd (\t -> Compose <<< Just $
      ChainPicker.Picker
        { component: HU.trimapComponent
              (\(ChainPicker.SQ f) -> Op.QueryOp f)
              identity
              (\(Op.ChangeEvent o) -> o)
              (Op.component t)
        , initialOut: case t of
            SDay  _ -> mkExists D3.sDays
            SDays _ -> mkExists D3.sDays
            SInt  _ -> mkExists D3.sInt
            SNumber _ -> mkExists D3.sNumber
            SPercent _ -> mkExists D3.sPercent
        }
  )

handleAction
    :: forall m. MonadEffect m
    => Action
    -> H.HalogenM State Action ChildSlots Output m Unit
handleAction act = do
    case act of
      SetBase sb -> H.modify_ $ \st ->
          st { projection = runExists (flip setBase st.projection) sb }
      SetOps _ -> do
        dsp <- H.gets (_.projection)
        withDSum dsp (\tOut proj -> withProjection proj (\pr -> do
            let tIn = baseType pr.base
            qres <- H.query _opselect { tagIn: mkWrEx tIn }
                  $ ChainPicker.someQuery tIn ChainPicker.askSelected
            case qres of
              Nothing       -> log "no response from component"
              Just (Left e) -> log $ "type mismatch: " <> runExists show e
              Just (Right dsc) -> withDSum dsc (\tNewOut chain ->
                H.modify_ $ \st ->
                  st { projection = tNewOut :=> projection
                         { base: pr.base, operations: chain }
                     }
              )
        )
      )
      SetNumScale s -> H.modify_ $ _ { numScale = s }
    H.raise <<< Update =<< H.get

handleQuery
    :: forall a c o m r.
       Query r
    -> H.HalogenM State a c o m (Maybe r)
handleQuery = case _ of QueryOp f -> Just <$> State.state f
      -- ^ hm this is not ok

_opselect :: SProxy "opselect"
_opselect = SProxy

