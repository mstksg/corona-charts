
module Corona.Chart.UI.Projection where

import Prelude

import Control.Monad.State.Class as State
import Control.Alternative
import Corona.Chart
import Corona.Chart.UI.Op as Op
import Corona.Data
import Corona.Marshal as Marshal
import D3.Scatter.Type (SType(..), NType(..), Scale(..), NScale(..))
import D3.Scatter.Type as D3
import Data.Array as A
import Data.Either
import Data.Exists
import Data.Foldable
import Data.Functor.Compose
import Data.Functor.Product
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

data NumScaleType = NSTLinear | NSTLog | NSTSymLog

derive instance eqNST :: Eq NumScaleType
derive instance ordNST :: Ord NumScaleType
instance showNST :: Show NumScaleType where
    show = case _ of
      NSTLinear -> "NSTLinear"
      NSTLog -> "NSTLog"
      NSTSymLog -> "NSTSymLog"


allNST :: Array NumScaleType
allNST = [ NSTLinear, NSTLog, NSTSymLog ]
nstLabel :: NumScaleType -> String
nstLabel = case _ of
    NSTLinear -> "Linear"
    NSTLog    -> "Logarithmic"
    NSTSymLog -> "Symmetric Log"


nstToScale :: forall a. SType a -> NumScaleType -> Boolean -> Scale a
nstToScale st = case D3.toNType st of
    Left (Left r)  -> \_ _ -> Date r
    Left (Right r) -> \_ -> D3.Linear (Left r)
    Right nt       -> case _ of
      NSTLinear -> D3.Linear (Right nt)
      NSTLog    -> \_ -> D3.Log nt
      NSTSymLog -> \_ -> D3.SymLog nt

scaleToNST
    :: forall a.
       Scale a
    -> { numScaleType :: Maybe NumScaleType, linearZero :: Maybe Boolean }
scaleToNST = case _ of
    Date _     -> { numScaleType: Nothing, linearZero: Nothing }
    Linear _ b -> { numScaleType: Just NSTLinear, linearZero: Just b }
    Log _      -> { numScaleType: Just NSTLog, linearZero: Nothing }
    SymLog _   -> { numScaleType: Just NSTSymLog, linearZero: Nothing }
    
type State =
    { projection   :: DSum SType Projection
    , numScaleType :: NumScaleType
    , linearZero   :: Boolean
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
      | SetNumScaleType  NumScaleType
      | SetLinearZero    Boolean

type Out = DSum SType (Product Projection Scale)

data Output = Update Out

data Query r = 
      QueryAsk (Out -> r)
    | QueryPut Out r        -- ^ always re-raises

  -- QueryOp (State -> Tuple r State)

-- TODO: need a way to restrict output, and then maybe wrap it.  like the good
-- old days
component
    :: forall m. MonadEffect m
    => String
    -> H.Component HH.HTML Query Out Output m
component label =
  H.mkComponent
    { initialState: \o -> withDSum o (\tOut (Product (Tuple proj sc)) ->
          let stout = scaleToNST sc
          in  { projection: tOut :=> proj
              , numScaleType: fromMaybe NSTLog stout.numScaleType
              , linearZero: fromMaybe false stout.linearZero
              }
        )
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
      , HH.select [HE.onSelectedIndexChange (map SetBase <<< (allBaseProjections A.!! _))] $
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
                  $ \_ -> Just SetOps
          )
        )
      ]
    , HH.div [HU.classProp "axis-scale"] $
        withDSum aState.projection (\t _ ->
          case D3.toNType t of
            Left  (Left _) -> []
            Left  (Right _) -> [
              HH.span_ [HH.text "Scale"]
            , linearZeroCheck
            ]
            Right _ -> fold [
              [ HH.span_ [HH.text "Scale"]
              , HH.select [HE.onSelectedIndexChange (map SetNumScaleType <<< (allNST A.!! _))] $
                  allNST <#> \nst ->
                    HH.option [HP.selected (nst == aState.numScaleType)] [HH.text (nstLabel nst)]
              ]
            , case aState.numScaleType of
                NSTLinear -> [linearZeroCheck]
                NSTLog    -> []
                NSTSymLog -> []
            ]
        )
    ]
  where
    linearZeroCheck = HH.div [HU.classProp "linear-zero"] [
      HH.input [
        HP.type_ HP.InputCheckbox
      , HP.checked aState.linearZero
      , HE.onChecked (Just <<< SetLinearZero)
      ]
    , HH.label_ [HH.text "Zero Axis"]
    ]


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
                updateProjection (tNewOut :=> projection
                                    { base: pr.base, operations: chain }
                                 )
              )
        )
      )
      SetNumScaleType nst ->
        H.modify_ $ _ { numScaleType = nst }
      SetLinearZero b ->
        H.modify_ $ _ { linearZero = b }
    H.raise <<< Update <<< assembleScale =<< H.get

assembleScale :: State -> Out
assembleScale st = withDSum st.projection (\t proj ->
    t :=> Product (Tuple proj (nstToScale t st.numScaleType st.linearZero))
  )

updateProjection
    :: forall m a i.
       DSum SType Projection
    -> H.HalogenM State i ChildSlots Output m Unit
updateProjection dproj = withDSum dproj (\tNew pNew ->
    H.modify_ $ \st0 ->
      withDSum st0.projection (\tOld pOld ->
        case decide tOld tNew of
          Just _  -> st0 { projection = dproj }
          Nothing ->
            { projection: dproj
            , numScaleType: fromMaybe st0.numScaleType $ defaultNST tNew
            , linearZero: false
            }
      )
  )




handleQuery
    :: forall a c o m r. MonadEffect m
    => Query r
    -> H.HalogenM State a ChildSlots Output m (Maybe r)
handleQuery = case _ of 
    QueryAsk f -> Just <<< f <$> State.gets assembleScale
    QueryPut s next -> withDSum s (\tOut (Product (Tuple sp scale)) -> do
      H.modify_ $ \st0 ->
        let sout = scaleToNST scale
        in  st0 { numScaleType = fromMaybe st0.numScaleType sout.numScaleType
                , linearZero   = fromMaybe st0.linearZero sout.linearZero
                }
      updateProjection (tOut :=> sp)
      withProjection sp (\p -> do
        let tBase     = baseType p.base
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
      H.raise (Update s)
      pure (Just next)
    )

defaultLZ :: Boolean
defaultLZ = false

defaultNST :: forall a. SType a -> Maybe NumScaleType
defaultNST = case _ of
    SDay  _ -> Nothing
    SDays _ -> Just NSTLinear
    SInt  _ -> Just NSTLog
    SNumber _ -> Just NSTLog
    SPercent _ -> Just NSTLinear

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

outParser :: P.Parser Out
outParser = do
  proj  <- Marshal.parse1
  withDSum proj (\t pr ->
    case t of
      SDay r -> Marshal.parse <#> \sc -> t :=> Product (Tuple pr (equivFromF r sc))
      SDays r -> Marshal.parse <#> \sc -> t :=> Product (Tuple pr (equivFromF r sc))
      SInt r -> Marshal.parse <#> \sc -> t :=> Product (Tuple pr (equivFromF r sc))
      SNumber r -> Marshal.parse <#> \sc -> t :=> Product (Tuple pr (equivFromF r sc))
      SPercent r -> Marshal.parse <#> \sc -> t :=> Product (Tuple pr (equivFromF r sc))
  )

outSerialize :: Out -> String
outSerialize p = withDSum p (\t (Product (Tuple proj sc)) ->
        Marshal.serialize1 proj
     <> case t of
          SDay r -> Marshal.serialize $ equivToF r sc
          SDays r -> Marshal.serialize $ equivToF r sc
          SInt r -> Marshal.serialize $ equivToF r sc
          SNumber r -> Marshal.serialize $ equivToF r sc
          SPercent r -> Marshal.serialize $ equivToF r sc

    ) 

outLabel :: Out -> String
outLabel p = withDSum p (\_ (Product (Tuple proj _)) -> projectionLabel proj)


_opselect :: SProxy "opselect"
_opselect = SProxy

