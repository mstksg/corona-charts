
module Corona.Chart.UI.DateOp where

-- import Prelude

-- import CSS as CSS
-- import Control.Alternative
-- import Control.Monad.Except
-- import Control.Monad.Maybe.Trans
-- import Control.Monad.State.Class
-- import Control.MonadZero as MZ
-- import Corona.Chart
-- import D3.Scatter.Type as D3
-- import Data.Array as A
-- import Data.Boolean
-- import Data.Either
-- import Data.Exists
-- import Data.Foldable
-- import Data.Functor
-- import Data.Functor.Compose
-- import Data.FunctorWithIndex
-- import Data.FunctorWithIndex
-- import Data.Int.Parse
-- import Data.List as L
-- import Data.Map (Map)
-- import Data.Map as M
-- import Data.Maybe
-- import Data.Newtype
-- import Data.Ord
-- import Data.Sequence (Seq)
-- import Data.Sequence as Seq
-- import Data.Set (Set)
-- import Data.Set as S
-- import Data.String as String
-- import Data.String.Pattern as String
-- import Data.String.Regex as Regex
-- import Data.String.Regex.Flags as Regex
-- import Data.Symbol (SProxy(..))
-- import Data.Traversable
-- import Data.Tuple
-- import Effect.Class
-- import Effect.Class.Console (log)
-- import Halogen as H
-- import Halogen.HTML as HH
-- import Halogen.HTML.CSS as HC
-- import Halogen.HTML.Events as HE
-- import Halogen.HTML.Properties as HP
-- import Halogen.Util as HU
-- import Type.Chain (Chain)
-- import Type.Chain as C
-- import Type.DMap (DMap)
-- import Type.DMap as DM
-- import Type.DProd
-- import Type.DSum
-- import Type.Equiv
-- import Type.Some
-- import Undefined
-- import Web.DOM.Element as W
-- import Web.DOM.HTMLCollection as HTMLCollection
-- import Web.HTML.HTMLOptionElement as Option
-- import Web.HTML.HTMLSelectElement as Select
-- import Web.UIEvent.MouseEvent as ME

-- -- data NumericOp =
-- --         ODelta
-- --       | OPGrowth
-- --       | OWindow

-- -- derive instance eqNumericOp :: Eq NumericOp

-- data DateOp = ODaysSince

-- type State =
--     { currentOp :: NumericOp
--     , windowSize :: Int
--     }

-- -- data Output = ChangeEvent (Exists D3.SType)

-- -- data Action = SetOp NumericOp
-- --             | SetWindow Int

-- -- data Query a r = QueryOp (DSum D3.SType (Operation a) -> r)

-- -- component
-- --     :: forall a i m.
-- --        D3.NType a
-- --     -> H.Component HH.HTML (Query a) i Output m
-- -- component t0 =
-- --   H.mkComponent
-- --     { initialState
-- --     , render
-- --     , eval: H.mkEval $ H.defaultEval
-- --         { handleAction = handleAction t0
-- --         , handleQuery  = handleQuery t0
-- --         }
-- --     }

-- -- initialState :: forall i. i -> State
-- -- initialState _ =
-- --     { currentOp:  ODelta
-- --     , windowSize: 1
-- --     }

-- -- render :: forall m. State -> H.ComponentHTML Action () m
-- -- render st = HH.div [HU.classProp "numeric-op"] $ A.catMaybes [
-- --       Just $ HH.div [HU.classProp "numeric-picker"] [
-- --         HH.select [ HE.onSelectedIndexChange (map SetOp <<< indexToOp) ] [
-- --           HH.option  [HP.selected true] [HH.text "Daily Change"]
-- --         , HH.option_ [HH.text "Percent Growth"]
-- --         , HH.option_ [HH.text "Moving Average"]
-- --         ]
-- --       ]
-- --     , if st.currentOp == OWindow
-- --         then Just $ HH.div [HU.classProp "window-picker"] [
-- --                HH.text "Window size (before and after): "
-- --              , HH.input [
-- --                  HP.type_ HP.InputNumber
-- --                , HP.value (show (st.windowSize))
-- --                , HE.onValueInput (map SetWindow <<< parseWindow)
-- --                ]
-- --              ]
-- --         else Nothing
-- --     ]
-- --   where
-- --     indexToOp = case _ of
-- --       0 -> Just ODelta
-- --       1 -> Just OPGrowth
-- --       2 -> Just OWindow
-- --       _ -> Nothing
-- --     parseWindow = map abs <<< flip parseInt (toRadix 10)

-- -- handleAction
-- --     :: forall a m.
-- --        D3.NType a
-- --     -> Action
-- --     -> H.HalogenM State Action () Output m Unit
-- -- handleAction t0 act = do
-- --     case act of
-- --       SetOp     o -> H.modify_ (_ { currentOp  = o })
-- --       SetWindow i -> H.modify_ (_ { windowSize = i })
-- --     assembleOp t0 >>= \ds ->
-- --       withDSum ds (\t _ ->
-- --         H.raise (ChangeEvent (mkExists t))
-- --       )

-- -- handleQuery
-- --     :: forall a o m r.
-- --        D3.NType a
-- --     -> Query a r
-- --     -> H.HalogenM State Action () o m (Maybe r)
-- -- handleQuery t0 = case _ of
-- --     QueryOp f -> Just <<< f <$> assembleOp t0

-- -- assembleOp
-- --     :: forall a o m.
-- --        D3.NType a
-- --     -> H.HalogenM State Action () o m (DSum D3.SType (Operation a))
-- -- assembleOp t0 = H.gets $ \st ->
-- --     case st.currentOp of
-- --       ODelta   -> D3.fromNType t0 `dsum` Delta   t0 refl
-- --       OPGrowth -> D3.sPercent     `dsum` PGrowth t0 refl
-- --       OWindow  -> runExists (\tf ->
-- --                toFractionalOut tf `dsum` Window  tf (st.windowSize)
-- --         ) (toFractional t0)

