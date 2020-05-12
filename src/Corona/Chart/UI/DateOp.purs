
module Corona.Chart.UI.DateOp where

import Prelude

import Corona.Chart
import D3.Scatter.Type as D3
import Data.Either
import Data.Exists
import Data.Functor.Product
import Data.Int
import Data.Maybe
import Data.ModifiedJulianDay (Day)
import Data.Number as N
import Data.Symbol (SProxy(..))
import Data.Tuple
import Effect.Class
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS as HC
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Util as HU
import Type.Chain as C
import Type.DProd
import Type.DSum
import Type.Equiv

-- we cannot do custom projection until we find a way to limit the output of
-- Projection widget
type State =
    { baseProjection :: BaseProjection Int
    , condition :: Condition Int
    }

data Output = ChangeEvent (Exists D3.SType)

data Action = SetBase (BaseProjection Int)
            | SetCond (Condition Unit)
            | SetLimit Int

data Query r = QueryOp (DSum D3.SType (Operation Day) -> r)

component
    :: forall a i m.
       H.Component HH.HTML Query i Output m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , handleQuery  = handleQuery
        }
    }

initialState :: forall i. i -> State
initialState _ =
    { baseProjection: bConfirmed
    , condition: AtLeast 100
    }
    -- { projCond: dsum D3.nInt $ Product $
    --     Tuple (projection { base: bConfirmed, operations: C.nil })
    --           (AtLeast 100)
    -- }

allCondType :: Array (Condition Unit)
allCondType = [AtLeast unit, AtMost unit]

intBaseProjections :: Array (BaseProjection Int)
intBaseProjections = [ bConfirmed, bDeaths, bRecovered ]

render :: forall m. State -> H.ComponentHTML Action () m
render st = HH.div [HU.classProp "date-op"] [
      HH.div [HU.classProp "date-picker"] [
        HH.select_ [
          HH.option_ [HH.text "Days since..."]
        ]
      ]
    , HH.div [HU.classProp "condproj-picker"] [
        HH.div [HU.classProp "condproj-proj-picker"] [
          HH.select [HE.onSelectedIndexChange (map SetBase <<< indexToBase)] $
            intBaseProjections <#> \bp -> 
              let isSelected = bp == st.baseProjection
              in  HH.option [HP.selected isSelected] [HH.text (baseProjectionLabel bp)]
          ]
        , HH.div [HU.classProp "condproj-cond-picker"] [
            HH.select [HE.onSelectedIndexChange (map SetCond <<< indexToCond)] $
              allCondType <#> \c ->
                let isSelected = c == void st.condition
                in  HH.option [HP.selected isSelected] [HH.text (condString c)]
          ]
        , HH.div [HU.classProp "condproj-num-picker"] [
            HH.input [
              HP.type_ HP.InputNumber
            , HP.value (show (conditionValue st.condition))
            , HE.onValueInput (map (SetLimit <<< round) <<< N.fromString)
            ]
        ]
      ]
    ]
  where
    condString = case _ of
      AtLeast _ -> "...is at least..."
      AtMost  _ -> "...is at most..."
    indexToCond :: Int -> Maybe (Condition Unit)
    indexToCond = case _ of
      0 -> Just (AtLeast unit)
      1 -> Just (AtMost unit)
      _ -> Nothing
    indexToBase :: Int -> Maybe (BaseProjection Int)
    indexToBase = case _ of
      0 -> Just bConfirmed
      1 -> Just bDeaths
      2 -> Just bRecovered
      _ -> Nothing

handleAction
    :: forall a m.
       Action
    -> H.HalogenM State Action () Output m Unit
handleAction act = do
    case act of
      SetBase bp -> H.modify_ $ _ { baseProjection = bp }
      SetCond cu -> H.modify_ $ \st ->
        st { condition = conditionValue st.condition <$ cu }
      SetLimit v -> H.modify_ $ \st ->
        st { condition = v <$ st.condition }
    H.raise (ChangeEvent (mkExists D3.sDays))

handleQuery
    :: forall o m r.
       Query r
    -> H.HalogenM State Action () o m (Maybe r)
handleQuery = case _ of
    QueryOp f -> Just <<< f <$> assembleOp

assembleOp
    :: forall o m.
       H.HalogenM State Action () o m (DSum D3.SType (Operation Day))
assembleOp = H.gets $ \st ->
    let pr = projection { base: st.baseProjection, operations: C.nil }
    in  D3.sDays :=>
          DaysSince refl refl (D3.nInt :=> Product (Tuple pr st.condition))

_projection :: SProxy "projection"
_projection = SProxy

