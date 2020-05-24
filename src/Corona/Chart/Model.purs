
module Corona.Chart.Model where

import Prelude

import Control.Apply
import Control.MonadZero as MZ
import Corona.Analyze.LinReg
import Corona.Analyze.Search
import Corona.Analyze.Transform
import Debug.Trace
import Corona.Chart
import Corona.Data.Type
import D3.Scatter.Type as D3
import Data.Array as A
import Data.Dated (Dated(..))
import Data.Dated as D
import Data.FunctorWithIndex
import Data.Int
import Data.Lazy
import Data.Maybe
import Data.ModifiedJulianDay as MJD
import Global as G
import Math as M
import Type.Chain as C
import Type.Equiv
import Undefined

data ModelFit = LinFit
              | ExpFit
              | LogFit

modelFitLabel :: ModelFit -> String
modelFitLabel = case _ of
    LinFit -> "Linear"
    ExpFit -> "Exponential"
    LogFit -> "Logistic"

type ModelSpec = { fit :: ModelFit, tail :: Int, extent :: Int }

toSeries2
    :: forall a b c d.
       Projection a
    -> Projection b
    -> LazyTimeCounts Int
    -> Array (D3.Point2D a b)
toSeries2 pX pY ps = D.datedValues $
    lift2 (\x y -> {x, y})
        (applyLazyProjection pX ps)
        (applyLazyProjection pY ps)

applyLazyBaseProjection
    :: forall a.
       BaseProjection a
    -> LazyTimeCounts Int
    -> Dated a
applyLazyBaseProjection = case _ of
    Time      r -> \tc -> D.generate tc.start tc.timespan (equivFrom r)
    Confirmed r -> \tc -> equivFromF r $
        Dated { start: tc.start, values: force $ tc.counts.confirmed }
    Deaths    r -> \tc -> equivFromF r $
        Dated { start: tc.start, values: force $ tc.counts.deaths }
    Recovered r -> \tc -> equivFromF r $
        Dated { start: tc.start, values: force $ tc.counts.recovered }

applyLazyProjection
    :: forall a.
       Projection a
    -> LazyTimeCounts Int
    -> Dated a
applyLazyProjection spr allDat = withProjection spr (\pr ->
          applyOperations pr.operations <<< applyLazyBaseProjection pr.base
        ) allDat

type LazyTimeCounts a =
    { start     :: MJD.Day
    , timespan  :: Int          -- ^ length - 1
    , counts    :: Counts (Lazy (Array a))
    }

-- | this fits all the counts, even the ones we never use.  
fitTimeCounts
    :: ModelSpec
    -> TimeCounts Int
    -> { modelInfo :: Counts (Lazy D3.ModelRes), timeCounts :: LazyTimeCounts Int }
fitTimeCounts info tc =
    { modelInfo: mapCounts (map (_.modelInfo)) fittedCounts
    , timeCounts: tc
        { counts = mapCounts
                (map (map round <<< D.datedValues <<< (_.results)))
                fittedCounts
        }
    }
  where
    fittedCounts :: Counts (Lazy { modelInfo :: D3.ModelRes, results :: Dated Number })
    fittedCounts = flip mapCounts tc.counts $ \xs ->
        defer $ const $ modelBase info (Dated { start: tc.start, values: map toNumber xs })

modelBase
    :: ModelSpec
    -> Dated Number
    -> { modelInfo :: D3.ModelRes, results :: Dated Number }
modelBase { fit, tail, extent } baseData = out
    { modelInfo = out.modelInfo
        { params = out.modelInfo.params <> paramTrans.params }
    }
  where
    paramTrans = modelFitTrans tail baseData fit
    out = modelBaseData
            paramTrans.trans
            (modelFitParams fit)
            tail
            extent
            baseData

modelFitParams
    :: ModelFit
    -> LinReg Number
    -> Array D3.Param
modelFitParams = case _ of
    LinFit -> \lr -> [{ name: "Daily Change", value: D3.someValue lr.beta }]
    ExpFit -> \lr -> [
        { name: "Daily % Growth"
        , value: D3.someValue (M.exp lr.beta - 1.0)
        }
      , { name: "Doubling Time"
        , value: D3.someValue (D3.Days (round (M.log 2.0 / lr.beta)))
        }
      ]
    LogFit -> \lr -> [
        { name: "Peak Date"
        , value: D3.someValue (MJD.fromModifiedJulianDay (round (-lr.alpha / lr.beta)))
        }
      ]

modelFitTrans
    :: Int          -- ^ number of days to take from end
    -> Dated Number
    -> ModelFit
    -> { params :: Array D3.Param
       , trans :: Transform Number
       }
modelFitTrans n dat = case _ of
    LinFit -> { params: [], trans: idTrans }
    ExpFit -> { params: [], trans: expTrans }
    LogFit -> case findLogisticCap (D.takeEnd n dat) of
      Nothing -> { params: [{ name: "Cap", value: D3.someValue G.infinity }]
                 , trans: expTrans
                 }
      Just c  -> { params: [{ name: "Cap", value: D3.someValue c }]
                 , trans: logisticTrans c
                 }

-- | this only works for linear and exponential models.
modelBaseData
    :: Transform Number     -- ^ transformation
    -> (LinReg Number -> Array D3.Param)     -- ^ params
    -> Int                  -- ^ points to take from end
    -> Int                  -- ^ points to extend into future
    -> Dated Number
    -> { modelInfo :: D3.ModelRes, results :: Dated Number }
modelBaseData tr mkP n m xs =
    { modelInfo: { params: mkP linReg, r2 }
    , results: D.generate
          (MJD.addDays (-n) (D.datedStart xs))
          (n + m)
          (applyLinRegTrans tr linReg <<< toNumber <<< MJD.toModifiedJulianDay)
    }
  where
    {linReg, r2} = linRegData tr n xs

-- | this only works for linear and exponential models.
linRegData
    :: Transform Number     -- ^ transformation
    -> Int                  -- ^ points to take from end
    -> Dated Number
    -> { linReg :: LinReg Number, r2 :: Number }
linRegData tr n =
            linRegTrans tr
        <<< D.datedValues
        <<< mapWithIndex prepare
        <<< D.takeEnd n
  where
    prepare i v = {
        x: toNumber (MJD.toModifiedJulianDay i)
      , y: v
      }

findLogisticCap
    :: Dated Number               -- ^ take the points from the end already
    -> Maybe Number               -- ^ logistic cap
findLogisticCap xs = do
    capMin <- (_ + 1.0) <$> A.last (D.datedValues xs)
    MZ.guard (capMin > 0.0)
    bisectionExtreme
        1e-7    -- epsilon for precision
        0.5
        (M.log <<< (1.0 - _) <<< getR2)
        capMin
        (capMin * 50.0) -- probably a reasonable upper bound. if too high
                        -- then floating point precision errors
  where
    vals = D.datedValues $ flip mapWithIndex xs $ \i v ->
                { x: toNumber (MJD.toModifiedJulianDay i)
                , y: v
                }
    getR2 cap = (linRegTrans (logisticTrans cap) vals).r2
