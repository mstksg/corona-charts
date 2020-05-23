
module Corona.Chart.Model where

import Prelude

import Control.MonadZero as MZ
import Corona.Analyze.LinReg
import Corona.Analyze.Search
import Corona.Analyze.Transform
import Corona.Chart
import Corona.Data.Type
import D3.Scatter.Type as D3
import Data.Array as A
import Data.Dated (Dated(..))
import Data.Dated as D
import Data.FunctorWithIndex
import Data.Int
import Data.Maybe
import Data.ModifiedJulianDay as MJD
import Global as G
import Math as M
import Type.Chain as C
import Type.Equiv
import Undefined

type Param = { name :: String, value :: D3.SomeValue }

type ModelRes = 
    { params :: Array Param
    , r2     :: Number
    }

data ModelFit = LinFit
              | ExpFit
              | LogFit

type ModelSpec = { fit :: ModelFit, tail :: Int, extent :: Int }

-- toModelFits
--     :: forall a b c d.
--        CoronaData
--     -> Projection a
--     -> Scale a
--     -> Projection b
--     -> Scale b
--     -> Projection c
--     -> Scale c
--     -> Projection d
--     -> Scale d
--     -> Set Region
--     -> ScatterPlot a b c d
-- -- toScatterPlot dat pX sX pY sY pZ sZ pT sT ctrys =
-- --         { xAxis  : toAxisConf pX sX
-- --         , yAxis  : toAxisConf pY sY
-- --         , zAxis  : toAxisConf pZ sZ
-- --         , tAxis  : toAxisConf pT sT
-- --         , series : flip A.mapMaybe (A.fromFoldable ctrys) $ \ctry -> do
-- --             cdat <- O.lookup ctry dat.counts
-- --             pure
-- --               { name : ctry
-- --               , values : toSeries pX pY pZ pT (mkTimeCounts dat.start cdat)
-- --               , modelfits : O.empty
-- --               }
-- --         }


-- type SeriesData a b c d =
--     { name      :: String
--     , values    :: Array (Point a b c d)
--     , modelfits :: O.Object (Array (Point2D a b))
--     }

-- -- | This is good but then we need a way to get rid of the extraneous
-- -- projection stuff
-- modelProjection
--     :: forall a.
--        ModelSpec
--     -> Projection a
--     -> TimeCounts Int
--     -> { modelInfo :: Counts ModelRes, results :: Dated a }
-- modelProjection info pr tc =
--     { modelInfo
--     , results: applyProjection pr timeCounts
--     }
--   where
--     { modelInfo, timeCounts } = fitTimeCounts info tc

fitTimeCounts
    :: ModelSpec
    -> TimeCounts Int
    -> { modelInfo :: Counts ModelRes, timeCounts :: TimeCounts Int }
fitTimeCounts info tc =
    { modelInfo: mapCounts (_.modelInfo) fittedCounts
    , timeCounts: tc { counts = mapCounts (map round <<< D.datedValues <<< (_.results)) fittedCounts }
    }
  where
    fittedCounts = flip mapCounts tc.counts $ \xs ->
        modelBase info (Dated { start: tc.start, values: map toNumber xs })

modelBase
    :: ModelSpec
    -> Dated Number
    -> { modelInfo :: ModelRes, results :: Dated Number }
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

overResults
    :: forall a b.
       (Dated a -> Dated b)
    -> { modelInfo :: ModelRes, results :: Dated a }
    -> { modelInfo :: ModelRes, results :: Dated b }
overResults f mi = mi { results = f mi.results }

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
    -> { modelInfo :: ModelRes, results :: Dated Number }
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
    capMin <- A.last (D.datedValues xs)
    MZ.guard (capMin > 0.0)
    let capMax = capMin * 10000.0
    bisectionExtreme
        1e-7    -- epsilon for precision
        1.0     -- one count before and after
        getR2
        capMin
        (capMin * 10000.0)      -- probably a reasonable upper bound
  where
    vals = D.datedValues $ flip mapWithIndex xs $ \i v ->
                { x: toNumber (MJD.toModifiedJulianDay i)
                , y: v
                }
    getR2 cap = (linRegTrans (logisticTrans cap) vals).r2
