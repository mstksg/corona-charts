
module Corona.Chart.Model where

import Prelude

import Control.Apply
import Control.MonadZero as MZ
import Corona.Analyze.LinReg
import Corona.Analyze.Search
import Corona.Analyze.Transform
import Corona.Chart
import Corona.Data.Type
import D3.Scatter.Type (ModelFit(..))
import D3.Scatter.Type as D3
import Data.Array as A
import Data.Dated (Dated(..))
import Data.Dated as D
import Data.FunctorWithIndex
import Data.Int
import Data.Lazy
import Data.Maybe
import Data.ModifiedJulianDay as MJD
import Debug.Trace
import Global as G
import Math as M
import Type.Chain as C
import Type.Equiv
import Undefined
import Undefined


type ModelSpec = { fit :: ModelFit, tail :: Int, forecast :: Int }

toSeries2
    :: forall a b.
       Projection a
    -> Projection b
    -> LazyTimeCounts Int
    -> Array (D3.Point2D a b)
toSeries2 pX pY tc =
            D.datedValues
        -- <<< D.take len
        -- <<< D.dropBefore start
          $ pts
  where
    pts = lift2 (\x y -> {x, y})
    -- TODO: this should be a seperate one that ignores stuff like
    -- take/restrict
    -- percentmax also needs to not take into account future dates
    -- day-count must as well
    -- maybe some of these we just...kll.  for example trnasforming to Date
    -- doesn't make sense  so drop the projections entirely
    -- yeah, dayCount would make sense to keep but it's not that simple...
    --
    -- maybe what needs ot happen is we have to transform the whole bundle
    -- alongside each other
        (applyLazyProjection pX tc)
        (applyLazyProjection pY tc)

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

-- hm this isn't lazy
fitTimeCounts
    :: ModelSpec
    -> TimeCounts Int
    -> { modelInfo :: Counts (Lazy D3.ModelRes), timeCounts :: LazyTimeCounts Int }
fitTimeCounts info tc =
    { modelInfo: mapCounts (map (_.modelInfo)) fittedCounts
    , timeCounts:
        { counts: mapCounts
                (map (map round <<< D.datedValues <<< (_.results)))
                fittedCounts
        , timespan: info.tail + info.forecast
        , start: MJD.addDays (tc.timespan - info.tail) tc.start
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
modelBase { fit, tail, forecast } baseData = out
    { modelInfo = out.modelInfo
        { params = out.modelInfo.params <> paramTrans.params }
    }
  where
    paramTrans = modelFitTrans tail baseData fit
    out = modelBaseData
            paramTrans.trans
            (modelFitParams fit)
            tail
            forecast
            baseData

modelFitParams
    :: ModelFit
    -> LinReg Number
    -> Array D3.Param
modelFitParams = case _ of
    LinFit -> \lr -> [{ name: "Daily Change", value: D3.someValue lr.beta }]
    ExpFit -> \lr -> [
        { name: "Daily % Growth"
        , value: D3.someValue (D3.Percent (M.exp lr.beta - 1.0))
        }
      , { name: "Doubling Time (Days)"
        , value: D3.someValue (D3.Days (round (M.log 2.0 / lr.beta)))
        }
      ]
    DecFit -> \lr -> [
        { name: "Growth Halving Time"
        , value: D3.someValue (D3.Days (round $ M.abs (M.log 2.0 / lr.beta)))
        }
      ]
    LogFit -> \lr -> [
        { name: "Peak Date"
        , value: D3.someValue (MJD.fromModifiedJulianDay (round (-lr.alpha / lr.beta)))
        }
      ]
    -- QuadFit -> \lr -> [
    --     { name: "Daily Change in Change"
    --     , value: D3.someValue (2.0 * lr.beta * lr.beta)
    --     }
    --   ]

modelFitTrans
    :: Int                  -- ^ number of days to take from end
    -> Dated Number
    -> ModelFit
    -> { params :: Array D3.Param
       , trans :: Transform Number
       }
modelFitTrans n dat = case _ of
    LinFit -> { params: [], trans: idTrans }
    ExpFit -> { params: [], trans: expTrans }
    DecFit -> finder idTrans decayTrans
    LogFit -> finder expTrans logisticTrans
    -- QuadFit -> { params: [], trans: quadTrans }
  where
    finder def mkT = case findCap mkT (D.takeEnd n dat) of
      Nothing -> {
          params: [{ name: "Cap", value: D3.someValue G.infinity }]
        , trans: def
        }
      Just c  -> {
          params: [{ name: "Cap", value: D3.someValue c }]
        , trans: mkT c
        }

-- | generate the model based on base data
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
          (D.datedStart ys)
          (n + m)
          (applyLinRegTrans tr linReg <<< toNumber <<< MJD.toModifiedJulianDay)
    }
  where
    ys = D.takeEnd n xs
    {linReg, r2} = linRegTrans tr
               <<< D.datedValues
               <<< mapWithIndex preparePoint
                 $ ys


preparePoint :: forall a. MJD.Day -> a -> { x :: Number, y :: a }
preparePoint i v = {
      x: toNumber (MJD.toModifiedJulianDay i)
    , y: v
    }

findCap
    :: (Number -> Transform Number)     -- ^ make a cap
    -> Dated Number               -- ^ just give the points you want to fit
    -> Maybe Number               -- ^ cap
findCap capper xs = do
    capMin <- (_ + 1.0) <$> A.last (D.datedValues xs)
    MZ.guard (capMin > 0.0)
    bisectionExtreme
        1e-7    -- epsilon for precision
        0.5
        (M.log <<< (1.0 - _) <<< getR2)
        capMin
        (capMin * 5.0) -- probably a reasonable upper bound. if too high
                        -- then floating point precision errors
  where
    vals = D.datedValues $ flip mapWithIndex xs $ \i v ->
      { x: toNumber (MJD.toModifiedJulianDay i)
      , y: v
      }
    getR2 cap = (linRegTrans (capper cap) vals).r2
