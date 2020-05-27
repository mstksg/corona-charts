
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
import Data.Bundle
import Data.Dated (Dated(..))
import Data.Dated as D
import Data.FunctorWithIndex
import Data.Int
import Data.Lazy
import Data.Maybe
import Data.ModifiedJulianDay as MJD
import Data.Point
import Data.Tuple
import Debug.Trace
import Foreign.Object as O
import Global as G
import Math as M
import Type.Chain as C
import Type.Equiv
import Undefined


type ModelSpec = { fit :: ModelFit, tail :: Int, forecast :: Int }

toSeries2
    :: forall a b.
       Projection a
    -> Projection b
    -> TimeCounts Int
    -> Array (Point2D a b)
toSeries2 pX pY tc =
            D.datedValues
        -- <<< D.take len
        -- <<< D.dropBefore start
          $ pts
  where
    b = mkBundle tc []
    pts = lift2 (\x y -> {x, y})
        (bundlePrincipal $ applyProjection pX b)
        (bundlePrincipal $ applyProjection pY b)

fitTimeCounts
    :: ModelSpec
    -> TimeCounts Int
    -> { modelInfo :: Counts (Lazy D3.ModelRes), timeCounts :: TimeCounts Int }
fitTimeCounts info (TC tc) =
    { modelInfo: mapCounts (map (_.modelInfo)) fittedCounts
    , timeCounts: TC
        { counts: mapCounts
                (map (map round <<< D.datedValues <<< (_.results)))
                fittedCounts
        , timespan: info.tail + info.forecast
        , start: MJD.addDays (tc.timespan - info.tail) tc.start
        }
    }
  where
    fittedCounts :: Counts (Lazy { modelInfo :: D3.ModelRes, results :: Dated Number })
    fittedCounts = flip (mapCounts <<< map) tc.counts $ \xs ->
        modelBase info (Dated { start: tc.start, values: map toNumber xs })

modelBase
    :: ModelSpec
    -> Dated Number
    -> { modelInfo :: D3.ModelRes, results :: Dated Number }
modelBase { fit, tail, forecast } baseData = out
    { modelInfo = out.modelInfo
        { params = touchup out.modelInfo.params `O.union` paramTrans.params }
    }
  where
    paramTrans = modelFitTrans tail baseData fit
    touchup = case fit of
      LogFit
        | capInfinity -> O.delete "Date of Peak"
                     <<< O.delete "95% Date"
        | otherwise   -> identity
      DecFit        -- TODO: infinityk
        | capInfinity -> O.insert "Halving Time" (D3.someValue G.infinity)
                     <<< O.delete "95% Date"
        | otherwise   -> identity
      _      -> identity
    inf = D3.someValue G.infinity
    capInfinity = case O.lookup "Final Total" paramTrans.params of
      Nothing -> false
      Just c  -> D3.eqSomeValue c inf
    out = modelBaseData
            paramTrans.trans
            (modelFitParams fit)
            tail
            forecast
            baseData

modelFitParams
    :: ModelFit
    -> LinReg Number
    -> O.Object D3.SomeValue
modelFitParams = case _ of
    LinFit -> \lr -> O.singleton "Daily Change" (D3.someValue lr.beta)
    ExpFit -> \lr -> O.fromFoldable [
        Tuple "Daily % Growth"
              (D3.someValue (D3.Percent (M.exp lr.beta - 1.0)))
      , Tuple "Doubling Time (Days)"
              (D3.someValue (D3.Days (round (M.log 2.0 / lr.beta))))
      ]
    DecFit -> \lr -> O.fromFoldable [
        Tuple "Halving Time"
              (D3.someValue (D3.Days (round $ M.abs (M.log 2.0 / lr.beta))))
      , Tuple "95% Date"
              (D3.someValue (MJD.fromModifiedJulianDay (round $ (M.log 0.05 - lr.alpha) / lr.beta)))
      ]
    LogFit -> \lr -> O.fromFoldable [
        Tuple "Date of Peak"
              (D3.someValue (MJD.fromModifiedJulianDay (round (-lr.alpha / lr.beta))))
      , Tuple "95% Date"
              (D3.someValue (MJD.fromModifiedJulianDay
                  (round $ (M.log (0.05/0.95) - lr.alpha) / lr.beta)
                 ))
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
    -> { params :: O.Object D3.SomeValue
       , trans :: Transform Number
       }
modelFitTrans n dat = case _ of
    LinFit -> { params: O.empty, trans: idTrans }
    ExpFit -> { params: O.empty, trans: expTrans }
    DecFit -> finder idTrans decayTrans
    LogFit -> finder expTrans logisticTrans
    -- QuadFit -> { params: [], trans: quadTrans }
  where
    finder def mkT = case findCap mkT (D.takeEnd n dat) of
      Nothing -> {
          params: O.singleton "Final Total" (D3.someValue G.infinity)
        , trans: def
        }
      Just c  -> {
          params: O.singleton "Final Total" (D3.someValue c)
        , trans: mkT c
        }

-- | generate the model based on base data
modelBaseData
    :: Transform Number     -- ^ transformation
    -> (LinReg Number -> O.Object D3.SomeValue)     -- ^ params
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
