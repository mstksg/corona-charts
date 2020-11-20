
module Corona.Chart.Assemble where

import Prelude

import Control.Apply
import Corona.Chart
import Corona.Chart.Model
import Corona.Data.Type
import D3.Scatter.Type
import Data.Array as A
import Data.Bundle
import Data.Dated (Dated)
import Data.Dated as D
import Data.Exists
import Debug.Trace
import Data.Functor.Compose
import Data.Lazy
import Data.Lens
import Data.Map (Map)
import Data.Map as M
import Data.Maybe
import Data.Newtype
import Data.Point
import Data.Set (Set)
import Data.Tuple
import Foreign.Object as O
import Prelude
import Undefined

-- | this throws away the model data
toScatterPlot
    :: forall a b c d.
       CoronaData
    -> Array ModelSpec
    -> PointF ProjScale a b c d
    -> Set Region
    -> ScatterPlot a b c d
toScatterPlot dat mspecs pss ctrys =
    { axis   : hoistPointF toAxisConf pss
    , series : flip A.mapMaybe (A.fromFoldable ctrys) $ \ctry -> do
        cdat <- O.lookup ctry dat.dat
        let tcounts :: TimeCounts Int
            tcounts = mkTimeCounts dat.start cdat
            mirroredTup :: Map ModelFit (Tuple (Counts (Lazy ModelRes)) (TimeCounts Int))
            mirroredTup = M.fromFoldable $ mspecs <#> \mspec ->
              let ftc = fitTimeCounts mspec tcounts
              in  Tuple mspec.fit (Tuple ftc.modelInfo ftc.timeCounts)
            modelResMap :: Map ModelFit (Counts (Lazy ModelRes))
            modelResMap = fst <$> mirroredTup
            mirrored :: Map ModelFit (TimeCounts Int)
            mirrored = snd <$> mirroredTup
            values
                :: { principal :: Array (Point a b c d)
                   , mirrored  :: PointF (Compose (Map ModelFit) Dated) a b c d
                   }
            values = toSeries projections $ mkBundle tcounts mirrored
            xModels :: Map ModelFit (Dated a)
            xModels = unwrap values.mirrored.x
            yModels :: Map ModelFit (Dated b)
            yModels = unwrap values.mirrored.y
            modelOutMaps = M.intersectionWith
              (\xs ys ->
                  D.datedValues $ lift2 (\x y -> {x, y}) xs ys
              ) xModels yModels
        pure
          { name: ctry
          , values: values.principal
          , modelfits: map reshuffle <<< M.toUnfoldable $ M.intersectionWith
                    (\cdr pts -> Tuple (assembleCdr cdr) pts)
                    modelResMap
                    modelOutMaps
          }
    }
  where
    projections = hoistPointF (\(PS ps) -> ps.projection) pss
    bX          = baseProjection projections.x
    bY          = baseProjection projections.y
    assembleCdr
        :: Counts (Lazy ModelRes)
        -> O.Object ModelRes
    assembleCdr cdr = map force <<< O.fromFoldable $ flip A.mapMaybe [bX, bY] $
        runExists (\bp ->
          Tuple (baseProjectionLabel bp) <$> case bp of
            Time      _ -> Nothing
            Confirmed _ -> Just cdr.confirmed
            Deaths    _ -> Just cdr.deaths
            Recovered _ -> Just cdr.recovered
            Active    _ -> Just cdr.active
        )
    reshuffle
        :: Tuple ModelFit (Tuple (O.Object ModelRes) (Array (Point2D a b)))
        -> FitData a b
    reshuffle (Tuple fit (Tuple info values)) =
        { fit, info, values }

