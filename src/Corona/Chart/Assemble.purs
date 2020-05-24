
module Corona.Chart.Assemble where

import Prelude

import Corona.Chart
import Corona.Chart.Model
import Corona.Data.Type
import D3.Scatter.Type
import Data.Array as A
import Data.Exists
import Data.Lazy
import Data.Maybe
import Data.Newtype
import Data.Set (Set)
import Data.Tuple
import Foreign.Object as O
import Prelude

-- | this throws away the model data
--
-- okay we ened to figure out how to display model data because the r2,
-- parameters, etc. are different for every country
--
-- but also we ned to only show 'relevant' base projection parameters
-- maybe show on box to the side
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
        cdat <- O.lookup ctry dat.counts
        let tcounts = mkTimeCounts dat.start cdat
        pure
          { name : ctry
          , values : toSeries projections tcounts
          , modelfits : O.fromFoldable $ mspecs <#> \mspec ->
              let ftc = fitTimeCounts mspec tcounts
                  bX = baseProjection projections.x
                  bY = baseProjection projections.y
              in  Tuple (modelFitLabel mspec.fit) $
                    { info: map force <<< O.fromFoldable $ flip A.mapMaybe [bX, bY] $
                        runExists (\bp ->
                          Tuple (baseProjectionLabel bp) <$> case bp of
                            Time      _ -> Nothing
                            Confirmed _ -> Just ftc.modelInfo.confirmed
                            Deaths    _ -> Just ftc.modelInfo.deaths
                            Recovered _ -> Just ftc.modelInfo.recovered
                        )
                    , values: toSeries2 projections.x projections.y ftc.timeCounts
                    }
          }
    }
  where
    projections = hoistPointF (\(PS ps) -> ps.projection) pss

-- toSeries
--     :: forall a b c d.
--        PointF Projection a b c d
--     -> TimeCounts Int
--     -> Array (Point a b c d)
-- toSeries {x, y, z, t} ps = D.datedValues $
--     lift4 (\x y z t -> {x, y, z, t})
--         (applyProjection x ps)
--         (applyProjection y ps)
--         (applyProjection z ps)
--         (applyProjection t ps)


-- fitTimeCounts
--     :: ModelSpec
--     -> TimeCounts Int
--     -> { modelInfo :: Counts ModelRes, timeCounts :: TimeCounts Int }
-- fitTimeCounts info tc =
--     { modelInfo: mapCounts (_.modelInfo) fittedCounts
--     , timeCounts: tc { counts = mapCounts (map round <<< D.datedValues <<< (_.results)) fittedCounts }
--     }
--   where
--     fittedCounts = flip mapCounts tc.counts $ \xs ->
--         modelBase info (Dated { start: tc.start, values: map toNumber xs })


