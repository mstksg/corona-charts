
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
          , modelfits : mspecs <#> \mspec ->
              let ftc = fitTimeCounts mspec tcounts
                  bX  = baseProjection projections.x
                  bY  = baseProjection projections.y
              in  { fit: mspec.fit
                  , info: O.toArrayWithKey assemble <<< map force <<< O.fromFoldable $ flip A.mapMaybe [bX, bY] $
                        runExists (\bp ->
                          Tuple (baseProjectionLabel bp) <$> case bp of
                            Time      _ -> Nothing
                            Confirmed _ -> Just ftc.modelInfo.confirmed
                            Deaths    _ -> Just ftc.modelInfo.deaths
                            Recovered _ -> Just ftc.modelInfo.recovered
                        )
                  , values: toSeries2 projections.x projections.y ftc.timeCounts
                  -- , values: toSeries2 projections ftc.timeCounts
                  }
          }
    }
  where
    projections = hoistPointF (\(PS ps) -> ps.projection) pss
    assemble name result = { name, result }

