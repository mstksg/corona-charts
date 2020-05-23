
module Corona.Chart.Assemble where

import Prelude

import Corona.Chart
import Corona.Chart.Model
import Data.Newtype
import Corona.Data.Type
import D3.Scatter.Type
import Data.Array as A
import Data.Set (Set)
import Foreign.Object as O
import Prelude

-- type ProjScales a b c d =
--     { projX
--     , scaleX
--     , projY
--     , scaleY
--     , projZ
--     , scaleZ
--     }

toScatterPlot
    :: forall a b c d.
       CoronaData
    -> PointF ProjScale a b c d
    -> Set Region
    -> ScatterPlot a b c d
toScatterPlot dat pss ctrys =
  { axis   : hoistPointF toAxisConf pss
  , series : flip A.mapMaybe (A.fromFoldable ctrys) $ \ctry -> do
      cdat <- O.lookup ctry dat.counts
      pure
        { name : ctry
        , values : toSeries (hoistPointF (\(PS ps) -> ps.projection) pss)
                  (mkTimeCounts dat.start cdat)
        , modelfits : O.empty
        }
  }

