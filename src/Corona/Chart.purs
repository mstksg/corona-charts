
module Corona.Chart where

import Prelude

import Apexcharts (createChart, render, Apexchart, Apexoptions)
import Apexcharts.Chart as C
import Apexcharts.Chart.Zoom as Z
import Apexcharts.Common as CC
import Apexcharts.Series as SE
import Apexcharts.Xaxis as X
import Apexcharts.Yaxis as Y
import Corona.JHU as JHU
import Data.Array as A
import Data.Int
import Data.JSDate (JSDate)
import Data.JSDate as JSDate
import Data.Map as M
import Data.Maybe
import Data.Options
import Data.Set (Set)
import Data.Set as Set
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class
import Foreign.Object as O
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Query.EventSource as ES

type Country = String

data Aspect = Time
            | Confirmed

-- confirmed, deaths, recovered
-- aspect:
--   * count
--   * moving
--   * change
--   * percent change

data DataView = DV
    { countries :: Set Country
    , spec      :: ViewSpec
    }

type ViewSpec =
    { xAxis :: Aspect
    , yAxis :: Aspect
    }

extractAspect
    :: Array JSDate
    -> JHU.CoronaCounts
    -> Aspect
    -> Array Number
extractAspect dat cc = case _ of
    Time      -> map JSDate.getTime dat
    Confirmed -> map toNumber cc.confirmed

-- aspectType :: Aspect -> AxisType
-- aspectType = case _ of
--     Time      -> DateTime
    

type SeriesData = 
    { series :: Options SE.Series
    , xAxis  :: Options X.Xaxis
    , yAxis  :: Options Y.Yaxis
    }

mkSeries
    :: JHU.CoronaData
    -> Country
    -> ViewSpec
    -> Maybe SeriesData
mkSeries dat country vs = do
    countryCounts <- O.lookup country dat.counts
    let xData    = extractAspect dat.dates countryCounts vs.xAxis
        yData    = extractAspect dat.dates countryCounts vs.yAxis
        fullData = A.zipWith (\x y -> [x, y]) xData yData
        series   = SE.name  := country
                <> SE.data' := fullData
    pure { series, xAxis: mempty, yAxis: mempty }
    
--       egyptData = case O.lookup "Egypt" dat of
    -- SE.name := country
  -- where
    -- xData

--     render $ createChart "#scatteredchart" (
--          SE.series := [
--             (SE.name := "Egypt" <> SE.data' := egyptData)
--          ]
--          <> C.chart := (C.type' := CC.Scatter <> C.height := 350 <> Z.zoom := (Z.enabled := true <> Z.type' := Z.XY))       
--          <> X.xaxis := (X.tickAmount := 10.0 <> X.type' := X.Datetime)
--          <> Y.yaxis := (Y.tickAmount := 10.0)
--       )
