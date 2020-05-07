
"use strict";

// var D3_Scatter = require('D3.Scatter');
// import { fromScale } from "D3/Scatter.purs";

// type SeriesData a b =
//     { name   :: String
//     , points :: Array (Point a b)
//     }

// data ScatterPlot a b = SP
//         { title  :: String
//         , xLabel :: String
//         , yLabel :: String
//         , series :: Array (SeriesData a b)
//         }

const findByX = (ps, x, lo) => d3.bisector(p => p.x).left(ps, x, lo)
    

exports.mkSvg = function(ident) {
    return function () {
        const width = 1000;
        const height = 600;
        const svg = d3.select(ident)
                .append("svg")
                .attr("viewBox", [0,0,width, height])
                .style("overflow","visible");
                // .node();
        return svg;
    }
}

const scaleFuncWith = handlers =>
    scale =>
      handlers.scale(scale)(
        { date: (_u => d3.scaleUtc())
        , count: c => handlers.numberScale(c)(
              { linear: (_u => d3.scaleLinear())
              , log: (_u => d3.scaleLog())
              }
            )
        }
      );

// { xAxis : { scale : Scale, label : String}
// , yAxis : { scale : Scale, label : String}
// , series : [{ name : String, values: [{x, y}]}]
// }
exports._drawData = function(handlers) {
    const scaleFunc = scaleFuncWith(handlers);
    return function(svg) {
        return function (scatter) {
            return function () {
                svg.selectAll("*").remove();
                const width = 1000;
                const height = 600;
                const margin = { top: 20, right: 20, bottom: 20, left: 50 };

                const allx = scatter.series.map(s => s.values.map(p => p.x) ).flat();
                const ally = scatter.series.map(s => s.values.map(p => p.y) ).flat();

                const x = scaleFunc(scatter.xAxis.scale)
                                .domain(d3.extent(allx))
                                .range([margin.left, width - margin.right]);
                const y = scaleFunc(scatter.yAxis.scale)
                                .domain(d3.extent(ally.filter(y => y > 0))).nice()
                                .range([height - margin.bottom, margin.top]);

                const line = d3.line()
                                    .defined(d => d.y > 0)
                                    .x(d => x(d.x))
                                    .y(d => y(d.y));

                const xAxis = function(g) {
                    g.attr("transform", `translate(0,${height - margin.bottom})`)
                        // .call(d3.axisBottom(x).ticks(width/80).tickSizeOuter(0));
                        .call(d3.axisBottom(x))
                        .call(g => g.append("text")
                                    .attr("x", width - margin.right)
                                    .attr("y", -3)
                                    .attr("fill", "currentColor")
                                    .attr("font-weight", "bold")
                                    .text(scatter.xLabel)
                             )
                        .call(g => g.selectAll(".tick line").clone()
                                    .attr("stroke-opacity", 0.1)
                                    .attr("y1", -height + margin.top + margin.bottom)
                             )
                };
                const yAxis = function(g) {
                    g.attr("transform", `translate(${margin.left},0)`)
                        .call(d3.axisLeft(y).ticks(10,",d"))
                        .call(g => g.selectAll(".tick line").clone()
                                        .attr("stroke-opacity", 0.1)
                                        .attr("x2", width - margin.left - margin.right)
                             )
                        .call(g => g.select(".tick:last-of-type text").clone()
                                .attr("x", 3)
                                .attr("text-anchor", "start")
                                .attr("font-weight", "bold")
                                .text(scatter.yLabel)
                            )
                };

                const hover = function(svg, path) {

                  if ("ontouchstart" in document) svg
                      .style("-webkit-tap-highlight-color", "transparent")
                      .on("touchmove", moved)
                      .on("touchstart", entered)
                      .on("touchend", left)
                  else svg
                      .on("mousemove", moved)
                      .on("mouseenter", entered)
                      .on("mouseleave", left);

                  const dot = svg.append("g")
                      .attr("display", "none");

                  dot.append("circle")
                      .attr("r", 2.5);

                  dot.append("text")
                      .attr("font-family", "sans-serif")
                      .attr("font-size", 10)
                      .attr("text-anchor", "middle")
                      .attr("y", -8);

                  function moved() {
                    d3.event.preventDefault();
                    const mouse = d3.mouse(this);
                    const xm = x.invert(mouse[0]);
                    const ym = y.invert(mouse[1]);
                    const i1 = d3.bisectLeft(allx, xm, 1);
                    const i0 = i1 - 1;
                    const i = xm - allx[i0] > allx[i1] - xm ? i1 : i0;
                    const s = d3.least(scatter.series, d => Math.abs(d.values[findByX(d.values,xm,1)].y - ym));
                    const di1 = findByX(s.values,xm,1);
                    const di0 = di1 - 1;
                    const di = xm - s.values[di0].x > s.values[di1].x - xm ? di1 : di0;
                    path.attr("stroke", d => d === s ? null : "#ddd").filter(d => d === s).raise();
                    dot.attr("transform", `translate(${x(allx[i])},${y(s.values[di].y)})`);
                    dot.select("text").text(s.name);
                  }

                  function entered() {
                    path.style("mix-blend-mode", null).attr("stroke", "#ddd");
                    dot.attr("display", null);
                  }

                  function left() {
                    path.style("mix-blend-mode", "multiply").attr("stroke", null);
                    dot.attr("display", "none");
                  }
                }

                svg.append("g")
                    .call(xAxis);
                svg.append("g")
                    .call(yAxis);

                const path = svg.append("g")
                          .attr("fill", "none")
                          .attr("stroke", "steelblue")
                          .attr("stroke-width", 1.5)
                          .attr("stroke-linejoin", "round")
                          .attr("stroke-linecap", "round")
                        .selectAll("path")
                        .data(scatter.series)
                        .join("path")
                          .style("mix-blend-mode", "multiply")
                          .attr("d", d => line(d.values));

                svg.call(hover, path);

            }
        }
    }
}
