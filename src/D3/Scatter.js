
"use strict";

const msPerDay      = 24*60*60*1000;
const mjdShift      = 40587;
const fromMJD = mjd => new Date((mjd - mjdShift) * msPerDay);

const findByX = (ps, x, lo) => d3.bisector(p => p.x).left(ps, x, lo)

const mantissa = (x, n) => Math.round((x / Math.pow(10, exponent(x)-n+1)));
const exponent = x => Math.floor(Math.log10(x));
const suffices = ["","k","M","B","T","q","Q","s","S","o","N","d"];

const fmtPrefix = function(i, n) {

    const ipos = Math.abs(i);
    const mant = mantissa(ipos,n);
    const expo = exponent(ipos);

    const extraDigits = expo % 3;
    const sections    = Math.floor(expo / 3);
    const numberPart  = (mant * Math.pow(10,extraDigits-n+1)).toFixed(n-extraDigits-1);
    const pref = (i < 0) ? "-" : "";

    return pref + numberPart + suffices[sections];
}

exports.mkSvg = function(elem) {
    return function () {
        const width = 1000;
        const height = 600;
        const svg = d3.select(elem)
                .append("svg")
                .attr("viewBox", [0,0,width, height])
                .style("overflow","visible");
        return svg;
    }
}

exports.clearSvg = function(svg) {
    return function () {
        svg.selectAll("*").remove();
    }
}

// { xAxis : { scale : Scale, label : String}
// , yAxis : { scale : Scale, label : String}
// , series : [{ name : String, values: [{x, y}]}]
// }
exports._drawData = function(handleType, handleScale, typeX, typeY, svg, scatter) {
    const scaleFunc = scale =>
        handleScale(scale)(
            { date:   (() => d3.scaleUtc())
            , linear: (() => d3.scaleLinear())
            , log:    (() => d3.scaleLog())
            }
        );

    const fmt = tp => val =>
        handleType(tp)(
            { day:     (() => val.toLocaleDateString("en-US", {month:"numeric",day:"numeric"}))
            , days:    (() => val)
            , "int":   (() => (Math.abs(val) < 1000) ? val : fmtPrefix(val,4))
            , number:  (() => (Math.abs(val) < 1) ? val
                                    : (Math.abs(val) < 1000) ? val.toFixed(2) : fmtPrefix(val,4)
                       )
            , percent: (() => ((Math.abs(val) < 1) ? val
                                    : (Math.abs(val) < 1000) ? val.toFixed(1) : fmtPrefix(val,3)) + "%"
                       )
            }
        );
    const fmtX = fmt(typeX);
    const fmtY = fmt(typeY);

    const validVal = scale => val =>
        !isNaN(val) && handleScale(scale)(
          { date: (() => true)
          , linear: (() => true)
          , log: (() => val > 0)
          }
        );
    const validX = validVal(scatter.xAxis.scale);
    const validY = validVal(scatter.yAxis.scale);
    const validPair = function(p) {
        return validX(p.x) && validY(p.y);
    }
    const convert = tp => val =>
        handleType(tp)(
            { day:     (() => fromMJD(val))
            , days:    (() => val)
            , "int":   (() => val)
            , number:  (() => val)
            , percent: (() => val * 100)
            }
        );
    const convertX = convert(typeX);
    const convertY = convert(typeY);
    const convertPair = function(p) {
            return {x: convertX(p.x), y: convertY(p.y)};
        }

    return function () {
        exports.clearSvg(svg)();
        console.log(scatter);
        const width = 1000;
        const height = 600;
        const margin = { top: 20, right: 20, bottom: 20, left: 50 };
        const series = scatter.series.map(function(s) {
                return { name: s.name
                       , values: s.values.filter(validPair).map(convertPair)
                       };
            });
        console.log(series);

        const allx = series.map(s => s.values.map(p => p.x) ).flat();
        const ally = series.map(s => s.values.map(p => p.y) ).flat();

        const x = scaleFunc(scatter.xAxis.scale)
                        .domain(d3.extent(allx)).nice()
                        .range([margin.left, width - margin.right]);
        const y = scaleFunc(scatter.yAxis.scale)
                        .domain(d3.extent(ally)).nice()
                        .range([height - margin.bottom, margin.top]);

        const line = d3.line()
                            // .defined(validPair)
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
                            .text(scatter.xAxis.label)
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
                        .text(scatter.yAxis.label)
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
            const s = d3.least(series, d => Math.abs(d.values[findByX(d.values,xm,1)].y - ym));
            const di1 = findByX(s.values,xm,1);
            const di0 = di1 - 1;
            const di = xm - s.values[di0].x > s.values[di1].x - xm ? di1 : di0;
            path.attr("stroke", d => d === s ? null : "#ddd").filter(d => d === s).raise();
            dot.attr("transform", `translate(${x(allx[i])},${y(s.values[di].y)})`);
            dot.select("text").text(s.name + ": " + fmtX(s.values[di].x) + ", " + fmtY(s.values[di].y));
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
                .data(series)
                .join("path")
                  .style("mix-blend-mode", "multiply")
                  .attr("d", d => line(d.values));

        svg.call(hover, path);
    }
}
