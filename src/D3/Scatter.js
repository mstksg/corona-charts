
"use strict";

const msPerDay      = 24*60*60*1000;
const mjdShift      = 40587;
const fromMJD = mjd => new Date((mjd - mjdShift) * msPerDay);

const findByX = (ps, x, lo) => d3.bisector(p => p.x).left(ps, x, lo);
const last = xs => xs[xs.length - 1];

const mantissa = (x, n) => Math.round((x / Math.pow(10, exponent(x)-n+1)));
const exponent = x => Math.floor(Math.log10(x));
const suffices = ["","k","M","B","T","q","Q","s","S","o","N","d"];


exports._mkSvg = function(elem, dim) {
    return function () {
        const svg = d3.select(elem)
                .append("svg")
                // .classed("svg-content-responsive",true)
                // .attr("viewBox", [0,0,1000, 600])
                .attr("viewBox", [0,0,dim.width, dim.height])
                .style("overflow","visible");
        return { svg: svg, dimensions: dim } ;
    }
}

exports.clearSvg = function(svg) {
    return function () {
        svg.selectAll("*").remove();
    }
}

// { xAxis : { scale : Scale, label : String}
// , yAxis : { scale : Scale, label : String}
// , zAxis : { scale : Scale, label : String}
// , series : [{ name : String, values: [{x, y, z}]}]
// }
exports._drawData = function(handleType, handleScale, typeX, typeY, typeZ, svgdat, scatter) {
    const svg = svgdat.svg;
    const width = svgdat.dimensions.width;
    const height = svgdat.dimensions.height;
    // const width = 1000;
    // const height = 600;

    const scaleFunc = scale =>
        handleScale(scale)(
            { date:   (() => d3.scaleUtc())
            , linear: (() => d3.scaleLinear())
            , log:    (() => d3.scaleLog())
            }
        );

    const fmt = tp =>
        handleType(tp)(
            { day:     (() => val => val.toLocaleDateString(undefined, {month:"numeric",day:"numeric"}))
            , days:    (() => d3.format(".3~s"))
            , "int":   (() => d3.format(".3~s"))
            , number:  (() => d3.format(".3~s"))
            , percent: (() => d3.format("+.3~p"))
            }
        )
    const fmtX = fmt(typeX);
    const fmtY = fmt(typeY);
    const fmtZ = fmt(typeZ);
    const axisTicker = tp =>
        handleType(tp)(
            { day:     (() => a => a.ticks(10))
            , days:    (() => a => a.ticks(10, ".3~s"))
            , "int":   (() => a => a.ticks(10, ".3~s"))
            , number:  (() => a => a.ticks(10, ".3~s"))
            , percent: (() => a => a.ticks(10, "+.3~p"))
            }
        );
    const axisTickerX = axisTicker(typeX);
    const axisTickerY = axisTicker(typeY);

    const validVal = scale => val =>
        !isNaN(val) && handleScale(scale)(
          { date: (() => true)
          , linear: (() => true)
          , log: (() => val > 0)
          }
        );
    const validX = validVal(scatter.xAxis.scale);
    const validY = validVal(scatter.yAxis.scale);
    const validZ = validVal(scatter.yAxis.scale);
    const validPair = function(p) {
        return validX(p.x) && validY(p.y) && validZ(p.z);
    }
    const convert = tp => val =>
        handleType(tp)(
            { day:     (() => fromMJD(val))
            , days:    (() => val)
            , "int":   (() => val)
            , number:  (() => val)
            , percent: (() => val)
            }
        );
    const convertX = convert(typeX);
    const convertY = convert(typeY);
    const convertZ = convert(typeZ);
    const convertPoint = function(p) {
            return {x: convertX(p.x), y: convertY(p.y), z: convertZ(p.z)};
        }

    return function () {
        exports.clearSvg(svg)();
        console.log(scatter);
        const margin = { top: 20, right: 20, bottom: 20, left: 50 };
        const series = scatter.series.map(function(s) {
                return { name: s.name
                       , values: s.values.filter(validPair).map(convertPoint)
                       };
            });
        console.log(series);


        const allx = series.map(s => s.values.map(p => p.x) ).flat();
        const ally = series.map(s => s.values.map(p => p.y) ).flat();
        const allz = series.map(s => s.values.map(p => p.z) ).flat();
        const extentx = d3.extent(allx);
        const extenty = d3.extent(ally);
        const extentz = d3.extent(allz);

        // this converts to and form the different coordinate systems
        const x = scaleFunc(scatter.xAxis.scale)
                        .domain(d3.extent(allx)).nice()
                        .range([margin.left, width - margin.right]);
        const y = scaleFunc(scatter.yAxis.scale)
                        .domain(d3.extent(ally)).nice()
                        .range([height - margin.bottom, margin.top]);
        // color
        const z = scaleFunc(scatter.zAxis.scale)
                        .domain(d3.extent(allz))        // not nice
                        // .range(["brown", "steelblue"]);
                        // .range(["#f7fbff", "#08306b"]);
                        .range(["lightsteelblue", "steelblue"]);
        // window.zzz = z;

        // quadtree to look up nearest point
        const quadPoints = series.map(s => s.values.map(function(p) { return { x: x(p.x), y: y(p.y), name: s.name };})).flat()
        // console.log(flatPoints);
        const quadtree = d3.quadtree()
                .x(d => d.x)
                .y(d => d.y)
                .extent([x(extentx[0]),y(extenty[0])], [x(extentx[1]),y(extenty[1])])
                .addAll(quadPoints);
        // console.log(quadtree);

        // makes axes lines. i think this converts coordinates into svg points
        // or something???
        const line = d3.line()
                            // .defined(validPair)
                            .x(d => x(d.x))
                            .y(d => y(d.y));

        // makes axes lines
        const xAxis = function(g) {
            g.attr("transform", `translate(0,${height - margin.bottom})`)
                // .call(d3.axisBottom(x).ticks(width/80).tickSizeOuter(0));
                .call(axisTickerX(d3.axisBottom(x)))
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
                .call(axisTickerY(d3.axisLeft(y)))
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

          const crosshair = svg.append("g").attr("display", "none");

          const ch_center = crosshair.append("circle").attr("r",2.5);

          crosshair.append("text")
              .attr("font-family", "sans-serif")
              .attr("font-size", 12)
              .attr("text-anchor", "middle")
              .attr("y", -8);

          const vertline = crosshair.append("line")
                // .attr("y1", margin.top)
                // .attr("y2", height - margin.bottom)
                .style("stroke-width",0.5)
                .style("stroke-dasharray","5 5")
                .style("stroke","#444")
                .style("fill","none");

          const horizline = crosshair.append("line")
                // .attr("x1", margin.left)
                // .attr("x2", width - margin.right)
                .style("stroke-width",0.5)
                .style("stroke-dasharray","5 5")
                .style("stroke","#444")
                .style("fill","none");

          function moved() {
            d3.event.preventDefault();
            const mouse = d3.mouse(this);

            const closest = quadtree.find(mouse[0], mouse[1], 50);
            // console.log(closest);
            const foundPoint = !(closest === undefined);

            const xc = foundPoint ? closest.x : mouse[0];
            const yc = foundPoint ? closest.y : mouse[1];
            const textlab = foundPoint ? (closest.name + ": ") : "";
            const xm = x.invert(xc);
            const ym = y.invert(yc);

            crosshair.attr("transform", `translate(${xc},${yc})`);
            vertline.attr("y1", -yc+margin.top).attr("y2", -yc+height-margin.bottom);
            horizline.attr("x1", -xc+margin.left).attr("x2", -xc+width-margin.right);

            crosshair.select("text").text(textlab + fmtX(xm) + ", " + fmtY(ym));

            if (foundPoint) {
              path.attr("stroke", "#ddd").attr("stroke-width",1);
              marks.style("display","none");
              path.attr("stroke", d => d.name === closest.name ? "steelblue" : "#ddd")
                  .attr("stroke-width", d => d.name === closest.name ? 2.5 : 1)
                    .filter(d => d.name === closest.name)
                    .raise();
            } else {
              path.attr("stroke", null).attr("stroke-width",1);
              marks.style("display",null);
            }

          }

          function entered() {
            crosshair.attr("display", null);
          }

          function left() {
            path.attr("stroke", null).attr("stroke-width",1);
            marks.style("display",null);
            crosshair.attr("display", "none");
          }

        }

        svg.append("g")
            .call(xAxis);
        svg.append("g")
            .call(yAxis);

        // const path = svg.append("g")
        //           .attr("fill", "none")
        //           .attr("stroke", "steelblue")
        //           .attr("stroke-width", 1.5)
        //           .attr("stroke-linejoin", "round")
        //           .attr("stroke-linecap", "round")
        //         .selectAll("path")
        //         .data(series)
        //         .join("path")
        //           .style("mix-blend-mode", "multiply")
        //           // .attr("stroke", d => z(d.z))
        //           .attr("d", d => line(d.values));

        // line.append("g")
        //     .attr("stroke","white")
        //     .attr("fill","brown")
        //     .selectAll("circle")
        //     .data(d => d.values)
        //     .join("circle")
        //     .attr("transform", d => `translate(${x(d.x)},${y(d.y)})`)
        //     .attr("r",5);

        const path = svg.append("g")
                  .attr("stroke", "#777")
                  // .attr("stroke", "steelblue")
                  .attr("stroke-width", 1)
                  .attr("stroke-linejoin", "round")
                  .attr("stroke-linecap", "round")
                  .selectAll("g")
                  // .style("mix-blend-mode", "multiply")
                  .data(series)
                  .join("g");

        path.append("path")
                  .attr("fill", "none")
                  .attr("d", d => line(d.values));

        const marks = path.append("g")
            .attr("stroke","white")
            .attr("stroke-width","0")
            .selectAll("circle")
            .data(d => d.values)
            .join("circle")
            .attr("transform", d => `translate(${x(d.x)},${y(d.y)})`)
            .attr("fill",d => z(d.z))
            .attr("r",2.5);

        svg.append("g")
            .attr("text-anchor","middle")
            .selectAll("text")
            .data(series)
            .join("text")
            .attr("x",d => x(last(d.values).x))
            .attr("y",d => y(last(d.values).y)-8)
            .attr("font-family", "sans-serif")
            .attr("font-size", 12)
            .text(d => d.name);

        svg.call(hover, path);
    }
}


        // const hover = function(svg, path) {

        //   if ("ontouchstart" in document) svg
        //       .style("-webkit-tap-highlight-color", "transparent")
        //       .on("touchmove", moved)
        //       .on("touchstart", entered)
        //       .on("touchend", left)
        //   else svg
        //       .on("mousemove", moved)
        //       .on("mouseenter", entered)
        //       .on("mouseleave", left);

        //   const dot = svg.append("g")
        //       .attr("display", "none");

        //   dot.append("circle")
        //       .attr("r", 2.5);

        //   dot.append("text")
        //       .attr("font-family", "sans-serif")
        //       .attr("font-size", 10)
        //       .attr("text-anchor", "middle")
        //       .attr("y", -8);

        //   function moved() {
        //     d3.event.preventDefault();
        //     const mouse = d3.mouse(this);
        //     const xm = x.invert(mouse[0]);
        //     const ym = y.invert(mouse[1]);
        //     const i1 = d3.bisectLeft(allx, xm, 1);
        //     const i0 = i1 - 1;
        //     const i = xm - allx[i0] > allx[i1] - xm ? i1 : i0;
        //     const s = d3.least(series, d => Math.abs(d.values[findByX(d.values,xm,1)].y - ym));
        //     const di1 = findByX(s.values,xm,1);
        //     const di0 = di1 - 1;
        //     const di = xm - s.values[di0].x > s.values[di1].x - xm ? di1 : di0;
        //     path.attr("stroke", d => d === s ? null : "#ddd").filter(d => d === s).raise();
        //     dot.attr("transform", `translate(${x(allx[i])},${y(s.values[di].y)})`);
        //     dot.select("text").text(s.name + ": " + fmtX(s.values[di].x) + ", " + fmtY(s.values[di].y));
        //   }

        //   function entered() {
        //     path.style("mix-blend-mode", null).attr("stroke", "#ddd");
        //     dot.attr("display", null);
        //   }

        //   function left() {
        //     path.style("mix-blend-mode", "multiply").attr("stroke", null);
        //     dot.attr("display", "none");
        //   }
        // }

