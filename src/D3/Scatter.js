
"use strict";

const msPerDay      = 24*60*60*1000;
const mjdShift      = 40586;
const fromMJD = mjd => new Date((mjd - mjdShift) * msPerDay);

const last = xs => xs[xs.length - 1];



const segmentize = function(xs) {
    var out = [];
    for (let i = 1; i < xs.length; i++) {
        out.push( [xs[i-1], xs[i]] );
    }
    return out;
}







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
    const validZ = validVal(scatter.zAxis.scale);
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

    return function () {
        exports.clearSvg(svg)();
        console.log(scatter);
        const margin = { top: 20, right: 20, bottom: 20, left: 50 };
        const series = scatter.series.map(function(s) {
                const vals = s.values.filter(validPair).map( p =>
                            ({x: convertX(p.x), y: convertY(p.y), z: convertZ(p.z)})
                         );
                return { name: s.name
                       , values: vals
                       , segments: segmentize(vals)
                       };
            });
        console.log(series);


        const allx = series.map(s => s.values.map(p => p.x) ).flat();
        const ally = series.map(s => s.values.map(p => p.y) ).flat();
        const allz = series.map(s => s.values.map(p => p.z) ).flat();
        const extentx = d3.extent(allx);
        const extenty = d3.extent(ally);
        const extentz = d3.extent(allz);
        const blues = d3.schemeBlues[9]

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
                        // .range(["lightsteelblue", "steelblue"]);
                        // .range([blues[3], blues[8]]);
                        // .range(d3.range(5).map(t => d3.interpolateWarm(t/4)));
                        // .range(d3.range(5).map(t => d3.interpolateWarm(t/4)));
                        // .interpolate(d3.interpolateWarm);
                        // .range([d3.interpolateSpectral(0), d3.interpolateSpectral(0.5), d3.interpolateSpectral(1)])
                        // .range(d3.range(3).map(t => d3.interpolateSpectral(t/2)))
                        .range([d3.interpolateOranges(0.75),d3.interpolateBlues(0.75)])
                        .interpolate(d3.interpolateCubehelix.gamma(3));
        // window.zzz = z;

        // quadtree to look up nearest point. note z is not encoded
        const quadPoints = series.map(s =>
              s.values.map(p => ({ x: x(p.x), y: y(p.y), z: p.z, name: s.name }))
            ).flat()
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
                            .attr("font-size", 12)
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
                        .attr("font-size", 12)
                        .text(scatter.yAxis.label)
                    )
        };

        const highlight = path => function(name) {
          // console.log(path, name);
          path.attr("stroke-width", d => d.name === name ? 3 : 0.5)
                .filter(d => d.name === name)
                .raise();
        }
        const unhighlight = path => function() {
            path.attr("stroke-width",1.5);
        }

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

          const ch_center = crosshair.append("circle")
                    .attr("stroke","steelblue")
                    .attr("stroke-width",2);

          const toplabel = crosshair.append("text")
              .attr("font-family", "sans-serif")
              .attr("font-size", 12)
              .attr("text-anchor", "middle")
              .attr("y", -12);
          const bottomlabel = crosshair.append("text")
              .attr("font-family", "sans-serif")
              .attr("font-size", 12)
              .attr("text-anchor", "middle")
              .attr("y", 24);

          const vertline = crosshair.append("line")
                .style("stroke-width",0.5)
                .style("stroke-dasharray","4 4")
                .style("stroke","#777")
                .style("fill","none");

          const horizline = crosshair.append("line")
                .style("stroke-width",0.5)
                .style("stroke-dasharray","4 4")
                .style("stroke","#777")
                .style("fill","none");

          function moved() {
            d3.event.preventDefault();
            const mouse0 = d3.mouse(this);
            const mouse =
                [Math.max(Math.min(mouse0[0],width-margin.right),margin.left),
                 Math.max(Math.min(mouse0[1],height-margin.bottom),margin.top)
                ]

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

            toplabel.text(textlab + fmtX(xm) + ", " + fmtY(ym));

            if (foundPoint) {
              ch_center.attr("r",6.0)
                       .attr("fill",z(closest.z));
              // path.attr("stroke", "#ddd").attr("stroke-width",1);
              // marks.style("display","none");
              highlight(path)(closest.name);
              // path.attr("stroke-width", d => d.name === closest.name ? 3 : 0.5)
              //       .filter(d => d.name === closest.name)
              //       .raise();
              bottomlabel.text(fmtZ(closest.z));
            } else {
              ch_center.attr("r",4.0)
                       .attr("fill","white");
              unhighlight(path)();
              path.attr("stroke-width",1.5);
              bottomlabel.text("");
              // marks.style("display",null);
            }

          }

          function entered() {
            crosshair.attr("display", null);
          }

          function left() {
            unhighlight(path)();
            // path.attr("stroke-width",1.5);
            // path.attr("stroke", null).attr("stroke-width",1);
            // marks.style("display",null);
            crosshair.attr("display", "none");
          }

        }

        svg.append("g")
            .call(xAxis);
        svg.append("g")
            .call(yAxis);

        const segments = series.map(s => s.segments.map (p => ({ name: s.name, pair: p }))).flat()
        console.log(segments);
        const path = svg.append("g")
                  // .attr("stroke", "#777")
                  // .attr("stroke", "steelblue")
                  .selectAll("g")
                  .data(segments)
                  .join("g");

        const pathsegments = path.append("path")
                  .attr("fill", "none")
                  .attr("stroke-width", 1.5)
                  .attr("stroke-linejoin", "round")
                  .attr("stroke-linecap", "round")
                  .attr("stroke", d => z(d.pair[1].z))
                  // .style("mix-blend-mode", "multiply")
                  // .attr("d", d => line(d.values));
                  .attr("d", d => line(d.pair));

        // const marks = path.append("g")
        //     .attr("stroke","white")
        //     .attr("stroke-width","0")
        //     .selectAll("circle")
        //     .data(d => d.values)
        //     .join("circle")
        //     .attr("transform", d => `translate(${x(d.x)},${y(d.y)})`)
        //     .attr("fill",d => z(d.z))
        //     .attr("r",2.5);

        svg.append("g")
            .attr("text-anchor","start")
            .selectAll("text")
            .data(series)
            .join("text")
            .attr("x",d => x(last(d.values).x)+8)
            .attr("y",d => y(last(d.values).y))
            .attr("font-family", "sans-serif")
            .attr("font-size", 12)
            .text(d => d.name);

        svg.call(hover, pathsegments);

        return {
            highlight: highlight(pathsegments),
            unhighlight: unhighlight(pathsegments)
        };
    }
}

// takes a (maybe string)
exports._highlight = function(handleMaybe,interactors,name) {
    return function() {
        // console.log(interactors, name);
        handleMaybe(name)(
            { nothing: (() => interactors.unhighlight())
            , just: (n => interactors.highlight(n))
            }
        );
    }
}
