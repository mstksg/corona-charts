
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

const timedSeries = series => function(t) {
    return series.map(function (s) {
        const cutoff = d3.bisector(p => p.t).right(s.values,t);
        return {
            name: s.name
          , values: s.values.slice(0,cutoff+1)
        };
    }).filter(s => s.values.length > 0);
}

// { xAxis : { scale : Scale, label : String}
// , yAxis : { scale : Scale, label : String}
// , zAxis : { scale : Scale, label : String}
// , tAxis : { scale : Scale, label : String}
// , series : [{ name : String, values: [{x, y, z, t}]}]
// }
exports._drawData = function(handleType, handleScale, typeX, typeY, typeZ, typeT, svgdat, scatter) {
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
            // { day:     (() => val => val.toLocaleDateString(undefined, {month:"numeric",day:"numeric"}))
            { day:     (() => d3.timeFormat("%b %d"))
            , days:    (() => d3.format(".3~s"))
            , "int":   (() => d3.format(".3~s"))
            , number:  (() => d3.format(".3~s"))
            , percent: (() => d3.format("+.3~p"))
            }
        )
    const fmtX = fmt(typeX);
    const fmtY = fmt(typeY);
    const fmtZ = fmt(typeZ);
    const fmtT = fmt(typeT);
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
    const axisTickerT = axisTicker(typeT);

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
    const validT = validVal(scatter.tAxis.scale);
    const validPair = function(p) {
        return validX(p.x) && validY(p.y) && validZ(p.z) && validT(p.t);
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
    const convertT = convert(typeT);

    const validTimeScale = handleType(typeT)(
            { day: (() => true)
            , days: (() => true)
            , "int": (() => false)
            , number: (() => false)
            , percent: (() => false)
            }
        );

    return function () {
        exports.clearSvg(svg)();
        // console.log(scatter);
        const margin = { top: 20, right: 20, bottom: 70, left: 50, slider: 20 };
        const series = scatter.series.map(function(s) {
                const vals = s.values.filter(validPair).map( p =>
                            ({ x: convertX(p.x)
                             , y: convertY(p.y)
                             , z: convertZ(p.z)
                             , t: convertT(p.t)
                            })
                         );
                return { name: s.name
                       , values: vals
                       , segments: segmentize(vals)
                       };
            });
        const sliceSeries = timedSeries(series);
        console.log(series);

        // var filteredSeries;



        const allx = series.map(s => s.values.map(p => p.x) ).flat();
        const ally = series.map(s => s.values.map(p => p.y) ).flat();
        const allz = series.map(s => s.values.map(p => p.z) ).flat();
        const allt = series.map(s => s.values.map(p => p.t) ).flat();
        const extentx = d3.extent(allx);
        const extenty = d3.extent(ally);
        const extentz = d3.extent(allz);
        const extentt = d3.extent(allt);
        const blues = d3.schemeBlues[9]

        // this converts to and form the different coordinate systems
        const x = scaleFunc(scatter.xAxis.scale)
                        .domain(extentx).nice()
                        .range([margin.left, width - margin.right]);
        const y = scaleFunc(scatter.yAxis.scale)
                        .domain(extenty).nice()
                        .range([height - margin.bottom, margin.top]);
        // color
        const z = scaleFunc(scatter.zAxis.scale)
                        .domain(extentz)        // not nice
                        .range([d3.interpolateOranges(0.75),d3.interpolateBlues(0.75)])
                        .interpolate(d3.interpolateCubehelix.gamma(3));

                        // .range([d3.schemeDark2[1],d3.schemeAccent[1]]);
        const t = scaleFunc(scatter.tAxis.scale)
                        .domain(extentt)        // not nice
                        .range([margin.left, width - margin.right]);

        // makes axes lines. i think this converts coordinates into svg points
        // or something???
        const line = d3.line()
                            // .defined(validPair)
                            .x(d => x(d.x))
                            .y(d => y(d.y));

        // makes axes lines
        const xAxis = function(g) {
            g.attr("transform", `translate(0,${height - margin.bottom})`)
                .call(axisTickerX(d3.axisBottom(x)))
                .call(g => g.append("text")
                            .attr("x", width - margin.right)
                            .attr("y", -5)
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
                        .attr("y", -5)
                        .attr("text-anchor", "start")
                        .attr("font-weight", "bold")
                        .attr("font-size", 12)
                        .text(scatter.yAxis.label)
                    );
        };
        // const zLegend = function(g) {
        //     const leg = d3.legendColor()
        //                     .labelFormat(fmtZ)
        //                     .scale(z);

        //     g.call(leg);
        // }
        const tAxis = function(g) {
            const sliderino = d3.sliderBottom(t)
                        .tickFormat(fmtT)
                        .displayFormat(fmtT)
            g.attr("transform", `translate(0,${height - margin.slider})`)
                .call(sliderino)
                .append("text")
                .attr("x", margin.left-5)
                .attr("y", -10)
                .attr("fill", "currentColor")
                .attr("font-weight", "bold")
                .attr("font-size", 12)
                .text(scatter.tAxis.label);
            return sliderino;
        }


        const highlight = subplot => function(name) {
          // console.log(path, name);
          subplot.selectAll("path").attr("stroke-width", d => d.name === name ? 3 : 0.5)
                .filter(d => d.name === name)
                .raise();
        }
        const unhighlight = subplot => function() {
            subplot.selectAll("path").attr("stroke-width",1.5);
        }

        const mkQuadPoints = ss => ss.map(s =>
              s.values.map(p => ({ x: x(p.x), y: y(p.y), z: p.z, t: p.t, name: s.name }))
            ).flat();

        var currQuadPoints = mkQuadPoints(series);
        // quadtree to look up nearest point. note z is not encoded
        const quadtree = d3.quadtree()
                .x(d => d.x)
                .y(d => d.y)
                .extent([x(extentx[0]),y(extenty[0])], [x(extentx[1]),y(extenty[1])])
                .addAll(currQuadPoints);

        const requad = function(t) {
            quadtree.removeAll(currQuadPoints);
            currQuadPoints = mkQuadPoints(sliceSeries(t));
            quadtree.addAll(currQuadPoints);
        }

        const hover = function(svg, highlight, unhighlight) {

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

          const mkLabel = (g,anchor,x,y) => g.append("text")
              .attr("font-family", "sans-serif")
              .attr("font-size", 12)
              .attr("text-anchor",anchor)
              .attr("x",x)
              .attr("y",y);

          const toplabel    = mkLabel(crosshair,"middle",0,-12);
          const bottomlabel = mkLabel(crosshair,"middle",0, 24)
                                    .attr("font-weight","bold");
          const footlabel   = mkLabel(crosshair,"middle",0, 40);

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
            const mouse = d3.mouse(this);
            const closest = quadtree.find(mouse[0], mouse[1], 50);
            const foundPoint = !(closest === undefined);
            const xc = foundPoint ? closest.x : mouse[0];
            const yc = foundPoint ? closest.y : mouse[1];
            const xm = x.invert(xc);
            const ym = y.invert(yc);

            crosshair.attr("transform", `translate(${xc},${yc})`);
            vertline.attr("y1", -yc+margin.top).attr("y2", -yc+height-margin.bottom);
            horizline.attr("x1", -xc+margin.left).attr("x2", -xc+width-margin.right);

            const showx = fmtX(xm);
            const showy = fmtY(ym);
            toplabel.text(showx + ", " + showy);

            if (foundPoint) {
              ch_center.attr("r",6.0)
                       .attr("fill",z(closest.z));
              highlight(closest.name);
              bottomlabel.text(closest.name);
              footlabel.text(
                    [fmtZ(closest.z),fmtT(closest.t)]
                                    .filter(st => st !== showx && st !== showy)
                                    .join(", ")
                );
            } else {
              ch_center.attr("r",4.0)
                       .attr("fill","white");
              unhighlight();
              bottomlabel.text("");
              footlabel.text("");
            }

          }

          function entered() {
            crosshair.attr("display", null);
          }

          function left() {
            unhighlight();
            crosshair.attr("display", "none");
          }

        }

        svg.append("g")
            .call(xAxis);
        svg.append("g")
            .call(yAxis);
        // svg.append("g")
        //     .call(zLegend);

        const flatSegments = ss => ss.map(s => s.segments.map (p => ({ name: s.name, pair: p }))).flat()
        // const mkTimed = timedSeries(series);

        const subplot = svg.append("g")

        // click capture
        subplot.append("g")
          .append('rect')
          .style('visibility','hidden')
          .style('pointer-events','all')
          .attr('x',margin.left)
          .attr('y',margin.top)
          .attr('width',width - margin.right - margin.left)
          .attr('height',height - margin.top - margin.bottom);

        subplot.append("g")
             .selectAll("g")
             .data(flatSegments(series))
             .join("g")
             .append("path")
             .attr("fill", "none")
             .attr("stroke-width", 1.5)
             .attr("stroke-linejoin", "round")
             .attr("stroke-linecap", "round")
             .attr("stroke", d => z(d.pair[1].z))
             .attr("d", d => line(d.pair));

        const endDots = svg.append("g");

        subplot.call(hover, highlight(subplot),unhighlight(subplot));


        const setTime = function(subplot, t) {
            subplot.selectAll("path")
                .attr("display", function(d) {
                    // console.log(d.pair[1].t, t);
                    return (d.pair[0].t <= t) ? null : "none";
                });

            endDots.selectAll("*").remove();


            const dotData = series.map(function(s) {
              const cutoff = d3.bisector(p => p.t).right(s.values,t);
              if (cutoff == 0) {
                  return [];
              } else {
                  return [{
                      name: s.name
                    , last: s.values[Math.min(cutoff,s.values.length-1)]
                  }];
              }
            }).flat();
            // console.log(dotData);

            endDots.append("g")
                .attr("text-anchor","start")
                .selectAll("text")
                .data(dotData)
                .join("text")
                .attr("x",d => x(d.last.x)+10)
                .attr("y",d => y(d.last.y)+3)
                .attr("font-family", "sans-serif")
                .attr("font-size", 12)
                .text(d => d.name)
                .style('pointer-events','none')

            endDots.append("g")
                .selectAll("circle")
                .data(dotData)
                .join("circle")
                .attr("r", 3)
                .attr("fill","red")
                .attr("transform", d => `translate(${x(d.last.x)},${y(d.last.y)})`)
                .style('pointer-events','none')

            requad(t);
        }

        const sliderino = tAxis(svg.append("g"))
                // .on('onchange', v => setTime(subplot,v))
                .on('onchange', _.throttle(v => setTime(subplot,v), 50))
                // .on('end',v => requad(v));

        const button = svg.append("g")
                    .attr("transform",`translate(-5,${height-margin.slider-15})`)
                    .style("cursor","pointer")
                    .style('pointer-events','all');

        const drawButton = function(g,isPlaying,callback) {
            g.selectAll("*").remove();
            g.on("click",null)
                .on("click",callback);
            g.append("rect")
                .attr("width",30)
                .attr("height",30)
                .attr("rx",3)
                .style("fill",d3.schemeDark2[1]);
            if (isPlaying) {
                g.append("rect")
                    .attr("width",6)
                    .attr("height",18)
                    .attr("x",6)
                    .attr("y",6)
                    .attr("rx",1)
                    .style("fill","white");
                g.append("rect")
                    .attr("width",6)
                    .attr("height",18)
                    .attr("x",18)
                    .attr("y",6)
                    .attr("rx",1)
                    .style("fill","white");
            } else {
                g.append("path")
                    .attr("d","M9 6 L9 24 L21 15 Z")
                    .style("fill","white");
            }
        }

        var timer = null;
        var is_playing = false;

        const moveSetSlider = function(t) {
            sliderino.value(t);
            setTime(subplot,t);
        }

        const playdata = (function () {
            const playtime = 15000;
            const delay = 50;
            const numSteps = playtime / delay;
            const minT = t(extentt[0])
            const maxT = t(extentt[1])
            const domainwidth = maxT - minT;
            const stepAmt = domainwidth / numSteps;
            return {
                minT: minT,
                maxT: maxT,
                delay: delay,
                stepAmt: stepAmt
            };
        })();

        const play_start = function () {
            button.call(drawButton,true,play_stop);
            timer = setInterval(play_tick,playdata.delay);
        }
        const play_stop = function () {
            button.call(drawButton,false,play_start);
            clearInterval(timer);
            timer = null;
            is_playing = false;
        }
        const play_tick = function () {
            const currval = t(sliderino.value());
            const nextval = currval + playdata.stepAmt;
            const hitTheEnd = nextval > playdata.maxT;
            // console.log(currval,nextval,hitTheEnd);
            if (hitTheEnd) {
                if (is_playing) {
                    return play_stop();
                } else {
                    moveSetSlider(extentt[0]);
                    is_playing = true;
                }
            } else {
                moveSetSlider(t.invert(nextval));
            }
        }

        window.sliderino = sliderino;
        window.tt = t;

        button.call(drawButton,false,play_start);

        moveSetSlider(extentt[1]);


        return {
            highlight: highlight(subplot),
            unhighlight: unhighlight(subplot),
            settime: (v => setTime(subPlot, v))
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
