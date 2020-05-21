
"use strict";

const msPerDay      = 24*60*60*1000;
const mjdShift      = 40586;
const fromMJD = mjd => new Date((mjd - mjdShift) * msPerDay);

const last = xs => xs[xs.length - 1];



// const segmentize = function(xs) {
//     var out = [];
//     for (let i = 1; i < xs.length; i++) {
//         out.push( [xs[i-1], xs[i]] );
//     }
//     return out;
// }







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

const timedSeries = series => function(t,keeplast) {
    return series.map(function (s) {
        const cutoff = d3.bisector(p => p.t).right(s.values,t);
        return {
            name: s.name
          , values: s.values.slice(0,cutoff+(keeplast ? 1 : 0))
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
            , linear: (() => () => d3.scaleLinear())
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
            { day:     (() => (a,n) => a.ticks(n).tickFormat(d3.timeFormat("%b %d")))
            , days:    (() => (a,n) => a.ticks(n ? n : 10, ".3~s"))
            , "int":   (() => (a,n) => a.ticks(n ? n : 10, ".3~s"))
            , number:  (() => (a,n) => a.ticks(n ? n : 10, ".3~s"))
            , percent: (() => (a,n) => a.ticks(n ? n : 10, "+.3~p"))
            }
        );
    const axisTickerX = axisTicker(typeX);
    const axisTickerY = axisTicker(typeY);
    const axisTickerZ = axisTicker(typeZ);
    const axisTickerT = axisTicker(typeT);

    const validVal = scale => val =>
        !isNaN(val) && handleScale(scale)(
          { date: (() => true)
          , linear: (() => () => true)
          , log: (() => val > 0)
          }
        );
    const validX = validVal(scatter.xAxis.scale);
    const validY = validVal(scatter.yAxis.scale);
    const validZ = validVal(scatter.zAxis.scale);
    const validT = validVal(scatter.tAxis.scale);
    const zeroScale = scale =>
        handleScale(scale)(
          { date: (() => [])
          , linear: (() => zer => zer ? [0] : [])
          , log: (() => [])
          }
        )
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
        console.log(scatter);
        const margin = { top: 10, right: 40, bottom: 80, left: 50, slider: 35 };
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
                       , segments: d3.pairs(vals)
                       };
            });
        const sliceSeries = timedSeries(series);
        // console.log(series);

        const allx = series.flatMap(s => s.values.map(p => p.x) );
        const ally = series.flatMap(s => s.values.map(p => p.y) );
        const allz = series.flatMap(s => s.values.map(p => p.z) );
        const allt = series.flatMap(s => s.values.map(p => p.t) );
        const extentx = d3.extent(allx.concat(zeroScale(scatter.xAxis.scale)));
        const extenty = d3.extent(ally.concat(zeroScale(scatter.yAxis.scale)));
        const extentz = d3.extent(allz.concat(zeroScale(scatter.zAxis.scale)));
        const extentt = d3.extent(allt.concat(zeroScale(scatter.tAxis.scale)));

        if (allx.length == 0) {
          svg.append("text")
              .attr("text-anchor","middle")
              .attr("x",width/2)
              .attr("y",height/2)
              .attr("fill","#666")
              .attr("font-size",32)
              .text("No data available to plot for this set of projections or regions :(")
          return {
            highlight: (function () { return; }),
            unhighlight: (function () { return; }),
            saveFile: (function () { return false; })
          };
        }

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
        // legend
        const legdim = { left: 12, top: 55, width: 200 };
        const z_ = scaleFunc(scatter.zAxis.scale)
                        .domain(extentz)
                        .range([0, legdim.width]);
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
                            .attr("x", width - margin.right - 4)
                            .attr("y", -5)
                            .attr("fill", "#666")
                            .attr("text-anchor", "end")
                            // .attr("font-weight", "bold")
                            .attr("font-size", 20)
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
                        .attr("x", 8)
                        .attr("y", 12)
                        .attr("text-anchor", "start")
                        .attr("font-size", 20)
                        .attr("fill", "#666")
                        .text(scatter.yAxis.label)
                    );
        };
        const zLegend = function(g) {
            const segs = d3.pairs(d3.range(33).map(function(x) {
                // between 0 and width
                const xx     = x/32 * legdim.width;
                const xm     = z_.invert(xx);
                const xcolor = z(xm);
                // console.log({ xx: xx, xcolor: xcolor });
                return { xx: xx, xcolor: xcolor }
              }));
            g.attr("transform", `translate(${margin.left + legdim.left},${margin.top + legdim.top})`)
                .call(axisTickerZ(d3.axisBottom(z_),4))
                .call(g => g.append("g")
                            .attr("stroke-width",10)
                            .attr("fill","none")
                            .selectAll("line")
                            .data(segs)
                            .join("line")
                            .attr("x1", d => d[0].xx)
                            .attr("x2", d => d[1].xx+1)
                            .attr("y1", -4.5)
                            .attr("y2", -4.5)
                            .attr("stroke", d => d[0].xcolor)
                        )
                .call(g => g.append("text")
                        .attr("x",0)
                        .attr("y",-12.5)
                        .attr("text-anchor", "start")
                        .attr("font-weight", "bold")
                        .attr("font-size", 12)
                        .attr("fill","currentColor")
                        .text(scatter.zAxis.label)
                    );
        }
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
          subplot.selectAll("path").attr("stroke-width", d => d.name === name ? 3 : 1)
                .filter(d => d.name === name)
                .raise();
        }
        const unhighlight = subplot => function() {
            subplot.selectAll("path").attr("stroke-width",2);
        }

        const mkQuadPoints = ss => ss.flatMap(s =>
              s.values.map(p => ({ x: x(p.x), y: y(p.y), z: p.z, t: p.t, name: s.name }))
            );

        var currQuadPoints = mkQuadPoints(series);
        // quadtree to look up nearest point. note z is not encoded
        const quadtree = d3.quadtree()
                .x(d => d.x)
                .y(d => d.y)
                .extent([x(extentx[0]),y(extenty[0])], [x(extentx[1]),y(extenty[1])])
                .addAll(currQuadPoints);

        const requad = function(t_) {
            quadtree.removeAll(currQuadPoints);
            currQuadPoints = mkQuadPoints(sliceSeries(t_, t_ == extentt[1]));
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

          const colorline = crosshair.append("line")
                .style("stroke-width",8)
                .style("stroke","#fff")
                .style("fill","none")
                .attr("y1", -5.5)
                .attr("y2", 5.5);
          const colorcenter = crosshair.append("line")
              .style("stroke-width",2)
              .attr("fill","none")
              .attr("y1", -7)
              .attr("y2", 7);


          const syncDots = crosshair.append("g");

          function moved() {
            d3.event.preventDefault();
            const mouse0 = d3.mouse(this);
            const mouse = [Math.max(margin.left,Math.min(width-margin.right,mouse0[0])),
                           Math.max(margin.top,Math.min(height-margin.bottom,mouse0[1]))
                          ];
            const closest = quadtree.find(mouse[0], mouse[1], 75);
            const foundPoint = !(closest === undefined);
            const xc = foundPoint ? closest.x : mouse[0];
            const yc = foundPoint ? closest.y : mouse[1];
            const xm = x.invert(xc);
            const ym = y.invert(yc);

            crosshair.attr("transform", `translate(${xc},${yc})`);
            vertline.attr("y1", -yc+margin.top).attr("y2", -yc+height-margin.bottom);
            horizline.attr("x1", -xc+margin.left).attr("x2", -xc+width-margin.right);
            syncDots.selectAll("*").remove();

            const showx = fmtX(xm);
            const showy = fmtY(ym);
            toplabel.text(showx + ", " + showy);

            if (foundPoint) {
              ch_center.attr("fill",z(closest.z))
                       .on("click",null)
                       .on("click", function() {
                           moveSetSlider(closest.t);
                           play_stop();
                        })
                       .attr("cursor","pointer")
                       .attr("r",6.0)

              ch_center.selectAll("title").remove()
              ch_center.append("title").text("Jump to time " + fmtT(closest.t));
              highlight(closest.name);
              bottomlabel.text(closest.name);
              footlabel.text(
                    [fmtZ(closest.z),fmtT(closest.t)]
                                    .filter(st => st !== showx && st !== showy)
                                    .join(", ")
                );

              const zloc = z_(closest.z) + legdim.left + margin.left;
              colorline.attr("display", null);
              colorcenter.attr("display", null);
              colorline
                  .attr("transform",`translate(${-xc+zloc},${-yc+margin.top+legdim.top-4.5})`);
              colorcenter
                  .attr("transform",`translate(${-xc+zloc},${-yc+margin.top+legdim.top-4.5})`)
                  .attr("stroke",z(closest.z));

              const syncDotData = series.flatMap(function(s) {
                const cutoff = d3.bisector(p => p.t).right(s.values,closest.t);
                if (cutoff == 0 || s.name == closest.name) {
                    return [];
                } else {
                    return [s.values[Math.min(cutoff-1,s.values.length-1)]];
                }
              });
              if (validTimeScale) {
                syncDots.append("g")
                    .selectAll("circle")
                    .data(syncDotData)
                    .join("circle")
                    .attr("r", 3)
                    .attr("stroke",d => z(d.z))
                    .attr("stroke-width",2)
                    .attr("fill",null)
                    .attr("fill-opacity","0")
                    .attr("transform", d => `translate(${x(d.x)-xc},${y(d.y)-yc})`)
                    .style('pointer-events','none')
              }
            } else {
              colorline.attr("display", "none");
              colorcenter.attr("display", "none");
              ch_center.attr("r",4.0)
                       .attr("fill","white")
                       .attr("cursor",null);
              ch_center.selectAll("title").remove()
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

        const flatSegments = ss => ss.flatMap(s => s.segments.map (p => ({ name: s.name, pair: p })));
        // const mkTimed = timedSeries(series);

        const mainplot = svg.append("g")
        mainplot.attr("id","mainplot");

        mainplot.append("g")
            .call(xAxis);
        mainplot.append("g")
            .call(yAxis);
        mainplot.append("g")
            .call(zLegend);

        const subplot = mainplot.append("g")


        // click capture
        subplot.append("g")
          .append('rect')
          .style('visibility','hidden')
          .style('pointer-events','all')
          .attr('x',margin.left)
          .attr('y',margin.top)
          .attr('width',width - margin.right - margin.left)
          .attr('height',height - margin.top - margin.bottom);

        // yo this is it
        //
        // we can use line instead of path but it intereferes with selectors
        subplot.append("g")
             .selectAll("g")
             .data(flatSegments(series))
             .join("g")
             .append("path")
             .attr("fill", "none")
             .attr("stroke-width", 2)
             .attr("stroke-linejoin", "round")
             .attr("stroke-linecap", "round")
             .attr("stroke", d => z(d.pair[1].z))
             .attr("d", d => line(d.pair));

        const endDots = mainplot.append("g");

        subplot.call(hover, highlight(subplot),unhighlight(subplot));


        const setTime = function(subplot, t_) {
            if (validTimeScale) {
              subplot.selectAll("path")
                  .attr("display", d => (d.pair[0].t < t_) ? null : "none")
                  .attr("stroke-dasharray", function (d) {
                      if (d.pair[0].t < t_ && d.pair[1].t > t_) {
                          const dx = x(d.pair[1].x) - x(d.pair[0].x);
                          const dy = y(d.pair[1].y) - y(d.pair[0].y);
                          const dist = Math.sqrt( dx*dx + dy*dy );
                          const timeScaled = (t(t_) - t(d.pair[0].t)) / (t(d.pair[1].t) - t(d.pair[0].t));
                          return `${dist*timeScaled},${dist}`
                      } else {
                          return null;
                      }
                  });
            }

            endDots.selectAll("*").remove();

            const dotData = series.flatMap(function(s) {
              if (validTimeScale) {
                  const cutoff = d3.bisector(p => p.t).right(s.values,t_);
                  if (cutoff == 0) {
                      return [];
                  } else if (cutoff >= s.values.length) {
                      return [{
                          name: s.name
                        , last: last(s.values)
                      }];
                  } else {
                      const here = s.values[cutoff-1];
                      const there = s.values[cutoff];
                      const interp = d3.interpolate(here, there);
                      const timeScaled = (t(t_) - t(here.t)) / (t(there.t) - t(here.t));
                      return [{
                          name: s.name
                        , last: interp(timeScaled)
                      }]
                  }
              } else {
                  if (s.values.length > 0) {
                      return [{name: s.name, last: last(s.values)}];
                  } else {
                      return [];
                  }
              }
            });

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

            requad(t_);
        }

        const subslider = svg.append("g");
        const sliderino = tAxis(subslider.append("g"))
                // .on('onchange', v => setTime(subplot,v))
                .on('onchange', _.throttle(v => setTime(subplot,v), 50))
                // .on('end',v => requad(v));

        const button = svg.append("g")
                    .attr("transform",`translate(0,${height-margin.slider-15})`)
                    .style("cursor", validTimeScale ? "pointer" : "not-allowed")
                    .style('pointer-events','all');

        const drawButton = function(g,isPlaying,callback) {
            g.selectAll("*").remove();
            g.on("click",null)
                .on("click",callback);
            g.append("rect")
                .attr("width",30)
                .attr("height",30)
                .attr("rx",3)
                .style("fill",validTimeScale ? d3.schemeDark2[1] : "#999")
                .append("title")
                .text(isPlaying ? "Pause" : "Play");
            if (isPlaying) {
                g.append("rect")
                    .attr("width",6)
                    .attr("height",18)
                    .attr("x",6)
                    .attr("y",6)
                    .attr("rx",1)
                    .style("fill","white")
                    .append("title")
                    .text("Pause");
                g.append("rect")
                    .attr("width",6)
                    .attr("height",18)
                    .attr("x",18)
                    .attr("y",6)
                    .attr("rx",1)
                    .style("fill","white")
                    .append("title")
                    .text("Pause");
            } else {
                g.append("path")
                    .attr("d","M9 6 L9 24 L21 15 Z")
                    .style("fill","white")
                    .append("title")
                    .text("Play");
            }
        }


        const moveSetSlider = function(t) {
            sliderino.value(t);
            setTime(subplot,t);
            resting = t == extentt[1];
        }

        var timer = null;
        var resting = true;

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
        }
        const play_tick = function () {
            const currval = t(sliderino.value());
            const nextval = currval + playdata.stepAmt;
            const hitTheEnd = nextval > playdata.maxT;
            if (hitTheEnd) {
                if (resting) {
                    moveSetSlider(extentt[0]);
                } else {
                    resting = true;
                    moveSetSlider(extentt[1]);
                    return play_stop();
                }
            } else {
                moveSetSlider(t.invert(nextval));
            }
        }

        // window.sliderino = sliderino;
        // window.tt = t;

        button.call(drawButton,false,play_start);

        moveSetSlider(extentt[1]);

        if (!validTimeScale) {
            subslider.attr("display","none");
            svg.append("text")
                .attr("fill","none")
                .attr("x",margin.left-5)
                .attr("y", height-margin.slider+5)
                .attr("fill", "currentColor")
                .attr("font-style", "italic")
                .attr("font-size", 14)
                .text("(Time controls disabled for non-time axis)");
        }

        const saveFile = function (fname) {
            saveSvgAsPng(mainplot._groups[0][0], fname);
            return true;
        }


        window.subplot = subplot;
        return {
            highlight: highlight(subplot),
            unhighlight: unhighlight(subplot),
            // settime: (v => setTime(subPlot, v)),
            saveFile: saveFile
        };
    }
}



// takes a (maybe string)
exports._highlight = function(handleMaybe,interactors,name) {
    return function() {
        handleMaybe(name)(
            { nothing: (() => interactors.unhighlight())
            , just: (n => interactors.highlight(n))
            }
        );
    }
}

// takes a filename
exports._saveFile = function(interactors, fn) {
    return function() {
        return interactors.saveFile(fn);
    }
}
