
"use strict";

exports._mkSvg = function(elem, dim) {
    return function () {
        const svg = d3.select(elem)
                .append("svg")
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


// exports._drawSlider = function(svgdat, tscale) {
//     const svg = svgdat.svg;
//     const width = svgdat.dimensions.width;
//     const height = svgdat.dimensions.height;
//     const margin = { top: 20, right: 20, bottom: 20, left: 50 };

//     return function () {
//       tscale.range([ margin.left, width - margin.right ]);

//         // const t = scaleFunc(scatter.tAxis.scale)
//         //                 .domain(extentt)        // not nice
//         //                 .range([0, 1]);

//       const slider = svg.append("g")
//               .append("line")
//               .attr("x1",)

//         const yAxis = function(g) {
//             g.attr("transform", `translate(${margin.left},0)`)
//                 .call(axisTickerY(d3.axisLeft(y)))
//                 .call(g => g.selectAll(".tick line").clone()
//                                 .attr("stroke-opacity", 0.1)
//                                 .attr("x2", width - margin.left - margin.right)
//                      )
//                 .call(g => g.select(".tick:last-of-type text").clone()
//                         .attr("x", 3)
//                         .attr("text-anchor", "start")
//                         .attr("font-weight", "bold")
//                         .attr("font-size", 12)
//                         .text(scatter.yAxis.label)
//                     )
//         };


//     }

// }

//     const svg = svgdat.svg;
//     const width = svgdat.dimensions.width;
//     const height = svgdat.dimensions.height;
