
"use strict";

exports.incrDate = function(n) {
  return function(d) {
    var out = new Date();
    out.setDate(d.getDate() + n);
    return out;
  }
}

// Alias require to prevent webpack or browserify from actually requiring.
var req = typeof module === "undefined" ? undefined : module.require;
var util = req === undefined ? undefined : req("util");

exports.traceTime = function () {
  return function (x) {
    return function (k) {
      // node only recurses two levels into an object before printing
      // "[object]" for further objects when using console.log()
      // if (util !== undefined) {
      //   console.log(util.inspect(x, { depth: null, colors: true }));
      // } else {
      //   console.log(x);
      // }
      console.time(x);
      var res = k({});
      console.timeEnd(x);
      return res;
    };
  };
};

// exports.spy = function () {
//   return function (tag) {
//     return function (x) {
//       if (util !== undefined) {
//         console.log(tag + ":", util.inspect(x, { depth: null, colors: true }));
//       } else {
//         console.log(tag + ":", x);
//       }
//       return x;
//     };
//   };
// };
