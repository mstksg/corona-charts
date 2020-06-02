
"use strict";

exports._intersectionWith = function (f, m0, n0) {
  var m = {};
  for (var k in m0) {
    if (hasOwnProperty.call(m0, k)) {
      if (hasOwnProperty.call(n0, k)) {
        m[k] = f(m0[k])(n0[k]);
      }
    }
  }
  return m;
};
