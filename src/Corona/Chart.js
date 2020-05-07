
"use strict";

exports.incrDate = function(n) {
  return function(d) {
    var out = new Date();
    out.setDate(d.getDate() + n);
    return out;
  }
}
