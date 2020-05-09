
"use strict";

exports.testRec = function(rec) {
  return rec.foo;
}

exports.logMe = function(x) {
  return function() {
    console.log(x);
  }
}
