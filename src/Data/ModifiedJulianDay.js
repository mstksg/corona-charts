
"use strict";

exports._parseIso = function (just, nothing, v) {
    const d = new Date(v);
    if (!isNaN(d)) {
        return just(d);
    } else {
        return nothing();
    }
}

exports._toIso = d => d.toISOString().slice(0,10);
