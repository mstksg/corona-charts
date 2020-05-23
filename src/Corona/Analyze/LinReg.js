
"use strict";

// getPair :: Point -> (forall r. Number -> Number -> r) -> r
exports._linReg = function(getPair, pts) {
    var n = 0;
    var x = 0;
    var y = 0;
    var x2 = 0;
    var y2 = 0;
    var xy = 0;
    for (const pt of pts) {
        getPair(pt)(function(xx,yy) {
            n += 1;
            x += xx;
            y += yy;
            x2 += xx*xx;
            y2 += yy*yy;
            xy += xx*yy;
        });
    }

    const mx = x / n;
    const my = y / n;
    const mx2 = x2 / n;
    const my2 = y2 / n;
    const mxy = xy / n;

    const cxy = mxy - mx * my;
    const vx2 = mx2 - mx * mx;
    const vy2 = my2 - my * my;
    const beta = cxy / vx2;
    const alpha = my - beta * mx;
    const r2 = beta * beta * vx2 / vy2;
    return {
      linReg: { alpha: alpha, beta: beta },
      r2: r2
    }
}
