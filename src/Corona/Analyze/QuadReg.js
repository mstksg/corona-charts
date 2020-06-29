
"use strict";

// getPair :: Point -> (forall r. Number -> Number -> r) -> r
exports._quadReg = function(getPair, pts) {
    var n = 0;
    var x = 0;
    var y = 0;
    var x2 = 0;
    var y2 = 0;
    var x3 = 0;
    var x4 = 0;
    var xy = 0;
    var x2y = 0;
    for (const pt of pts) {
        getPair(pt)(xx => function(yy) {
            n += 1;
            x += xx;
            y += yy;
            x2 += xx*xx;
            y2 += yy*yy;
            x3 += xx*xx*xx;
            x4 += xx*xx*xx*xx;
            xy += xx*yy;
            x2y += xx*xx*yy;
        });
    }

    // https://www.varsitytutors.com/hotmath/hotmath_help/topics/quadratic-regression

             // -x2^3     + 2 x x2 x3 - n x3^2  - x^2 x4 + n x2 x4
    const det = -x2*x2*x2 + 2*x*x2*x3 - n*x3*x3 - x*x*x4 + n*x2*x4;

    // { {-x^2 + n x2, x x2 - n x3, -x2^2 + x x3}
    const m11 = (-x*x + n*x2) / det;
    const m21 = (x*x2 - n*x3) / det;
    const m31 = (-x2*x2 + x*x3) / det;
    // , {x x2 - n x3, -x2^2 + n x4, x2 x3 - x x4}
    const m12 = (x*x2 - n*x3) / det;
    const m22 = (-x2*x2 + n*x4) / det;
    const m32 = (x2*x3 - x*x4) / det;
    // , {-x2^2 + x x3, x2 x3 - x x4, -x3^2 + x2 x4}
    const m13 = (-x2*x2 + x*x3) / det;
    const m23 = (x2*x3 - x*x4) / det;
    const m33 = (-x3*x3 + x2*x4) / det;

    const alpha = m11 * x2y + m21 * xy + m31 * y;
    const beta  = m12 * x2y + m22 * xy + m32 * y;
    const gamma = m13 * x2y + m23 * xy + m33 * y;
    //   c^2
    // + 2 b c x
    // + b^2 x^2
    // + 2 a c x^2
    // + 2 a b x^3
    // + a^2 x^4
    // - 2 c y
    // - 2 b x y
    // - 2 a x^2 y
    // + y^2
    const sse = n * gamma * gamma
              + 2 * beta * gamma * x
              + beta * beta * x2
              + 2 * alpha * gamma * x2
              + 2 * alpha * beta * x3
              + alpha * alpha * x4
              - 2 * gamma * y
              - 2 * beta * xy
              - 2 * alpha * x2y
              + y2;
    // Sum(mu^2 - 2 mu y + y^2)
    // = n*mu^2 - 2*mu*Sum(y) + Sum(y^2)
    // = n*(Sum(y)/n)^2 - 2*(Sum(y)/n)*Sum(y) + Sum(y^2)
    // = n*Sum(y)^2/n^2 - 2*Sum(y)/n*Sum(y) + Sum(y^2)
    // = Sum(y)^2/n - 2*Sum(y)^2/n + Sum(y^2)
    // = Sum(y^2) - Sum(y)^2/n;
    const sst = y2 - y*y/n;

    // console.log(det);
    // console.log(n,x,y,x2,y2,x3,x4,xy,x2y);
    // console.log(-x2*x2*x2, 2*x*x2*x3, -n*x3*x3, -x*x*x4, n*x2*x4);
    // console.log(alpha, beta, gamma);

    return {
      quadReg: { alpha: alpha, beta: beta, gamma: gamma },
      r2: 1-sse/sst
    };
}
