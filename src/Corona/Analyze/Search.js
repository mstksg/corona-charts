
"use strict";

exports._bisectionSearch = function(mkMaybe, eps, f, mn, mx) {
    if (f(mn) * f(mx) > 0) {
        return mkMaybe.nothing();
    } else {
        var a   = mn;
        var b   = mx;
        var p   = (mn + mx) / 2;
        var err = Math.abs(f(p));
        while (err > eps) {
            if (f(a) * f(p) < 0) {
                b = p;
            } else {
                a = p;
            }
            p = (a + b)/2;
            err = Math.abs(f(p));
        }
        return mkMaybe.just(p);
    }
}

// -- if f(a)*f(b)>0 
// --     p = 0/0;
// -- else
// --     p = (a + b)/2;
// --     err = abs(f(p));
// --     while err > 1e-7
// --       if f(a)*f(p)<0 
// --           b = p;
// --       else
// --           a = p;          
// --       end
// --       p = (a + b)/2; 
// --       err = abs(f(p));
// --     end
// -- end
    

