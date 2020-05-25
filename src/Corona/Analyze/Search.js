
"use strict";

exports._bisectionSearch = function(mkMaybe, eps, f, mn, mx) {
    if (f(mn) * f(mx) > 0) {
        return mkMaybe.nothing();
    } else {
        var i   = 0;
        var a   = mn;
        var b   = mx;
        var p   = (mn + mx) / 2;
        var err = Math.abs(f(p));
        while (err > eps && i < 100) {
            if (f(a) * f(p) < 0) {
                b = p;
            } else {
                a = p;
            }
            p = (a + b)/2;
            err = Math.abs(f(p));
            i += 1;
        }
        if (i >= 100) {
            console.warn("search timed out after 100 steps")
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
    

