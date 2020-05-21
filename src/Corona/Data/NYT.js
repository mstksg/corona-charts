
"use strict";

// type CoronaData =
//     { start  :: Day
//     , counts :: O.Object (Counts (Array Int))
//     }


// type Counts a =
//     { confirmed :: a
//     , deaths    :: a
//     , recovered :: a
//     }

const zeroPad = (xs, n) => n > xs.length ?
            Array(n - xs.length).fill(0).concat(xs)
          : xs
          ;

exports.buildCorona = function (rows) {
    var start;
    const counts = {};

    for (const r of rows) {
        const state = r[1];
        const cases = parseInt(r[3]);
        const deaths = parseInt(r[4]);

        if (!start) {
            const datesegs = r[0].split('-');
            start = new Date(datesegs[0],datesegs[1],datesegs[2]);
        }

        if (counts[state]) {
            counts[state].confirmed.push(cases);
            counts[state].deaths.push(deaths);
        } else {
            counts[state] = { confirmed: [cases], deaths: [deaths] };
        }
    }

    var maxcount = 0;
    for (const state in counts) {
        maxcount = Math.max(maxcount,counts[state].confirmed.length);
    }

    for (const state in counts) {
        counts[state].confirmed = zeroPad(counts[state].confirmed,maxcount);
        counts[state].deaths = zeroPad(counts[state].deaths,maxcount);
    }

    return { start: start, counts: counts };
}
