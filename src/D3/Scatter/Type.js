
"use strict";

const msPerDay      = 24*60*60*1000;
const mjdShift      = 40586;
const fromMJD = mjd => new Date((mjd - mjdShift) * msPerDay);

exports._formatSType = (handleType,tp) =>
    handleType(tp)(
        { day:     (() => val => d3.timeFormat("%b %d")(fromMJD(val)))
        , days:    (() => d3.format(".3~s"))
        , "int":   (() => d3.format(".3~s"))
        , number:  (() => d3.format(".3~s"))
        , percent: (() => d3.format("+.3~p"))
        }
    );
