
"use strict";


exports._formatSType = (handleType,tp) =>
    handleType(tp)(
        { day:     (() => d3.timeFormat("%b %d"))
        , days:    (() => d3.format(".3~s"))
        , "int":   (() => d3.format(".3~s"))
        , number:  (() => d3.format(".3~s"))
        , percent: (() => d3.format("+.3~p"))
        }
    );
