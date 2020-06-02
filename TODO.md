
# Features

*   other datasets: australia, china, etc.
*   fullscreen

# Fixes

*   some weird behavior when first adding 'moving average'?  it starts as 1 and
    you chagnge to 2 but it sitll stays same.
*   performance when loading:
    *   changing scales should be fast
    *   i tried cacheing but actually generating the data does none of the
        work.  what is the work?  the type checking/dependent types?  the url
        generation?  or the d3 plot itself?
    *   it is probably the d3 plot itself

*   the onLeave handler seems to be called too late when leaving the area
    * edit: what?
*   apparent bug: restrict-before for dates
    * edit: what?

# Someday

*   select to get statistics (mean, median, min, max)
*   icons
*   Discrete data: consider dot plots or bar graphs
*   localization
*   zoom in
*   fourier transform?  but this requires something on both axes...or at least
    to do them separately.
*   feedback for parse errors in form elements

# Meh

*   configure playback speed?
*   toggle sidebar on and off
*   path segments fall from the sky
*   disable recovered selector for US data
*   support adjusting symmetric log parameter
*   text wrapping? is this feasible?
*   toggle switch for log/linear
*   fast-forward to end button
*   window should have final day as the label, not the center
*   some sort of loading icon when  things are loading. but this is tricky
    because synchronous.
