
# Features

*   exponential/logistic fits and projections
*   responsive layout
*   fullscreen mode?
*   other datasets: australia, china, etc.
*   favicon
*   "helo" info hovers

# Fixes

*   loading a linear url for confirmed case count seems to display as logarithmic on 'scale' ui when 
*   the onLeave handler seems to be called too late when leaving the area
    * edit: what?
*   toggle switch for log/linear
*   touch on mobile
*   Better labeling system
    *   maybe just a white background
*   fast-forward to end button
*   missing data
*   apparent bug: restrict-before for dates
    * edit: what?

*   performance when loading:
    *   changing scales should be fast
    *   i tried cacheing but actually generating the data does none of the
        work.  what is the work?  the type checking/dependent types?  the url
        generation?  or the d3 plot itself?

# Someday

*   select to get statistics (mean, median, min, max)
*   icons
*   Discrete data: consider dot plots or bar graphs
*   localization
*   scale per capita
*   zoom in
*   fourier transform?  but this requires something on both axes...or at least
    to do them separately.
*   some sort of loading icon when  things are loading. but this is tricky
    because synchronous.
*   feedback for parse errors in form elements

# Meh

*   configure playback speed?
*   toggle sidebar on and off
*   path segments fall from the sky
*   disable recovered selector for US data
*   support adjusting symmetric log parameter
*   window should have final day as the label, not the center
*   text wrapping? is this feasible?
