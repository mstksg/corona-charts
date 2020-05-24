
# Features

*   exponential/logistic fits and projections
    * maybe can fit on the deltas too but meh
        * exponential decay: fit against cap-x
*   responsive layout
*   fullscreen mode?
*   other datasets: australia, china, etc.
*   favicon
*   "helo" info hovers

# Fixes

*   loading a linear url for confirmed case count seems to display as logarithmic on 'scale' ui when 
    * loading log url for percent growth displays linear on UI but is
      logartihmic
    * also removing ops seems to reset the scale
*   some weird behavior when first adding 'moving average'?  it starts as 1 and
    you chagnge to 2 but it sitll stays same.
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
*   need a good way to show  missing data in log plots where number is less
    than 0
*   country spec should be left in url if given originally
*   performance when loading:
    *   changing scales should be fast
    *   i tried cacheing but actually generating the data does none of the
        work.  what is the work?  the type checking/dependent types?  the url
        generation?  or the d3 plot itself?
    *   it is probably the d3 plot itself
*   inifnite loop: south korea decay

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
