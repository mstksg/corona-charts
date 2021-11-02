Changelog
=========

## Version 1.4.1

*November 1, 2021*

*   Allow graceful failure from missing data for a single country

## Version 1.4.0

*November 19, 2020*

*   Add "Active cases" base projection, defined as confirmed - deaths -
    recovered.
*   For US data, estimate recovered cases by subtracting deaths from confirmed
    cases, lagged 13 days.

## Version 1.3.2

*July 2, 2020*

*   Add "Vertex Date" parameter interpretation to quadratic model, to help
    gauge steepness.

## Version 1.3.1

*June 28, 2020*

*   Fixed inaccuracies in description/parameter interpretations of quadratic
    model, and regressions in date-based parameter interpretations.

## Version 1.3.0

*June 28, 2020*

*   Add quadratic fit, for linear changes in new cases.

## Version 1.2.0

*June 1, 2020*

*   Add "Per-Capita" transformation, allowing you to scale the number per
    million people in population.  See "Growth curve plots" link on the
    introduction box for an example.

## Version 1.1.0

*May 31, 2020*

*   Add "Lag" transformation, for the ability to lag (forward or backwards) a
    time series.  The intent is to be able to show more accurate associations
    with death rates and confirmed cases, as per [this CDC recommendation][cdc].

    [cdc]: https://wwwnc.cdc.gov/eid/article/26/6/20-0320_article?fbclid=IwAR3ActUE0gqlqQAdxb_ComHcb5P22tvaLWwp03UhzERAEJgI1eZgO_jNr5U

## Version 1.0.0

*May 27, 2020*

*   Initial Release
