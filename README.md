Coronavirus Data Plotter
========================

*COVID-19 / Coronavirus interactive, customizable data visualization and
animation portal. Together, we can make it through this.*

Source repository for <https://coronavirus.jle.im>, the Coronavirus Data
Plotter.

From the front page:

>   On this data portal, explore different aspects of COVID-19 /
>   Coronavirus time evolution. I put this together because I saw a lot of
>   different visualizations and data views over many different sites but
>   no way to explore them all within the same tool. I hope this will be
>   as helpful for you as it was for me, in helping process and understand
>   the torrent of information and numbers we see in the news and on our
>   phones every day.

>   Created and maintained by [Justin Le][] ([🐦][twitter]) in association
>   with [Chapman University][] and [Dr. Hesham El-Askary][hesham]
>   (Director, Computational and Data Sciences).

>   [Justin Le]: https://blog.jle.im/
>   [twitter]: https://twitter.com/mstk
>   [Chapman University]: https://chapman.edu/
>   [hesham]: https://www.chapman.edu/our-faculty/hesham-el-askary

>   Nation-level data provided by [John Hopkins University][JHU] and US
>   state-level data provided by the [New York Times][NYT], typically
>   updated daily around 00:00 UTC and 16:00 UTC, respectively.  Much
>   thanks to [Aatish Bhatia][] and [1point3acres][] for inspiration and
>   helping show the power of data-driven interactive visualization in
>   this time.

>   [JHU]: https://github.com/CSSEGISandData/COVID-19
>   [NYT]: https://github.com/nytimes/covid-19-data
>   [Aatish Bhatia]: https://aatishb.com/covidtrends/
>   [1point3acres]: https://coronavirus.1point3acres.com/en

>   These are truly unprecedented times, but it in a lot of cases it has
>   also brought out the best in all of us — the only way we will get
>   through all of this is by coming together to protect those most
>   vulnerable.  [Stay safe][], stay at home, and be mindful of the impact
>   of your presence in public areas and the power you personally wield to
>   make a difference.  Take care of your physical [and mental
>   health][mentalhealth]!  If you are in a situation of financial
>   stability, consider contributing what you can spare ([Give Directly][]
>   / [more info][]).  We can make it through this, together ❤️.

>   [Stay safe]: https://www.cdc.gov/coronavirus/2019-ncov/prepare/prevention.html
>   [mentalhealth]: https://bit.ly/WHO-mental-health
>   [Give Directly]: https://www.givedirectly.org/
>   [more info]: https://www.givewell.org/charities/give-directly

>   Application developed in [Purescript][] and powered by strongly and
>   dependently typed functional programming, love, and coffee ☕. All code
>   is open-sourced and [available on Github][source].  [Code
>   contributions][contributions] and [ideas][] welcomed and greatly
>   appreciated!

>   [Purescript]: https://www.purescript.org/
>   [source]: https://github.com/mstksg/corona-charts
>   [contributions]: https://github.com/mstksg/corona-charts/pulls
>   [ideas]: https://github.com/mstksg/corona-charts/issues


Code Background
---------------

The overall infrastructure is built in *yarn* scripts.  This is actually my
first ever full javascript-based project, so admittedly I'm not too familiar
with the ecosystem.

This project also served as a learning project for me for [Purescript][]
(programming language), [Halogen][] (UI framework), and [D3.js][] (interactive
visualizations), so a lot of the actual code itself may not be exactly
idiomatic.  My programming background is a Haskell tradition where I heavily
use [dependent types][] to ensure type safety and correctness and help guide my
implementation and make things more smooth during the development and
maintenance process...though I admit that using a off-mainstream code
discipline/style that is not exactly idiomatic within the already
not-very-common language I am using makes the barrier for contribution pretty
high.  However, I am definitely willing to help explain any aspects of the
implementation if you have any questions.

[Purescript]: https://www.purescript.org/
[Halogen]: https://github.com/purescript-halogen/purescript-halogen
[D3.js]: https://d3js.org/
[dependent types]: https://blog.jle.im/entries/tagged/dependent-types.html
