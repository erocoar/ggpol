## Test environments
* local OS X install, R 3.4.3
* local Windows install, R 3.5.0
* win builder (devel and release)

## R CMD check results
There were no ERRORs, WARNINGs or NOTEs.

## Changes:
* `StatBoxJitter` now inherits from `StatBoxplot` rather than `Stat`, making for slimmer bars.
* `geom_boxjitter()` does not require `x` argument anymore.
* Added import of `ggplot2::resolution()`, `ggplot2::alpha()`, `ggplot2::position_dodge2()` and `ggplot2:PositionJitter` to `geom_boxjitter()`. (#2)*  Added import of `ggplot2::.pt` to `geom_bartext()`.
*  Added import of ``grid::unit()`, `ggplot2::zeroGrob()` and `ggplot2::render_axes()` to `facet_share()`.