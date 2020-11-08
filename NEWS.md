# ggpol 0.0.7

## Minor changes
- updated documentation, thanks to @romagnolid
- fixed unit error when creating shared axes, thanks to @martinschmelzer

# ggpol 0.0.6

## Minor changes
- fix `jitter.seed` argument for `geom_boxjitter`
- internal changes in `facet_share` to make it compatible with new versions of `ggplot2` and `grid`

# ggpol 0.0.5

## New features
- `geom_confmat` for easy embedding of confusion matrices in `ggplot2` pipeline.

## Minor changes
- `geom_arcbar` permits setting `sep = 0` for no separation between individual parts.

# ggpol 0.0.4

## Minor changes
- `StatBoxJitter` now inherits from `StatBoxplot` rather than `Stat`, making for slimmer bars.
- `geom_boxjitter()` does not require `x` argument anymore.
-  Added import of `ggplot2::resolution()`, `ggplot2::alpha()`, `ggplot2::position_dodge2()` and `ggplot2:PositionJitter` to `geom_boxjitter()`. (#2)
-  Added import of `ggplot2::.pt` to `geom_bartext()`.
-  Added import of ``grid::unit()`, `ggplot2::zeroGrob()` and `ggplot2::render_axes()` to `facet_share()`.


# ggpol 0.0.3

## Minor changes
- `facet_share` fixed for `ggplot` version `3.0.0`.


# ggpol 0.0.2

## New features
- `geom_bartext` for otherwise overlapping bar chart labels

## Minor changes
- `facet_share` axis spacing has been fixed for vertical direction


# ggpol 0.0.1

## Major changes
- First commit

## New features
- `facet_share` for two plots sharing an axis
- `geom_arcbar` for arc bar diagrams with optional spacing
- `geom_boxjitter` for hybrid boxplots with optional errorbars
- `geom_circle` for circles with given radius
- `geom_parliament` for parliament diagrams
- `geom_tshighlight` for timeseries highlighting
