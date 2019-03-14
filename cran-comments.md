## Test environments
* local OS X install, R 3.5.0
* local Windows install, R 3.5.2
* win builder (devel and release)

## R CMD check results
There were no ERRORs, WARNINGs or NOTEs.

## Changes:
* added `geom_confmat` for easy embedding of confusion matrices in `ggplot2` pipeline.
* fixed bug in `geom_arcbar`. It now permits setting `sep = 0` for no separation between individual parts.