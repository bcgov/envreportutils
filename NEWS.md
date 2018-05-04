# envreportutils 0.6.3

* Added new function report_percent() to format percentages in a standard way.

# envreportutils 0.6.2

* Dropped `ggthemes` dependency for `theme_soe()` and `theme_soe_facet` (#5).

# envreportutils 0.6.0.99999

* Added function `svg_px()` for exporting charts as svg, specifying size in pixels.
* Added `soft()` function for sending files via ENV SOFT service.

# envreportutils 0.6.1

* Added function `png_retina()` - a drop in replacement for `png()` to produce 
retina-quality graphics.

# envreportutils 0.5.1

* Two functions deprecated, functionality available in other CRAN packages.

# envreportutils 0.5

* Numerous functions defunct, most of them moved to [bcgovr](https://github.com/bcgov/bcgovr).

# envreportutils 0.4.1

* Updated `devex_badge()` to refer to new location of BCDevExchange project state badges. 

# envreportutils 0.4

* Added three RStudio Addins:
  - Add Boilerplate Apache header to top of current document
  - Add EnvreportBC footer to bottom of current document (for README)
  - Add Rmarkdown code block to generate BCDevexchange badge (for README.Rmd)

# envreportutils 0.3.1

* Added a `NEWS.md` file to track changes to the package.
* Fixed a bug in `theme_soe()` and `theme_soe_facet()` where panel.border was specified twice (#7)
