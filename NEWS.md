# envreportutils 0.8.0

* Added a set of functions to help create/customize leaflet maps:
  * Added new function `add_bc_home_button()`
  * Added new function `set_bc_view`
  * Added new function `set_bc_view_on_close`
  * Added new function `css_caaqs_copy`
  * Added new function `popup_caaqs`
  * Added new function `popup_combine_rows`
  * Added new function `popup_create_row`

* Added new function `soe_path()` to set the path to a file or folder on the SOE
shared drive to enable platform/user agnostic reading/writing to the SOE shared 
drive. Also added `set_soe_root()` so each user can set their path to the root
of the SOE shared drive.

# envreportutils 0.7

* Added new function `to_titlecase()`
* Added new `soft()` function to use ENV SOFT utility from within R

# envreportutils 0.6.3

* Added new function `report_percent()` to format percentages in a standard way.

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
