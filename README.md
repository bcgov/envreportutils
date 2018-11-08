
<!-- README.md is generated from README.Rmd. Please edit that file -->

# envreportutils 0.8.0

<a rel="Delivery" href="https://github.com/BCDevExchange/assets/blob/master/README.md"><img alt="In production, but maybe in Alpha or Beta. Intended to persist and be supported." style="border-width:0" src="https://assets.bcdevexchange.org/images/badges/delivery.svg" title="In production, but maybe in Alpha or Beta. Intended to persist and be supported." /></a>[![Travis-CI
Build
Status](https://travis-ci.org/bcgov/envreportutils.svg?branch=master)](https://travis-ci.org/bcgov/envreportutils)[![License](https://img.shields.io/badge/License-Apache%202.0-blue.svg)](https://opensource.org/licenses/Apache-2.0)

## Overview

An [R](https://www.r-project.org/) package with
[ggplot2](http://ggplot2.org/) themes and other functions commonly used
by the [Environmental Reporting
BC](http://www2.gov.bc.ca/gov/content?id=FF80E0B985F245CEA62808414D78C41B)
team when developing environmental reporting indicators and related
products.

## Features

**Plotting-related functions:**

  - `theme_soe()` - default `ggplot2` theme for Environmental Reporting
    BC graphs
  - `theme_soe_facet()` - default `ggplot2` theme for Environmental
    Reporting BC graphs using facetting
  - `bgc_colours()` - get colour codes for B.C. Biogeoclimatic (BGC)
    Zones
  - `svg_px()` - create svg for the web, specifying size in pixels
  - `png_retina()` - create png for retina display

**Data wrangling & sharing functions:**

  - `report_percent()` - perform standardized rounding of percentage
    values for reporting
  - `to_titlecase()` - simple helper function to convert `"AnY stRanGELy
    forMaTTed STring"` to `"Title Case"`
  - `get_data_licence()` - get the url, or a markdown or html-formatted
    link to one of several B.C. or Canadian licences
  - `soft()` - use ENV SOFT utility from within R

**Leaflet-related functions:**

  - `add_bc_home_button()` - add a ‘Zoom to B.C. button’ to a leaflet
    map
  - `set_bc_view()` - set leaflet map view to B.C.
  - `set_bc_view_on_close()`- re-centre map to B.C. on popup close
  - `popup_create_row()` - create a popup row div for leaflet maps
  - `popup_combine_rows()` - combine popup rows for leaflet maps
  - `css_caaqs_copy()` - create copy of CAAQS CSS styles for leaflet map
  - `popup_caaqs()` - create popup for CAAQS indicators

**Deprecated functions:**

  - `order_df()` - order a data frame using summary of a specified
    column or specify the order manually (this function is deprecated,
    see
    [`forcats::fct_reorder`](https://cran.r-project.org/web/packages/forcats/index.html)
    for the same functionality)
  - `multiplot()` - combine multiple `ggplot2` plots into one (this
    function is deprecated, see the
    [`cowplot`](https://cran.r-project.org/web/packages/cowplot/index.html)
    or [patchwork](https://github.com/thomasp85/patchwork) packages for
    the same functionality)

The package also installs an [RStudio
Addin](https://rstudio.github.io/rstudioaddins/) for adding a custom
footer to READMEs for projects maintained by [Environmental Reporting
BC](https://github.com/bcgov/EnvReportBC).

## Installation

You can install the package directly from this repository. To do so, you
will need the [devtools](https://github.com/hadley/devtools/) package:

``` r
install.packages("devtools")
```

Next, install the `envreportutils` package using
`devtools::install_github()`:

``` r
library("devtools")
install_github("bcgov/envreportutils")
```

## Project Status

This package is under continual development.

## Getting Help or Reporting an Issue

To report bugs/issues/feature requests, please file an
[issue](https://github.com/bcgov/envreportutils/issues/).

## How to Contribute

If you would like to contribute to the package, please see our
[CONTRIBUTING](CONTRIBUTING.md) guidelines.

Please note that this project is released with a [Contributor Code of
Conduct](CODE_OF_CONDUCT.md). By participating in this project you agree
to abide by its terms.

## License

    Copyright 2015 Province of British Columbia
    
    Licensed under the Apache License, Version 2.0 (the "License");
    you may not use this file except in compliance with the License.
    You may obtain a copy of the License at 
    
       http://www.apache.org/licenses/LICENSE-2.0
    
    Unless required by applicable law or agreed to in writing, software
    distributed under the License is distributed on an "AS IS" BASIS,
    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and
    limitations under the License.

This repository is maintained by [Environmental Reporting
BC](http://www2.gov.bc.ca/gov/content?id=FF80E0B985F245CEA62808414D78C41B).
Click [here](https://github.com/bcgov/EnvReportBC-RepoList) for a
complete list of our repositories on GitHub.
