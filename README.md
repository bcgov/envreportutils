
<!-- README.md is generated from README.Rmd. Please edit that file -->

# envreportutils 0.6.3

<div id="devex-badge">

<a rel="Delivery" href="https://github.com/BCDevExchange/assets/blob/master/README.md"><img alt="In production, but maybe in Alpha or Beta. Intended to persist and be supported." style="border-width:0" src="https://assets.bcdevexchange.org/images/badges/delivery.svg" title="In production, but maybe in Alpha or Beta. Intended to persist and be supported." /></a>[![Travis-CI
Build
Status](https://travis-ci.org/bcgov/envreportutils.svg?branch=master)](https://travis-ci.org/bcgov/envreportutils)

</div>

## Overview

An [R](https://www.r-project.org/) package with
[ggplot2](http://ggplot2.org/) themes and other functions commonly used
by the [Environmental Reporting
BC](http://www2.gov.bc.ca/gov/content?id=FF80E0B985F245CEA62808414D78C41B)
team when developing environmental reporting indicators and related
products.

## Features

Currently there are six main functions:

  - `theme_soe()` - default `ggplot2` theme for Environmental Reporting
    BC graphs
  - `theme_soe_facet()` - default `ggplot2` theme for Environmental
    Reporting BC graphs using facetting
  - `multiplot()` - combine multiple `ggplot2` plots into one (this
    function is deprecated, see the
    [`cowplot`](https://cran.r-project.org/web/packages/cowplot/index.html)
    package for the same functionality)
  - `bgc_colours()` - get colour codes for B.C. Biogeoclimatic (BGC)
    Zones
  - `get_data_licence()` - get the url, or a markdown or html-formatted
    link to one of several B.C. or Canadian licences
  - `order_df()` - order a data frame using summary of a specified
    column or specify the order manually (this function is deprecated,
    see
    [`forcats::fct_reorder`](https://cran.r-project.org/web/packages/forcats/index.html)
    for the same functionality)
  - `report_percent()` - perform standardized rounding of percentage
    values for reporting.

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
