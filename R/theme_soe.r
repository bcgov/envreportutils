# Copyright 2015 Province of British Columbia
# 
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
# 
# http://www.apache.org/licenses/LICENSE-2.0
# 
# Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and limitations under the License.

#' Default theme for EnvReportBC graphs and plots
#' 
#' @import ggplot2 extrafont
#' 
#' @param  base_size base font size (default = 12)
#' @param  base_family base font family (default = Verdana)
#' @export
#' @keywords plotting theme
#' @return returns a plot theme

theme_soe <- function(base_size=12, base_family="Verdana") {
  thm <- theme_soe_foundation(base_size = base_size, base_family = base_family)
  thm
}  

#' BC sans theme for EnvReportBC graphs and plots
#' 
#' Function applies BC sans font to standard SOE theme. 
#' 
#' @import ggplot2 showtext
#' 
#' @param  base_size base font size (default = 12)
#' @param  base_family base font family (default = BC Sans)
#' @export
#' @keywords plotting theme
#' @return returns a plot theme

theme_bc_sans <- function(base_size=12, base_family="BCSans") {
  font_add(family = "BCSans", regular = "/inst/fonts/BCSans-Regular.ttf", 
           italic = "/inst/fonts/BCSans-Italic.ttf",
           bold = "/inst/fonts/BCSans-Bold.ttf",
           bolditalic = "/inst/fonts/BCSans-BoldItalic.ttf"
           )
  
  thm <- theme_soe_foundation(base_size = base_size, base_family = base_family)
  thm
}  


#' Default theme for EnvReportBC facetted graphs and plots
#' 
#' @import ggplot2
#' @param  base_size base font size (default = 12)
#' @param  base_family base font family (default = Verdana)
#' @export
#' @keywords plotting theme
#' @return a ggplot2 theme

theme_soe_facet <- function(base_size = 12, base_family = "Verdana") {
  
  theme_soe_foundation(base_size = base_size, base_family = base_family) + 
    theme(
      panel.spacing = unit(.6,"lines"),
      panel.border = element_rect(colour = "black", fill = NA),
      strip.background = element_rect(colour = "black", fill = "grey85"))
  
}

theme_soe_foundation <- function(base_size, base_family) {
    theme_grey() + theme(
      text = element_text(colour = "black"),
      line = element_line(colour = "black", size = 0.5,
                          linetype = 1, lineend = "butt"),
      rect = element_rect(fill = "white", colour = "black",
                          size = 0.5, linetype = 1),
      axis.line = element_line(colour = "black"),
      axis.line.x = element_line(colour = "black"),
      axis.line.y = element_line(colour = "black"),
      axis.text = element_text(colour = 'black'),
      axis.text.y = element_text(hjust = 1),
      axis.ticks = element_blank(),
      plot.title = element_text(vjust = 2),
      legend.title = element_text(face = "plain"),
      panel.background = element_blank(),
      panel.border = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(colour = "grey80",size = 0.5),
      axis.title.y = element_text(vjust = 1, angle = 90),
      axis.title.x = element_text(vjust = 0),
      panel.spacing = unit(0.25, "lines"),
      plot.background = element_blank(),
      legend.key = element_blank(),
      complete = TRUE)
  
}

