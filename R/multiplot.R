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


#'Plot multiple plots in a single pane. This function stills work but will be removed (defunct)
#' in the next version of the package. The `cowplot` and `patchwork` packages have the same functionality.
#'
#'ggplot objects can be passed in ..., or to plotlist (as a list of ggplot 
#'objects)
#'@import ggplot2
#'@import grid
#'@export
#'
#'@param ... Two or more ggplot2 objects.
#'@param  plotlist A list of ggplot2 objects (optional).
#'@param  cols Number of columns in layout.
#'@param  layout A matrix specifying the layout. If present, 'cols' is ignored.
#'@param  title Optional title as a character string.
#'@param  widths A vector of relative column widths eg. c(3,2).
#'@param  heights A vector of relative column heights eg. c(3,2).
#'@param  titlefont The font of the title.
#'@param  titleface The font face (1 = normal, 2 = bold, 3 = italic, 4 = bold 
#'  italic).
#'@param  titlesize The size of the title font.
#'  
#'@details If plotting three plots and the layout is something like 
#'  matrix(c(1,2,3,3), nrow=2, byrow=TRUE), then plot 1 will go in the upper 
#'  left, 2 will go in the upper right, and 3 will go all the way across the 
#'  bottom.  To save, you must use the desired device (eg \code{png()}), or save
#'  from the RStudio Viewer.
#'  
#'  This extends the
#'  \href{http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)}{multiplot}
#'  function by Winston Chang in the \href{http://www.cookbook-r.com}{Cookbook 
#'  for R}.
#'  
#'@return NULL (invisibly)
#' @examples \dontrun{
#' library("ggplot2")
#' plot1 <- ggplot(iris, aes(x = Species, y = Sepal.Length)) + 
#'    geom_bar(stat = "identity")
#' plot2 <- ggplot(mtcars, aes(x = mpg, y = disp)) + 
#'    geom_smooth()
#' multiplot(plot1, plot2, cols = 2, widths = c(3,2), title = "My two unrelated plots")
#' multiplot(plot1, plot2, cols = 1, heights = c(10,2), title = "My two unrelated plots")
#' myplots <- list(plot1, plot2, plot1)
#' multiplot(plotlist = myplots, layout =matrix(c(1,2,3,3), nrow=2), 
#'      heights = c(1,3), widths = c(3,4), title = "My three unrelated plots")
#' ## Adjusting fonts
#' library(extrafont)
#' loadfonts()
#' multiplot(plotlist = myplots, layout =matrix(c(1,2,3,3), nrow=2),
#'           heights = c(1,3), widths = c(3,4), title = "My three unrelated plots", 
#'           titlefont = "Wingdings", titleface = 4, titlesize = 20)
#'}
multiplot <- function(..., plotlist=NULL, cols=1, layout=NULL, widths=NULL, heights=NULL, 
                      title=NULL, titlefont = "", titleface = 1, titlesize = 16) {
  
  .Deprecated("cowplot")
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (!is.null(title)) { # Add a narrow row at the top for the title
    layout <- rbind(rep(0,ncol(layout)),layout)
    if (is.null(heights)) {
      plotrows <- nrow(layout)-1
      rowheights <- c(0.1, rep(1,plotrows)/plotrows)
    } else {
      rowheights <- c(0.1, heights/sum(heights))
    }
  } else {
    if (is.null(heights)) {
      rowheights <- rep(1,nrow(layout))  
    } else {
      rowheights <- heights
    }
  }
  
  if (is.null(widths)) {
    colwidths <- rep(1, cols)
  } else {
    colwidths <- widths
  }
  
  if (numPlots==1) {
    
    return(plots[[1]] + labs(title=title))
    
  } else {
    # Set up the page
    grid::grid.newpage()
    grid::pushViewport(grid::viewport(layout = grid::grid.layout(nrow(layout), 
                                                                 ncol(layout), 
                                                                 widths=colwidths, 
                                                                 heights=rowheights)))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = grid::viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
    
    if (!is.null(title)) {
      grid::grid.text(title, vp = grid::viewport(layout.pos.row = 1
                                     , layout.pos.col = 1:ncol(layout)), 
                gp = grid::gpar(fontfamily = titlefont, fontface = titleface, 
                          fontsize = titlesize))
    }
    
  }
return(invisible(NULL))
}
