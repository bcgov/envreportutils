#' Create png for retina display
#' 
#' This is a drop-in replacement for the \code{\link[grDevices]{png}} function 
#' for creating images for the web for retina devices. Internally, it simply 
#' doubles the width, height, and resolution specified. The intention is then
#' that in the webpage that you would specify the \code{width} and \code{height} 
#' attributes in the html at the original resolution.
#'
#' @inheritParams grDevices::png
#'
#' @return A plot device is opened: nothing is returned to the R interpreter
#' @export
#' 
#' @seealso \code{\link[grDevices]{png}}
#'
#' @examples
#' 
#' # You want to display at 500 * 500 in the web:
#' png_retina("myplot.svg", width = 500, height = 500)
#' plot(x = 1:500, y = rnorm(500))
#' dev.off()
#' 
#' # Although the output image will be 1000 * 1000, in the html you would put:
#' # <img src="myplot.png", width="500", height="500" />
#' 
png_retina <- function(filename = "Rplot%03d.png", width = 480, height = 480, 
                       units = "px", pointsize = 12, bg = "white",  res = NA, 
                       ..., type = c("cairo", "cairo-png", "Xlib", "quartz"), 
                       antialias) {
  
  argslist <- as.list(match.call(expand.dots = TRUE)[-1])
  
  argslist$height <- height * 2
  argslist$width <- width * 2
  argslist$res <- ifelse(is.na(res), 144, res * 2)
  
  do.call(grDevices::png, argslist)
}

#' Create svg for the web, specifying size in pixels
#' 
#' This is a thin wrapper around \code{\link[svglite]{svglite}} 
#' for creating images for the web in svg format, but enables you specify the 
#' size in pixels, which is more useful for web work. Internally, it simply 
#' converts the width and height to inches at the resolution specified. 
#'
#' @inheritDotParams svglite::svglite
#' 
#' @param width The width of the plot in pixels. Default \code{800}.
#' @param height The height of the plot in pixels. Default \code{800}.
#' @param res The resolution at which it is displayed. Default \code{72}. 
#' You should rarely have to change this.
#'
#' @return A plot device is opened: nothing is returned to the R interpreter
#' @export
#' 
#' @seealso \code{\link[svglite]{svglite}}
#'
#' @examples
#' 
#' # You want to display at 500 * 500 in the web:
#' svg_px("myplot.png", width = 500, height = 500)
#' plot(x = 1:500, y = rnorm(500))
#' dev.off()
svg_px <- function(file = "Rplots.svg", width = 800, height = 800, res = 72, bg = "white", 
                    pointsize = 12, standalone = TRUE, system_fonts = list(), 
                    user_fonts = list()) {
  
  if (!(is.numeric(width) && is.numeric(height) && is.numeric(res))) {
    stop("width, height, and res must be numeric")
  }
  
  if (width < 50 || height < 50) {
    warning("This seems like a very small value for width or height. These values represent pixels.")
  }
  
  
  width <- width / res
  height <- height / res
  
  svglite::svglite(file = file, width = width, height = height, bg = bg, 
                   pointsize = pointsize, standalone = standalone, 
                   system_fonts = system_fonts, user_fonts = user_fonts)
}
