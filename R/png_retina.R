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
#' @return A plot device is opened: nothing is returned to the R interpreter.
#' @export
#' 
#' @seealso \code{\link[grDevices]{png}}
#'
#' @examples
#' 
#' # You want to display at 500 * 500 in the web:
#' png_retina("myplot.png", width = 500, height = 500)
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
