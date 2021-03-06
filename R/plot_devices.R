#' Create png for retina display
#' 
#' `png_retina` is a drop-in replacement for the \code{\link[grDevices]{png}} function 
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
  
  width <- width * 2
  height <- height * 2
  res <- ifelse(is.na(res), 144, res * 2)
  
  grDevices::png(filename = filename, width = width, height = height, units = units, 
                 pointsize = pointsize, bg = bg, res = res, ..., 
                 type = type, antialias = antialias)
}

#' Create svg for the web, specifying size in pixels
#' 
#' This is a thin wrapper around \code{\link[svglite]{svglite}} 
#' for creating images for the web in svg format, but enables you specify the 
#' size in pixels, which is more useful for web work. Internally, it simply 
#' converts the width and height to inches at the resolution specified. 
#'
#' @inheritParams svglite::svglite
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

#' Save a ggplot to a png for retina display
#' 
#' `save_png_retina` is a wrapper function around `png_retina` analagous to 
#' [ggplot2::gsave()]
#'
#' @param x a ggplot2 object
#' @inheritParams png_retina
#' @describeIn png_retina
#'
#' @return NULL
#' @export
#' 
#' @examples 
#' if (suppressPackageStartupMessages(require("ggplot2", quietly = TRUE))) {
#' p <- ggplot(mtcars, aes(x = cyl, y = mpg)) + geom_point()
#' file <- tempfile(fileext = ".png")
#' save_png_retina(p, file)
#' }
save_png_retina <- function(x, filename = "Rplot%03d.png", width = 480, height = 480, 
                            units = "px", pointsize = 12, bg = "white",  res = NA, 
                            ..., type = c("cairo", "cairo-png", "Xlib", "quartz"), 
                            antialias) {
  on.exit(graphics_exit(grDevices::dev.cur()))
  png_retina(filename = filename, width = width, height = height, units = units,
             pointsize = pointsize, bg = bg, res = res, ..., type = type, 
             antialias = antialias)
  graphics::plot(x)
  invisible(NULL)
}

#' Save a ggplot to an svg file using pixel dimensions
#' 
#' `save_svg_px` is a wrapper function around `svg_px` analagous to 
#' [ggplot2::gsave()]
#'
#' @param x a ggplot2 object
#' @inheritParams svg_px
#' @describeIn svg_px
#'
#' @return NULL
#' @export
#' 
#' @examples 
#' if (suppressPackageStartupMessages(require("ggplot2", quietly = TRUE))) {
#' p <- ggplot(mtcars, aes(x = cyl, y = mpg)) + geom_point()
#' file <- tempfile(fileext = ".svg")
#' save_svg_px(p, file)
#' }
save_svg_px <- function(x, file = "Rplots.svg", width = 800, height = 800, 
                        res = 72, bg = "white", pointsize = 12, 
                        standalone = TRUE, system_fonts = list(), 
                        user_fonts = list()) {
  on.exit(graphics_exit(grDevices::dev.cur()))
  svg_px(file = file, width = width, height = height, res = res, bg = bg, 
         pointsize = pointsize, standalone = standalone, 
         system_fonts = system_fonts, user_fonts = user_fonts)
  graphics::plot(x)
  invisible(NULL)
}

graphics_exit <- function(old_dev) {
  utils::capture.output({
    grDevices::dev.off()
    if (old_dev > 1) grDevices::dev.set(old_dev)
  })
}
