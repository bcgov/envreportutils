library(sf)
library(bcmaps)
library(rmapshaper)
library(svglite)
library(base64enc)

bc_simp <- bc_bound() %>% 
  st_geometry() %>% 
  ms_simplify(0.02, explode = TRUE, keep_shapes = FALSE)

bc_svg <- stringSVG(plot(bc_simp, col = "black"), 
                    height = 0.5, width = 0.5, 
                    pointsize = 0.5, standalone = TRUE)

bc_svg_64 <- base64encode(charToRaw(bc_svg))

use_data(bc_svg_64, internal = TRUE)
