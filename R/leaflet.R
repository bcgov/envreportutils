#' Add a 'Zoom to BC button' to a leaflet map
#'
#' @param map a Leaflet map object
#' @param ... Other arguments passed on to `leaflet::easyButton()`, such 
#' as `position` (default is `"topleft"`)
#'
#' @return a Leaflet map object
#' @export
add_bc_home_button <- function(map, ...) {
  if (!requireNamespace("leaflet", quietly = TRUE) ||
      !requireNamespace("htmltools", quietly = TRUE)) {
    stop("The leaflet and htmltools packages are required. Please install them.", 
         call. = FALSE)
  }
  
  map <- leaflet::addEasyButton(map, leaflet::easyButton(
    htmltools::img(src = paste0("data:image/svg+xml;base64,", 
                                bc_svg_64)), 
    onClick = leaflet::JS("function(btn, map) { 
                          map.closePopup();
                          map.setView({lon: -126.5, lat: 54.5}, 5);
                          // Close labels - they stay stuck open on mobile
                          map.eachLayer(function (layer) {
                          if (layer instanceof L.Polygon) {
                          layer.label.close();
                          }
                          });
}"), 
    id = "bc-home", ...)
  )
  
  map$dependencies <- c(
    map$dependencies, 
    list(
      htmltools::htmlDependency(
        name = "bc-home-button",
        version = "1.0",
        src = c(file = "bc-home-button"), 
        stylesheet = "bc-home-button.css", 
        package = "envreportutils"
      ))
  )
  
  map
}

#' Set Leaflet map view to B.C.
#'
#' @param map a Leaflet map object
#' @param zoom Zoom level, default `5`
#'
#' @return a Leaflet map object
#' @export
set_bc_view <- function(map, zoom = 5) {
  leaflet::setView(map, lng = -126.5, lat = 54.5, zoom = zoom)
}

#' Re-centre map to B.C. on popup close
#' 
#' @param map Map. A Leaflet map object
#' @param zoom Numeric. Zoom level, default `5`
#' 
#' @return A Leaflet map object
#' 
#' @export

set_bc_view_on_close <- function(map, zoom = 5) {
  htmlwidgets::onRender(map, jsCode = htmlwidgets::JS(paste0("
    function(el, x) {
      var map = this;
      map.on('popupclose',
        function (e) {
          map.setView({lon: -126.5, lat: 54.5}, ", zoom, ");
        })
    }")))
}

#' Keep popups centred in view port
#' 
#' Reloads popups the first time they're opened to correctly pan popup in
#' display. Based on https://stackoverflow.com/a/38172374, EDIT 2 (example:
#' https://jsfiddle.net/3v7hd2vx/277/)
#' 
#' @param map Map. A Leaflet map object
#' 
#' @return A Leaflet map object
#' 
#' @export
popups_centre <- function(map) {
  htmlwidgets::onRender(map, jsCode = htmlwidgets::JS(paste0("
    function(el, x) {
      var map = this;
      document.querySelector('.leaflet-popup-pane').addEventListener('load', function (event) {
	      var target = event.target,
  		  tagName = target.tagName,
        popup = map._popup;
        //console.log('got load event from ' + tagName);
        if (tagName === 'IMG' && popup) {
  	      popup.update();
        }
      }, true);
    }")))
}


create_popup_caaqs <- function(data, caaqs = "o3", type = "polygon") {
  
  data %>%
    # Define individual elements
    content_popup_caaqs_title(., type) %>%
    content_popup_caaqs_metric(., caaqs) %>%
    content_popup_caaqs_standard(., caaqs) %>%
    mutate(popup_svg = paste0("./station_plots/", p_station_id, "_lineplot.svg"),
           # Create the rows
           popup_row1 = paste0("<div class = 'popup-row'>\n",
                               "  <div class = 'title'>\n", popup_title, "  </div>\n",
                               "</div>\n"),
           popup_row2 = paste0("<div class = 'popup-row'>\n",
                               "  <div class = 'section-metric'>\n", popup_metric, "  </div>\n",
                               "  <div class = 'section-standard' ",
                               "style = 'background-color: ", popup_standard_col, "'>\n",
                               popup_standard, "  </div>\n",
                               "</div>\n"),
           popup_row3 = paste0("<img src = ", popup_svg, ">"),
           
           # Assemble them all together
           popup = pmap_chr(list(popup_row1, popup_row2, popup_row3),
                            ~HTML(paste0(..1, ..2, ..3))))
  
}

content_popup_caaqs_title <- function(data, type) {
  if(type == "polygon") {
    data <- mutate(data, popup_title = paste0("    <h2>Air Zone: ", p_az, "</h2>\n",
                                              "    <h4>Station: ", p_station, "</h4>\n"))
  } else if(type == "markers") {
    data <- mutate(data, popup_title = paste0("    <h2>Station: ", p_station, "</h2>\n",
                                              "    <h4>Air Zone: ", p_az, "</h4>\n"))
  }
  data
}

content_popup_caaqs_metric <- function(data, caaqs) {
  if(caaqs == "o3") {
    m <- "Ozone Metric"
    units <- "ppm"
  } else if (caaqs == "pm2.5_annual") {
    m <- "PM<sub>2.5</sub> Metric (annual)"
    units <- "&mu;g/m&sup3;"
  } else if (caaqs == "pm2.5_24h") {
    m <- "PM<sub>2.5</sub> Metric (24h)"
    units <- "&mu;g/m&sup3;"
  }
  
  data <- mutate(data,
                 popup_metric = if_else(caaqs == "Insufficient Data", 
                                        caaqs, paste(metric_value, units)),
                 popup_metric = paste0("    <h4>", m, "</h4>\n",
                                       "    <h3>", popup_metric, "</h3>\n"),
                 popup_metric = if_else(caaqs == "Insufficient Data",
                                        popup_metric,
                                        paste0(popup_metric, 
                                               "    <span>(", n_years, 
                                               " year average)</span>\n")))
}

content_popup_caaqs_standard <- function(data, caaqs) {
  s <- case_when(caaqs == "o3" ~ "Ozone Air Quality Standard",
                 caaqs == "pm2.5_annual" ~ "PM<sub>2.5</sub> Air Quality Standard (annual)",
                 caaqs == "pm2.5_24h" ~ "PM<sub>2.5</sub> Air Quality Standard (24h)")
  
  data <- mutate(data, 
                 popup_standard = paste0("    <h4>", s, "</h4>\n",
                                         "    <h2>", caaqs, "</h2>\n"),
                 popup_standard_col = case_when(caaqs == "Achieved" ~ "#377EB8",
                                                caaqs == "Not Achieved" ~ "#B8373E",
                                                caaqs == "Insufficient Data" ~ "#CCCCCC",
                                                TRUE ~ as.character(NA)))
}

#' Create a popup row div for leaflet maps
#' 
#' @param Character. Row content
#'
#' @export
popup_create_row <- function(...) {
  paste0("<div class = 'popup-row'>\n  ", ..., "\n</div>\n")
}

#' Combine popup rows for leaflet maps
#' 
#' @param Character. Row content
#'
#' @export
#' @export
popup_combine_rows <- function(data) {

  cols <- names(data)[stringr::str_detect(names(data), "popup_row")]
  if(length(cols) == 1) return(data[, cols])
  
  cols <- as.list(data[, cols])
  data <- dplyr::mutate(data,
                        popup = purrr::pmap(cols, ~htmltools::HTML(paste0(...))))
  data$popup
}
