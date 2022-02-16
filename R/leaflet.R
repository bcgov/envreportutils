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

#' Create a popup row div for leaflet maps
#' 
#' @param ... Character. Row content
#'
#' @export
popup_create_row <- function(...) {
  paste0("<div class = 'popup-row'>\n  ", ..., "\n</div>\n")
}

#' Combine popup rows for leaflet maps
#' 
#' @param data Data frame including one or more columns: "popup_row1",
#'   "popup_row2", etc.
#'
#' @export

popup_combine_rows <- function(data) {

  cols <- names(data)[stringr::str_detect(names(data), "popup_row")]
  if(length(cols) == 1) return(data[, cols])
  
  cols <- as.list(data[, cols])
  data <- dplyr::mutate(data,
                        popup = purrr::pmap(cols, ~htmltools::HTML(paste0(...))))
  data$popup
}

#' Create popup for CAAQS indicators
#'
#' @param data Data Frame. CAAQ information
#' @param type Character. Which type of popup? "station" or "region"?
#' @param metric_name Character. Display name of the CAAQ metric in HTML code
#'   (e.g., "Ozone Metric", or "PM<sub>2.5</sub> Metric (annual)").
#' @param units Character. Metric units in HTML code (e.g., "ppm" or
#'   "&mu;g/m&sup3;")
#' @param standard_name Character. Display name of the CAAQ standard in HTML
#'   code (e.g., "Ozone Air Quality Standard" or "PM<sub>2.5</sub> Air Quality
#'   Standard (annual)")
#'   
#' @details Data frame must contain the following columns: 'p_az' reflecting the
#'   airzone name, 'p_station_id' reflecting the station id, 'p_station'
#'   reflecting the station name, 'metric_value_ambient' reflecting the CAAQS metric
#'   value, 'caaqs_ambient' reflecting the CAAQS status (not in HTML code), and
#'   'n_years' reflecting the number of years the CAAQS metric is averaged over.
#'
#' @return Character vector of the HTML code for the popup to be passed to
#'   leaflet
#'
#' @export

popup_caaqs <- function(data, type = "station", metric_name, units, standard_name) {
  if("sf" %in% class(data)) data <- as.data.frame(data)
  
  # Define individual elements
  data <- popup_caaqs_title(data, type)
  data <- popup_caaqs_metric(data, metric_name, units)
  data <- popup_caaqs_standard(data, standard_name)
  data <- dplyr::mutate(data, 
                        popup_row1 = popup_create_row(.data$title),
                        popup_row2 = popup_create_row(.data$info_metric, .data$info_standard),
                        popup_row3 = paste0("<img src = ", 
                                            paste0("./station_plots/", .data$p_station_id, 
                                                   ".svg"), 
                                      ">"))
  
  
  popup_combine_rows(data)
}

popup_caaqs_title <- function(data, type) {
  if(type == "region") {
    data <- dplyr::mutate(data, title = paste0("    <h2>Air Zone: ", .data$p_az, "</h2>\n",
                                               "    <h4>Station: ", .data$p_station, "</h4>\n"))
  } else if(type == "station") {
    data <- dplyr::mutate(data, title = paste0("    <h2>Station: ", .data$p_station, "</h2>\n",
                                               "    <h4>Air Zone: ", .data$p_az, "</h4>\n"))
  }
  dplyr::mutate(data, 
                title = paste0("  <div class = 'title'>\n", .data$title, "  </div>\n"))
}

popup_caaqs_metric <- function(data, metric_name, units) {

  dplyr::mutate(data,
                info_metric = dplyr::if_else(.data$caaqs_ambient == "Insufficient Data", 
                                             .data$caaqs_ambient, paste(.data$metric_value_ambient, units)),
                info_metric = paste0("    <h4>", metric_name, "</h4>\n",
                                     "    <h3>", .data$info_metric, "</h3>\n"),
                info_metric = dplyr::if_else(.data$caaqs_ambient == "Insufficient Data",
                                             .data$info_metric,
                                             paste0(.data$info_metric, 
                                                    "    <span>(", .data$n_years, 
                                                    " year average)</span>\n")),
                info_metric = paste0("  <div class = 'section-metric'>\n", 
                                     .data$info_metric, "  </div>\n"))
}

popup_caaqs_standard <- function(data, standard_name) {

  dplyr::mutate(data, 
                info_standard = paste0("    <h4>", standard_name, "</h4>\n",
                                       "    <h2>", .data$caaqs_ambient, "</h2>\n"),
                info_standard_col = dplyr::case_when(.data$caaqs_ambient == "Achieved" ~ "#377EB8",
                                                     .data$caaqs_ambient == "Not Achieved" ~ "#B8373E",
                                                     .data$caaqs_ambient == "Insufficient Data" ~ "#CCCCCC",
                                                     TRUE ~ as.character(NA)),
                info_standard = paste0("  <div class = 'section-standard' ",
                                       "style = 'background-color: ", 
                                       .data$info_standard_col, "'>\n",
                                       .data$info_standard, "  </div>\n"))
}

#' Create copy of CAAQS CSS styles for leaflet map
#'
#' Creates a copy in the local repository of the CAAQS CSS file.
#'
#' @param folder Character. Location of the leaflet maps folder.
#' @param overwrite Logical. Overwrite the CSS file if it already exits?
#'
#' @export

css_caaqs_copy <- function(folder = "./leaflet_map", overwrite = FALSE) {
  loc <- paste0(folder, "/assets/")
  if(!file.exists(loc)) dir.create(loc)
  file.copy(system.file("css", "caaqs-styles.css", package = "envreportutils"),
            loc, overwrite = overwrite)
}
