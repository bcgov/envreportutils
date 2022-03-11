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
  data <- dplyr::mutate(
    data,
    popup = purrr::pmap(cols, ~htmltools::HTML(paste0(...))))
  data$popup
}

#' Create popup for CAAQS indicators
#'
#' @param data Data Frame. CAAQ information
#' @param type Character. Which type of popup? "station" or "region"?
#' @param metric_type Character Display name of the CAAQ metric in HTML code
#'   (e.g., `"Ozone"`).
#' @param units Character. Metric units in HTML code (e.g., "ppm"")
#' @param airzone Character. Column name of the airzone column.
#' @param n_years Character. Column name of the number of years (e.g., 'n_years')
#' @param station_name Character. Column name of the station name.
#' @param station_id Character. Column name of the station id (can be the same
#'   as station name).
#' @param value Character. Column name of the metric value (e.g.,
#'   'metric_value_ambient').
#' @param level Character. Column name of the CAAQS level (e.g., 'caaqs_ambient').
#' @param colour Character. Column name of the colour for the CAAQS status box
#'   (should correspond to each 'level')
#'
#' @return Character vector of the HTML code for the popup to be passed to
#'   leaflet
#'
#' @export

popup_caaqs <- function(data, type = "station", metric_type, units, 
                        airzon = "airzone", n_years = "n_years",
                        station_name, station_id, value, level, 
                        colour = "colour") {
  
  if("sf" %in% class(data)) data <- as.data.frame(data)
  
  metric_names <- paste0(metric_type, " Metric")
  standard_name <- paste(metric_type, "Air Quality Standard")
  
  # Define individual elements
  data <- popup_caaqs_title(data, type, airzone, station_name)
  data <- popup_caaqs_metric(data, metric_names, units, value, n_years)
  data <- popup_caaqs_standard(data, standard_name, level, colour)
  
  data <- dplyr::mutate(
    data, 
    popup_row1 = popup_create_row(.data$title),
    popup_row2 = popup_create_row(.data$info_metric, .data$info_standard),
    popup_row3 = paste0("<img src = '", 
                        paste0("./station_plots/", .data$p_station_id, 
                               ".svg'"), 
                        ">"))
  popup_combine_rows(data)
}


#' Create popup for combination CAAQS indicators
#' 
#' For CAAQs where two metrics used to assess management status (e.g., 
#' PM 2.5, SO2, NO2)
#'
#' @param data Data Frame. CAAQ information
#' @param type Character. Which type of popup? "station" or "region"?
#' @param metric_type Character Display name of the CAAQ metric in HTML code
#'   (e.g., `"PM<sub>2.5</sub>"`).
#' @param metrics Character vector. Metrics to combine (e.g., c("annual",
#'   "24hr"))
#' @param units Character. Metric units in HTML code (e.g., "ppm" or
#'   "&mu;g/m&sup3;")
#' @param airzone Character. Column name of the airzone column.
#' @param n_years Character. Column name of the number of years (e.g., 'n_years')
#' @param station_name Character. Column name of the station name.
#' @param station_id Character. Column name of the station id (can be the same
#'   as station name).
#' @param value1 Character. Column name of the first metric value (e.g.,
#'   'metric_value_ambient_annual').
#' @param value2 Character. Column name of the second metric value (e.g.,
#'   'metric_value_ambient_24h').
#' @param level Character. Column name of the CAAQS level (e.g., 'caaqs_ambient').
#' @param colour Character. Column name of the colour for hte CAAQS status box
#'   (should correspond to each 'level')
#'
#' @return Character vector of the HTML code for the popup to be passed to
#'   leaflet
#'
#' @export

popup_caaqs_combo <- function(data, type = "station", 
                              metric_type, metrics, units, 
                              airzone = "airzone", n_years = "n_years", 
                              station_name, station_id, value1, value2, 
                              level, colour = "colour") {
  if("sf" %in% class(data)) data <- as.data.frame(data)
  
  metric_names <- paste0(metric_type, " Metric (", metrics, ")")
  standard_name <- paste(metric_type, "Air Quality Standard")
  
  # Define individual elements
  data <- popup_caaqs_title(data, type, airzone, station_name)
  data <- popup_caaqs_standard(data, standard_name, level, colour, type = "center")
  data <- popup_caaqs_metric(data, metric_names[1], units, 
                             type = "left", value = value1, n_years)
  data <- popup_caaqs_metric(data, metric_names[2], units, 
                             type = "right", value = value2, n_years)
  
  data <- dplyr::mutate(
    data, 
    popup_row1 = popup_create_row(.data$title),
    popup_row2 = popup_create_row(.data$info_standard),
    popup_row3 = popup_create_row(.data$info_metric1,
                                  .data$info_metric2),
    popup_row4 = popup_create_row(paste0("<img src = '", 
                        paste0("./station_plots/", .data[[station_id]], 
                               "_", metrics[1], ".svg'"), 
                        ">")),
    popup_row5 = popup_create_row(paste0("<img src = '", 
                        paste0("./station_plots/", .data[[station_id]], 
                               "_", metrics[2], ".svg'"), 
                        ">")))
  
  popup_combine_rows(data)
}




popup_caaqs_title <- function(data, type, airzone, station_name) {
  if(type == "region") {
    data <- dplyr::mutate(data, title = paste0(
      "    <h2>Air Zone: ", .data[[airzone]], "</h2>\n",
      "    <h4>Station: ", .data[[station_name]], "</h4>\n"))
  } else if(type == "station") {
    data <- dplyr::mutate(data, title = paste0(
      "    <h2>Station: ", .data[[station_name]], "</h2>\n",
      "    <h4>Air Zone: ", .data[[airzone]], "</h4>\n"))
  }
  dplyr::mutate(data, 
                title = paste0(
                  "  <div class = 'title'>\n", .data$title, "  </div>\n"))
}

popup_caaqs_metric <- function(data, metric_name, units, 
                               type = "left", value, n_years) {

  class <- paste0("section-metric section-metric-", type)
  n <- length(stringr::str_subset(names(data), "info_metric")) + 1
  
  data %>%
    dplyr::mutate(
      info_metric = dplyr::if_else(is.na(.data[[value]]),
                                   "Insufficient Data", 
                                   paste(.data[[value]], units)),
      info_metric = paste0("    <h4>", metric_name, "</h4>\n",
                           "    <h3>", .data$info_metric, "</h3>\n"),
      info_metric = dplyr::if_else(is.na(.data[[value]]),
                                   .data$info_metric,
                                   paste0(.data$info_metric, 
                                          "    <span>(", .data[[n_years]], 
                                          " year average)</span>\n")),
      info_metric = paste0("  <div class = '", class, "'>\n", 
                           .data$info_metric, "  </div>\n")) %>%
    dplyr::rename_with(.cols = "info_metric", ~ paste0(., n))
}

popup_caaqs_standard <- function(data, standard_name, level, colour, 
                                 type = "right") {

  class <- paste0("section-standard section-standard-", type)
  
  data %>%
    dplyr::mutate(
      info_standard = paste0("    <h4>", standard_name, "</h4>\n",
                             "    <h2>", .data[[level]], "</h2>\n"),
      info_standard = paste0("  <div class = '", class, "' ",
                             "style = 'background-color: ", 
                             .data[[colour]], "'>\n",
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
