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
#' Create popup for combination CAAQS indicators
#' 
#' For CAAQs where two metrics used to assess management status (e.g., 
#' PM 2.5, SO2, NO2)
#'
#' @param data Data Frame. CAAQ information
#' @param type Character. Which type of popup? "station" or "airzone"?
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
#' @param text_colour Character. Column name of the text colour for the CAAQS
#'   status box (should correspond to each 'level')
#'   
#' @return Character vector of the HTML code for the popup to be passed to
#'   leaflet
#'
#' @export

popup_caaqs <- function(data, type = "station", 
                        metric_type, metrics, units, 
                        airzone = "airzone", n_years = "n_years", 
                        station_name, station_id, value1, value2, 
                        level1, level2 = NULL, 
                        colour1 = "colour", colour2 = "colour2",
                        text_colour1 = "text_colour", 
                        text_colour2 = "text_colour2") {
  if("sf" %in% class(data)) data <- as.data.frame(data)

  if(!is.null(level2) && level1 != level2) standards <- 2 else standards <- 1
  
  # How many info boxes?
  nboxes <- 2                                                   # One of each
  if(length(metrics) > 1) nboxes <- nboxes + 1                  # Two metrics
  if(standards == 2) nboxes <- nboxes + 1 # Two standards
  
  if(length(metrics) > 1) {
    metric_names <- paste(to_titlecase(metrics), "Metric")
  } else {
    metric_names <- paste(metric_type, " Metric")
  }
    
  if(type == "station") {
    standard_name <- paste(metric_type, "Air Quality Management Actions")
  } else if(type == "airzone") {
    standard_name <- paste(metric_type, "Air Quality Standard") 
  }

  # Define title
  data <- popup_caaqs_title(data, type, airzone, station_name)
  data <- dplyr::mutate(data, popup_row_title = popup_create_row(.data$title))
  
  # Define metrics
  if(length(metrics) > 1) {
    data <- popup_caaqs_metric(data, metric_names[1], units, 
                               nboxes = nboxes, value = value1, n_years)
    data <- popup_caaqs_metric(data, metric_names[2], units, 
                               nboxes = nboxes, value = value2, n_years)
  } else {
    data <- popup_caaqs_metric(data, metric_names[1], units, nboxes, 
                               value = value1, n_years = n_years)
  }
  
  # Define Standards
  # (If Achievement depends on tfees, show both)
  if(standards == 2) {
    data <- popup_caaqs_standard(data, standard_name, level1,
                                 colour1, text_colour1, nboxes = nboxes,
                                 subtext = "Unadjusted for TFEEs")
    data <- popup_caaqs_standard(data, standard_name, level2,
                                 colour2, text_colour2, nboxes = nboxes,
                                 subtext = "Adjusted for TFEEs")
  } else {
    data <- popup_caaqs_standard(
      data, standard_name, level1, colour1, text_colour1, nboxes = nboxes)
  }
  
  # Combine info boxes (tidyverse way to do this?)
  boxes <- as.data.frame(data) %>%
    dplyr::select(dplyr::contains("info_box")) %>% 
    as.list()
  data$popup_row_info <- do.call("popup_create_row", args = boxes)
    
  data %>%
    dplyr::mutate(
      popup_row_plot1 = popup_create_row(
        paste0("<img src = '", paste0("./station_plots/", .data[[station_id]], 
                                      "_", metrics[1], ".svg'"), 
               ">")),
      popup_row_plot2 = popup_create_row(
        paste0("<img src = '", 
               paste0("./station_plots/", .data[[station_id]], 
                      "_", metrics[2], ".svg'"), 
               ">"))) %>%
    popup_combine_rows()
}




popup_caaqs_title <- function(data, type, airzone, station_name) {
  if(type == "airzone") {
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
                               nboxes = 2, value, n_years) {

  class <- paste0("section-box section-metric boxes", nboxes)
  n <- length(stringr::str_subset(names(data), "info_box")) + 1
  
  data %>%
    dplyr::mutate(
      info_box = dplyr::if_else(is.na(.data[[value]]),
                                "Insufficient Data", 
                                paste(.data[[value]], units)),
      info_box = paste0("    <h4>", metric_name, "</h4>\n",
                        "    <h3>", .data$info_box, "</h3>\n"),
      info_box = dplyr::if_else(is.na(.data[[value]]),
                                .data$info_box,
                                paste0(.data$info_box, 
                                       "    <span>(", .data[[n_years]], 
                                       " year average)</span>\n")),
      info_box = paste0("  <div class = '", class, "'>\n", 
                        .data$info_box, "  </div>\n")) %>%
    dplyr::rename_with(.cols = "info_box", ~ paste0(., n))
}

popup_caaqs_standard <- function(data, standard_name, level, colour, 
                                 text_colour, nboxes = 2,
                                 subtext = "") {

  class <- paste0("section-box section-standard boxes", nboxes)
  n <- length(stringr::str_subset(names(data), "info_box")) + 1

  if(subtext != "") subtext <- paste0("    <span>(", subtext, ")</span>")
    
  data %>%
    dplyr::mutate(
      info_box = paste0("    <h4>", standard_name, "</h4>\n",
                          "    <h2>", .data[[level]], "</h2>\n",
                          subtext),
      info_box = paste0("  <div class = '", class, "' style = '",
                          "background-color: ", .data[[colour]], "; ", 
                          "color: ", .data[[text_colour]], "'>\n",
                          .data$info_box, "  </div>\n")) %>%
    dplyr::rename_with(.cols = "info_box", ~ paste0(., n))
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
