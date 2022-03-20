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
  list(...) %>%
    unlist() %>%
    na.omit() %>%
    paste0(collapse = "") %>%
    paste0("<div class = 'popup-row'>\n  ", ., "\n</div>\n")
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
  purrr::pmap(cols, ~{
    l <- list(...)[!is.na(list(...))]
    htmltools::HTML(paste0(l, collapse = ""))
  })
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
                        metric_type, metrics, metric_names, 
                        units, plot_loc = "./station_plots/", 
                        airzone = "airzone", n_years = "n_years", 
                        station_name, station_id, value1, value2, 
                        level1, level2 = NULL, 
                        colour1 = "colour", colour2 = "colour2",
                        text_colour1 = "text_colour", 
                        text_colour2 = "text_colour2") {
  
  if("sf" %in% class(data)) data <- as.data.frame(data)

  # How many info boxes?
  if(length(metrics) == 1) data$nboxes <- 2 else data$nboxes <- 3
  if(!is.null(level2)) {
    data <- data %>%
      dplyr::mutate(nboxes = dplyr::if_else(.data[[level1]] != .data[[level2]],
                                            nboxes + 1, nboxes))
  }
    
  if(type == "station") {
    standard_name <- paste(metric_type, "Air Quality Management Actions")
  } else if(type == "airzone") {
    standard_name <- paste(metric_type, "Air Quality Standard") 
  }
  
  if(missing(metric_names) && length(metrics) == 1) metric_names <- metric_type
  
  # Everything calculated rowwise
  data <- dplyr::rowwise(data)

  # Define title
  data <- dplyr::mutate(data, title = popup_caaqs_title(type, 
                                                        .data[[airzone]],
                                                        .data[[station_name]]))

  # Define metrics - Always have the first
  data <- dplyr::mutate(data, info_box_metric1 = popup_caaqs_metric(
    metric_names[1], units, value = .data[[value1]], n_years = .data[[n_years]],
    nboxes = .data[["nboxes"]]))
    
  # Add second if more than one
  if(length(metrics) > 1) {
    data <- dplyr::mutate(data, info_box_metric2 = popup_caaqs_metric(
      metric_names[2], units, value = .data[[value2]], n_years = .data[[n_years]],
      nboxes = .data[["nboxes"]]))
  } else {
    data <- dplyr::mutate(data, info_box_metric2 = NA_character_)
  }


  # Define Standards - always have the first
  # (internally skip the second as needed, returns NA)
  
  if(is.null(level2)) {
    level2 <- "level2"
    data[[level2]] <- NA
  }

  data <- data %>%
    dplyr::mutate(
      info_box_std1 = popup_caaqs_standard(
        standard_name, level = .data[[level1]], level_comp = .data[[level2]],
        colour = .data[[colour1]], text_colour = .data[[text_colour1]], 
        box = 1, nboxes = .data[["nboxes"]]),
      
      info_box_std2 = popup_caaqs_standard(
        standard_name, level = .data[[level2]], level_comp = .data[[level1]], 
        colour = .data[[colour2]], text_colour = .data[[text_colour2]], 
        box = 2, nboxes = .data[["nboxes"]]),
    
      info_box_std3 = dplyr::if_else(
        !is.na(.data[["info_box_std2"]]), 
        paste0("<span style = 'text-align:right; margin-left:450px;'>",
               "*TFEEs = Transboundary Flows and Exceptional Events</span>"),
        NA_character_))
  
  # Create Rows and combine
  data %>%
    dplyr::mutate(
      popup_row_title = popup_create_row(.data$title),
      popup_row_info = popup_create_row(.data$info_box_metric1,
                                        .data$info_box_metric2,
                                        .data$info_box_std1,
                                        .data$info_box_std2,
                                        .data$info_box_std3),
      popup_plots = popup_plot(plot_loc, .data[[station_id]], metrics)) %>%
    tidyr::unnest(popup_plots) %>%
    popup_combine_rows()
}

popup_plot <- function(plot_loc, stn_id, metrics) {
  files <- list.files(
    plot_loc, 
    pattern = paste0(stn_id, "_(", paste0(metrics, collapse = "|"), ").svg")) %>%
    paste0("<img src = '", ., "'>")
  
  data.frame(popup_row_plot1 = popup_create_row(files[1]), 
             popup_row_plot2 = popup_create_row(files[2]))
}


popup_caaqs_title <- function(type, airzone, station_name) {
  if(type == "airzone") {
    title <- paste0("    <h2>Air Zone: ", airzone, "</h2>\n",
                    "    <h4>Station: ", station_name, "</h4>\n")
  } else if(type == "station") {
    title <- paste0("    <h2>Station: ", station_name, "</h2>\n",
                    "    <h4>Air Zone: ", airzone, "</h4>\n")
  }
  
  paste0("<div class = 'title'>\n", title, "  </div>\n")
}

popup_caaqs_metric <- function(metric_name, units, value, n_years, 
                               nboxes = "nboxes") {

  if(is.na(value)) {
    value <- "<h3>Insufficient Data</h3>"
  } else {
    value <- paste0("<h3>", value, " ", units, "</h3\n>",
                   "<span>(", n_years, " year average)</span>\n")
  }
  
  paste0("<div class = 'section-box section-metric boxes", nboxes, "'>\n",
         "  <h4>", metric_name, "</h4>\n",
            value, 
         "</div>\n")
}

popup_caaqs_standard <- function(standard_name, level, level_comp = NULL, 
                                 colour, text_colour, box = 1, nboxes = "nboxes") {

  if(box == 2 && (is.na(level) || level == level_comp)) return(NA_character_)
  
  subtext <- dplyr::case_when(
    is.na(level_comp) || level == level_comp ~ "",
    box == 1 ~ "<span>(Unadjusted for TFEEs<sup>*</sup>)</span>",
    box == 2 ~ "<span>(Adjusted for TFEEs<sup>*</sup>)</span>")
  
  if(nchar(level) > 30) add <- " small-std" else add <- ""
  
  paste0("<div class = '",
         "section-box section-standard boxes", nboxes, add,
         "' style = '",
         "background-color: ", colour, "; ", 
         "color: ", text_colour, "'>\n",
         "    <h4>", standard_name, "</h4>\n",
         "    <h2>", level, "</h2>\n",
         subtext,
         "  </div>\n")
}

next_box <- function(n) {
  stringr:::str_subset(n, "info_box") %>%
    sort() %>%
    .[length(.)] %>%
    paste0("info_box", . + 1)
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
