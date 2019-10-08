# Copyright 2018 Province of British Columbia
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

#' Round percentage values in standard way for reporting
#' 
#' Follows the advice from: http://adc.bmj.com/content/early/2015/04/15/archdischild-2014-307149
#' "Percentages: [Use] integers, or one decimal place for values under 10%. 
#' Values over 90% may need one decimal place if their complement is informative. 
#' Use two or more decimal places only if the range of values is less than 0.1%"
#'
#' @param x Numeric vector of percentages.
#' @param round_90_to_one Should values above 90 percent be reported to a single 
#' decimal place?
#' @param as_char Should the result be returned as a character vector? Default `TRUE`.
#'
#' @return Character vector of formatted percentages following these rounding rules
#' @export
#'
#' @examples
#' report_percent(c(2.5, 11.324, 95.898, 99.6))
#' report_percent(c(2.5, 11.324, 95.898, 99.6), round_90_to_one = TRUE)
report_percent <- function(x, round_90_to_one = FALSE, as_char = TRUE) {
  
  if (length(x) > 1 && diff(range(x)) <= 0.1) {
    y <- round(x, 2)
  } else {
    y <- ifelse(x < 10 | x >= 99.5 | round_90_to_one & x > 90, round(x, 1),
                round(x, 0))
  }
  
  if (as_char) {
    return(as.character(y))
  }
  
  y
  
}

#' Convert a word or phrase to Title Case
#'
#' @param x character string
#'
#' @return A character string: x in Title Case
#' @export
#'
#' @examples
#' x <- "hello world"
#' to_titlecase(x)
#' 
#' x <- "HELLO WORLD"
#' to_titlecase(x)
#' 
#' x <- "heLLo WoRLd"
#' to_titlecase(x)
to_titlecase <- function(x) {
  if (!is.character(x)) stop("x must be a character string", call. = FALSE)
  tools::toTitleCase(tolower(x))  
}

#' Specify a path to a folder or file in the SOE folder on the network drive
#'
#' @param x character. Path to a file or folder, relative to `pickaxe/SOE`
#'
#' @return full path to the file or folder
#' @export
#'
#' @examples
#' \dontrun{
#' soe_path("Operations ORCS/indicators/air")
#' # In a function call:
#' read.csv(soe_path("Operations ORCS/data/foo.csv"))
#' }
soe_path <- function(x) {
  if (!is.character(x)) 
  stop("x must be a character string denoting a 
path relative to the SOE root folder (e.g. Operations ORCS/indicators/air/.")
  root <- get_soe_root()
  file.path(root, gsub("^/", "", x))
}

#' Set the path to the root of your SoE network folder
#' 
#' This will add an environment variable called `ENVREPORTUTILS_SOE_PATH` to 
#' your `.Renviron` file and set it to the path on your computer to the 
#' `pickaxe/SOE` folder.
#'
#' @param x character. A path to the SOE folder on the network (pickaxe) drive.
#' For Mac users it will look something like `"/Users/username/Volumes/pickaxe/SOE"`. 
#' For Windows users, use the mapped drive letter rather than the UNC path 
#' (e.g., `"P:/pickaxe/SOE"`)
#'
#' @return logical (invisibly)
#' @export
#'
#' @examples
#' \dontrun{
#' # Mac
#' set_soe_root("/Users/username/Volumes/pickaxe/SOE")
#' # Windows
#' set_soe_root("P:/pickaxe/SOE")
#' }
set_soe_root <- function(x) {
  if (.Platform$OS.type == "windows" && grepl("\\\\", x)) {
    stop("It looks like you are trying to set your SOE path as a UNC path
(path beginning with \\. Please use the mapped drive letter (E.g., P:/etc).", 
         call. = FALSE)
  }
  home_dir <- Sys.getenv("HOME")
  renviron_file <- file.path(home_dir, ".Renviron")
  renviron_lines <- readLines(renviron_file)
  soe_path_env <- grepl(paste0("^", soe_path_envvar_name()), renviron_lines)
  if (any(soe_path_env)) {
    overwrite <- utils::askYesNo("Your soe path has already been set. Overwrite?")
    if (!overwrite) return(invisible(FALSE))
    renviron_lines <- renviron_lines[!soe_path_env]
  }
  renviron_lines <- c(renviron_lines, "", "## envreportutils soe path", 
                      paste0(soe_path_envvar_name(), "=", x), "")

  writeLines(renviron_lines, con = renviron_file)
  message("Restart R for changes to take effect")
  invisible(TRUE)
}

get_soe_root <- function() {
  soe_root <- Sys.getenv(soe_path_envvar_name())
  if (!nzchar(soe_root)) {
    stop("You need to set your soe root path. Use set_soe_root()", call. = FALSE)
  }
  soe_root
}

soe_path_envvar_name <- function() "ENVREPORTUTILS_SOE_PATH"
