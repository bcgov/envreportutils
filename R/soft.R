# Copyright 2017 Province of British Columbia
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

#' Post a file to SOFT for sharing
#'
#' Files can be shared from within the government network, from a government user to the public, or
#' from the public to a government user. Public-to-public sharing is not supported. A user can specify the
#' number of days the file will be available, and whether or not to restrict access to the file to
#' within the governmnet network.
#' 
#' The SOFT web form and more information can be found [here](http://www.env.gov.bc.ca/csd/imb/soft/).
#'
#' @param file Path to the file or directory on your computer. If `file` is a 
#' directory or a character vector of filenames, the files in the directory will 
#' be zipped before they are uploaded.
#' @param email Optional email address to which to send the link to the file.
#' @param email_subj Optional subject of the email.
#' @param days The number of days the file should be available. 
#' Default 7, maximum 14.
#' @param internal Should the link be only available with the B.C. Government 
#' network? Default `TRUE`
#' @param progress Should a progress bar be displayed? Default \code{TRUE}.
#' @param zip Should the file be zipped before sending? Default \code{FALSE}, 
#' however it \code{file} is a directory or multiple files they will be zipped 
#' anyway.
#' @param zipname an option name to call the zipfile. If left \code{NULL} (default), 
#' a name will be generated.
#'
#' @importFrom httr POST stop_for_status content upload_file
#' @importFrom xml2 xml_find_all xml_text
#'
#' @return A url of the link from which the file can be downloaded
#' @export
#' @md
soft <- function(file, email = NULL, email_subj = NULL, internal = TRUE, days = 7, 
                 progress = TRUE, zip = FALSE, zipname = NULL) {
  
  if (!all(file.exists(file))) stop("files do not exist")
  if (!is.logical(internal)) stop("'internal' must be TRUE or FALSE")
  if (!is.numeric(days)) stop("'days' must be a number")
  
  ## Zip it up if it is a directory
  if (file.info(file)$isdir || length(file) > 1 || zip) {
    message("Zipping files in directory: ", file)
    zipfile <- zip_it(file, zipname)
    file <- zipfile
    on.exit(unlink(zipfile, recursive = TRUE))
  }
  
  if (file.size(file) > 2e9) stop("File size must be less than 2GB")
  
  if (!is.null(email)) {
    if (length(email) > 1) stop("only one email address can be entered")
    isvalid <- grepl("\\<[A-Z0-9._%+-]+@[A-Z0-9.-]+\\.[A-Z]{2,}\\>", email, ignore.case = TRUE)
    if (!isvalid) stop("You have entered an invalid email address")
  }
  
  params <- list("_soft_mail_sendto" = email,
                 "_soft_mail_subject" = email_subj,
                 "_soft_force_gov" = as.integer(internal),
                 "_soft_submit" = "submit",
                 "_soft_keep_days" = as.integer(days),
                 "_soft_filename" = httr::upload_file(file))
  
  if (progress) {
    prog <- httr::progress(type = "up")
  } else {
    prog <- list()
  }
  
  message("Uploading file...\n")
  res <- httr::POST("http://www.env.gov.bc.ca/perl/soft/ul.pl",
                    config = prog,
                    body = params)
  
  httr::stop_for_status(res)
  
  content <- httr::content(res)
  
  ret <- url_from_content(content)
  
  message("\nSuccess! Your file is available at the following link for ",
          days, " days:\n", ret, "\n")
  
  if (!is.null(email)) {
    message("It has been sent to ", email, "\n")
  }
  
  if (internal) {
    message("It is only available inside the government network.\n")
  }
  
  ret
  
}

url_from_content <- function(x) {
  xml_url <- xml2::xml_find_all(x, ".//p[contains(., 'soft/dl.pl')]")
  url <- xml2::xml_text(xml_url)
  url
}

zip_it <- function(dir, zipname) {
  if (is.null(zipname)) {
    zipname <- paste0("soft_send_", Sys.Date(), ".zip")
  } else if (!grepl("\\.zip$", zipname)) {
    zipname <- paste0(zipname, ".zip")
  }
  
  zipdir <- normalizePath(tempdir(), winslash = "/")
  zipfile <- file.path(zipdir, zipname, fsep = "/")
  utils::zip(zipfile = zipfile, files = dir, 
             flags = paste0(formals(utils::zip)$flags, " -q"))
  zipfile
}

