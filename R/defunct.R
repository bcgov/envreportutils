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


#' Defunct functions in envreportutils
#' 
#' These functions are no longer available in this package.
#' 
#' All of these functions, except roxygen_template, are available in the bcgovr
#' package available on GitHub here: https://github.com/bcgov/bcgovr
#' 
#'  \itemize{
#'  \item \code{\link{roxygen_template}}: This function is defunct.
#'   \item \code{\link{devex_badge}}: This function is defunct.
#'    \item \code{\link{analysis_skeleton}}: This function is defunct.
#'    \item \code{\link{add_code_of_conduct}}: This function is defunct.
#'    \item \code{\link{add_contributing}}: This function is defunct.
#'     \item \code{\link{add_license}}: This function is defunct.
#'    \item \code{\link{add_license_header}}: This function is defunct.
#'    \item \code{\link{add_readme}}: This function is defunct.
#'    \item \code{\link{add_rproj}}: This function is defunct.
#' }
#' 
#' @name envreportutils-deprecated
NULL

#' @rdname envreportutils-deprecated
roxygen_template <-  function(...) {
  .Defunct("the RStudio shortcut Ctrl+Alt+Shift+R")
}

#' @rdname envreportutils-deprecated
devex_badge <-  function(...) {
  .Defunct("bcgovr::devex_badge")
}

#' @rdname envreportutils-deprecated
analysis_skeleton <-  function(...) {
  .Defunct("bcgovr::analysis_skeleton")
}

#' @rdname envreportutils-deprecated
add_code_of_conduct <-  function(...) {
  .Defunct("bcgovr::add_code_of_conduct")
}

#' @rdname envreportutils-deprecated
add_contributing <-  function(...) {
  .Defunct("bcgovr::add_contributing")
}

#' @rdname envreportutils-deprecated
add_license <-  function(...) {
  .Defunct("bcgovr::add_license")
}

#' @rdname envreportutils-deprecated
add_license_header <-  function(...) {
  .Defunct("bcgovr::add_license_header")
}

#' @rdname envreportutils-deprecated
add_readme <-  function(...) {
  .Defunct("bcgovr::add_readme")
}

#' @rdname envreportutils-deprecated
add_rproj <-  function(...) {
  .Defunct("bcgovr::add_rproj")
}









