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
#' @param x numerice vector of percentages
#' @param round_90_to_one Should values above 90% be reported to a single 
#' decimal place?
#'
#' @return character vector of formatted percentages following these rounding rules
#' @export
#'
#' @examples
#' percent_round(c(2.5, 11.324, 95.898, 99.6))
#' percent_round(c(2.5, 11.324, 95.898, 99.6), round_90_to_one = TRUE)
percent_round <- function(x, round_90_to_one = FALSE) {
  
  if (length(x) > 1 && range(x) <= 0.1) {
    y <- round(x, 2)
  } else {
    y <- ifelse(x < 10 | x >= 99.5 | round_90_to_one & x > 90, round(x, 1),
                round(x, 0))
  }
  as.character(y)
}
