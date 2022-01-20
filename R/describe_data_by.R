#' Describe data by a grouping variable
#'
#' Create comprehensive tibble of variable metadata using Hmisc::describe as engine.
#'
#' @param data A data frame.
#' @param by Name of by variable.
#'
#' @importFrom Hmisc describe
#' @import dplyr
#'
#' @return A tibble containing variable metadata with 1 row per group.
#' @export
#'
#'
#' @examples
#'
#' describe_data_by(safetyData::adam_adsl, by = ARM)
#'
describe_data_by <- function(data, by){

  data %>%
    nest_by({{by}}) %>%
    mutate(describe_data_result = list(describe_data(data))) %>%
    select(-data) %>%
    unnest(cols = describe_data_result)

}
