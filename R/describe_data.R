#' Describe data
#'
#' Create comprehensive tibble of variable metadata using Hmisc::describe as engine.
#'
#' @param data A data frame.
#'
#' @importFrom Hmisc describe
#' @import dplyr
#' @import tidyr
#' @import forcats
#' @importFrom purrr pluck
#'
#' @return A tibble containing variable metadata.
#' @export
#'
#'
#' @examples
#'
#' describe_data(safetyData::adam_adsl)
#'
describe_data <- function(data){

  stopifnot(is.data.frame(data))

  data_nest <- data %>%
    as.list %>%
    tibble(VAR = names(.),
           x = .) %>%
    mutate(ORDER = row_number()) %>%
    rowwise %>%
    mutate(attributes = list(attributes(x) %>%
                               as_tibble %>%
                               select(any_of(c("label","units","format.sas"))) %>%
                               bind_rows(
                                 tibble(label = character(),
                                        units = character(),
                                        `format.sas` = character())
                               ) %>%
                               unique %>%
                               rename(LABEL = label, UNITS = units, FORMAT = `format.sas`))) %>%
    unnest(cols=attributes, keep_empty = TRUE) %>%
    rowwise %>%
    mutate(
      TYPE = case_when(
        is.character(x) | is.factor(x) | is.logical(x) ~ "CHAR",
        lubridate::is.Date(x) | lubridate::is.POSIXct(x) | lubridate::is.POSIXlt(x) ~ "DT",
        is.numeric(x) ~ "NUM"
      ),
      describe = Hmisc::describe(x, digits = 3) %>% list(),
      counts   = describe$counts %>% list(),
      values   = describe$values %>% list(),
      extremes = describe$extremes %>% list()) %>%
    mutate( n        = counts %>% purrr::pluck('n')                       %>% as.numeric(),
            missing  = counts %>% purrr::pluck('missing',  .default = NA) %>% as.numeric(),
            distinct = counts %>% purrr::pluck('distinct', .default = NA) %>% as.numeric() )%>%
    mutate(spike_hist = list(create_hist(x))) %>%
    select(-x)


  data_nest_describe <- data_nest %>%
    arrange(ORDER) %>%
    mutate( counts_df = tryCatch(
      counts[-c(1:3)] %>%
        enframe(name = 'statistic', value = 'value') %>%
        pivot_wider(values_from = value,
                    names_from = statistic) %>%
        list(),
      error = function(e) NULL %>% list()
    ),

    extremes_df = tryCatch(
      extremes %>%
        enframe(name = 'extreme', value = 'value') %>%
        pivot_wider(values_from = value,
                    names_from = extreme) %>%
        list(),
      error = function(e) NULL %>% list()
    ),

    values_df = tryCatch(
      values %>%
        data.frame() %>%
        arrange(desc(frequency)) %>%
        pivot_wider(values_from = frequency,
                    names_from = value) %>%
        list(),
      error = function(e) NULL %>% list()
    )
    )

  return(data_nest_describe%>%
           select(ORDER, TYPE, VAR, LABEL, FORMAT, UNITS, n, missing, distinct,
                  counts_df, values_df, extremes_df,
                  spike_hist) )
}

