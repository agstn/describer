
#' @importFrom htmltools tags
#' @keywords internal
with_tooltip <- function(value, tooltip) {
  htmltools::tags$abbr(style = "text-decoration: underline; text-decoration-style: dotted; cursor: help",
                       title = tooltip, value)
}

#' @importFrom shiny icon
#' @importFrom htmltools span
#' @keywords internal
type_indicator <- function(value = c("NUM", "CHAR", "DT")) {
  value <- match.arg(value)
  label <- switch(value, NUM = "NUM", CHAR = "CHAR", DT = "DT")
  # Add img role and tooltip/label for accessibility
  args <- list(role = "img", title = label)
  if (value == "NUM") {
    args <- c(args, list(shiny::icon("list-ol", "fa-2x"), style = "color: #7f7f7f; font-weight: 500"))
  } else if (value == "CHAR") {
    args <- c(args, list(shiny::icon("font",    "fa-2x"), style = "color: #7f7f7f; font-weight: 500"))
  } else if (value == "DT") {
    args <- c(args, list(shiny::icon("calendar","fa-2x"), style = "color: #7f7f7f; font-weight: 500"))
  } else {
    args <- c(args, list(shiny::icon("circle"),           style = "color: #7f7f7f; font-weight: 500"))
  }
  do.call(htmltools::span, args)
}



#' Create interactive table using Hmisc::describe + reactable
#'
#' @param data Output of `describe_data()`
#' @param data_shared [Optional] `data` converted to a `SharedData` object using crosstalk, for use with linked widgets.
#' @param elementId Unique element ID for the table
#'
#' @import reactable
#' @importFrom reactablefmtr fivethirtyeight
#' @importFrom purrr map
#' @import htmltools
#'
#' @return Reactable display
#' @export
#'
#' @examples
#'
#' adsl <- safetyData::adam_adsl
#' adsl_describe <- describe_data(adsl)
#' describer(adsl_describe)
#'
describer <- function(data, data_shared = NULL, elementId = NULL){

  stopifnot(is.data.frame(data))

  if (!is.null(data_shared)){
    stopifnot(is.SharedData(data_shared))
  }

  if (is.null(data_shared)){
    data_shared <- data
  }

  if (is.null(elementId)){
    elementId <- "describer-table-1"
  }

  tagList(
    div(
      style = "display: grid; grid-template-columns: 1fr 2fr; grid-gap: 20px; margin-bottom: 12px",
        div(
          tags$button(
            "Expand/collapse all",
            onclick = paste0("Reactable.toggleAllRowsExpanded('", elementId, "')")
          )
        ),
        div(
          tags$input(
            type = "text",
            placeholder = "Search variable name/label",
            style = "padding: 4px 8px; width: 75%",
            oninput = paste0("Reactable.setSearch('", elementId, "', this.value)")
          )
        )
    ),
    reactable(
      data_shared,
      elementId = elementId,
      searchable = FALSE,
      pagination = FALSE,
      compact = TRUE,
      highlight = TRUE,
      defaultExpanded = FALSE,
      height = 850,
      theme = reactablefmtr::fivethirtyeight(),

      defaultColDef = colDef(vAlign = 'top',
                             sortable = FALSE),

      columnGroups = list(
        colGroup(name = 'Variable',
                 columns = c('ORDER','TYPE','LABEL')),
        colGroup(name = 'Completeness',
                 columns = c('n', 'missing', 'distinct')),
        colGroup(name = 'Interactive Figure',
                 columns = c('spike_hist'))
      ),

      columns = list(

        ORDER = colDef(name = 'No',
                       header = with_tooltip('No', 'Order of Variable in data'),
                       width = 45,
                       sortable = TRUE,
                       align = 'center'),

        LABEL = colDef(
          name = 'Name - Label',
          header = with_tooltip('Name - Label', 'Variable Name (Labels, Formats & Units if present'),
          width = 275,
          sortable = TRUE,
          style = list(borderRight = "1px solid #eee"),
          cell = function(value, index) {
            htmltools::tagList(
              htmltools::div(style = list(fontWeight= 'bold'), data$VAR[index]),
              htmltools::div(style = list(fontSize = 12, color = "#999"), ifelse(!is.na(value), value, '')),
              htmltools::div(style = list(fontSize = 12, color = "#999"), ifelse(!is.na(data$FORMAT[index]), paste("fmt:",data$FORMAT[index]), '')),
              htmltools::div(style = list(fontSize = 12, color = "#999"), ifelse(!is.na(data$UNITS[index]),  paste("units:",data$UNITS[index]), ''))
            )
          }),

        VAR = colDef(show = FALSE),

        FORMAT = colDef(show = FALSE),

        UNITS = colDef(show = FALSE),

        TYPE  = colDef(name = 'TYPE',
                       header = with_tooltip('TYPE', 'Variable Type (Character, Date/Time, Numeric'),
                       html = TRUE,
                       width = 75,
                       sortable = TRUE,
                       align = 'center',
                       cell = function(value) type_indicator(value)),

        n = colDef(name = 'Observed',
                   width = 150,
                   sortable = TRUE,
                   style = list(fontFamily = "monospace", whiteSpace = "pre"),
                   cell = data_bars(data,
                                    max_value = max(data$n),
                                    text_size = 14,
                                    text_position = 'outside-base',
                                    fill_color = '#2780e3',
                                    fill_opacity = 0.5,
                                    background = ifelse(min(data$n)==max(data$n),
                                                        "#2780E380",
                                                        'lightgrey'))
        ),

        missing = colDef(name = '<small>Missing</small>',
                         html = TRUE,
                         width = 75,
                         sortable = TRUE,
                         align = 'right',
                         cell = function(value, index){
                           paste0(value, "<br><small>", round(100*value/(value + data$n[[index]]),0), "%</small>")
                         }),

        distinct = colDef(name = '<small>Distinct</small>',
                          html = TRUE,
                          width = 75,
                          sortable = TRUE,
                          align = 'right'),


        spike_hist = colDef(name = '<small>
                           <span style="color:#000000;">Mean</sub>&#8231;</span> &nbsp;
                           <span style="color:#FF0000;">Q<sub>0.05</sub>&mid;</span>
                           <span style="color:#0000FF;">Q<sub>0.25</sub>&mid;</span>
                           <span style="color:#000000;">Median</sub>&mid;</span>
                           <span style="color:#0000FF;">Q<sub>0.75</sub>&mid;</span>
                           <span style="color:#FF0000;">Q<sub>0.95</sub>&mid;</span>
                           </small>',
                           html = TRUE,
                           footer = '<small>
                           <span style="color:#000000;">Mean</sub>&#8231;</span> &nbsp;
                           <span style="color:#FF0000;">Q<sub>0.05</sub>&mid;</span>
                           <span style="color:#0000FF;">Q<sub>0.25</sub>&mid;</span>
                           <span style="color:#000000;">Median</sub>&mid;</span>
                           <span style="color:#0000FF;">Q<sub>0.75</sub>&mid;</span>
                           <span style="color:#FF0000;">Q<sub>0.95</sub>&mid;</span>
                           </small>',
                           footerStyle = "font-weight: bold; font-size: 12px; text-transform: uppercase;",
                           cell  = function(x){return(htmltools::div(x))},
                           align = 'center',
                           width  = 270),
        counts_df = colDef(show = FALSE),
        values_df = colDef(show = FALSE),
        extremes_df = colDef(show = FALSE)
      ),
      details = function(index){
        cols <- c("counts","values","extremes")

        d <- data
        create_div <- function(data, index, col){
          d_col <- data[[paste0(col, "_df")]][[index]]
          table_fun <- match.fun(paste0("nested_tab_", col))
          if (!is.null(d_col) && nrow(d_col)>0){
            if ((col=="values" & ncol(d_col)<=12) | (col=="counts" & ncol(d_col)>1) | col=="extremes"){
              htmltools::div(style = "padding: 5px 25px 5px 0px;",table_fun(d_col))
            }
          }
        }
        divs <- map(cols, ~create_div(data=d, index = index, col = .x))
        htmltools::div(divs)
      }
    )
  )
}
