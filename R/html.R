
#'@title Wrap outputs
#'
#'@description Wrap corresponding chart and table outputs and add toggle functionality
#'
#'@param name output name - string
#'@param chart plotly widget
#'@param table html table (string) - knitr::kable is recommended
#'
#'@return html widget containing the chart, table and toggling functionality
#'
#'@export

wrap_outputs <- function(name, chart, table) {

  # Validate name
  if (typeof(name) != "character" | length(name) != 1) {
    stop("Unexpected input - name should be a single character string")
  } else if (!grepl("^[A-Za-z]+$", substring(name, 1, 1))) { # If the first character of output_name is not a letter
    stop("Invalid html ID - output_name should begin with a letter")
  } else if (!grepl("^[a-zA-Z0-9_.-]*$", name)) { # If output_name contains invalid characters for html id
    stop("invalid html ID - output_name should not include special characters other than underscores, hyphens or stops")
  }

  # Validate chart and table
  if (!any(class(chart) == "htmlwidget")) {
    stop("Unexpected input - chart is not an html widget")
  }

  if (typeof(table) != "character" | length(table) != 1) {
    stop("Unexpected input - table is not a character object")
  }


  # Remove knitr tags from table html if needed
  table <- htmltools::HTML(gsub("```\\{=html\\}|```\n", "", table))

  js <- htmltools::HTML(setup_table_toggle())

  buttons <- htmltools::HTML(insert_table_toggle(name))

  chart_div <- htmltools::HTML(paste0('<div id="', name, '-chart" role="img" aria-label="Chart. Click the show table button to present the data as a text table instead.">'))
  table_div <- htmltools::HTML(paste0('<div id="', name, '-table">'))
  close_div <- htmltools::HTML("</div>")

  widget <- htmlwidgets::prependContent(chart, js, buttons, chart_div)
  widget <- htmlwidgets::appendContent(widget, close_div, table_div, table, close_div)

  return(widget)

}

#' @title Set up table toggle
#'
#' @description Set up JavaScript functions for show table/chart buttons
#'
#' @details Insert JavaScript code for show table/chart buttons. Use with insert_table_toggle().
#'Only include once per document. Assumes the naming convention "name-table" and "name-chart" for divs containing tables and charts.
#'
#' @return JavaScript code (raw html)
#'
#' @export
#'

setup_table_toggle <- function() {

  script <-
    '
  <script>
    function show_table(output_name) {
      $("#show-table-" + output_name).hide();
      $("#show-chart-" + output_name).show();
      $("#" + output_name + "-chart").hide();
      $("#" + output_name + "-table").show();
      $("#show-chart-" + output_name).focus();
    }

    function show_chart(output_name) {
      $("#show-table-" + output_name).show();
      $("#show-chart-" + output_name).hide();
      $("#" + output_name + "-chart").show();
      $("#" + output_name + "-table").hide();
      $("#show-table-" + output_name).focus();
    }
  </script>
'

  return(script)

}


#' @title Insert table toggle buttons
#'
#' @description Add table toggle buttons to rmarkdown site
#'
#' @details Add "show table" and "show chart" buttons to rmarkdown site (html). Expects table contaners to be named "name-table" and chart containers to be named "name-chart"
#'
#' @param output_name the name of assigned to the output. Used to name buttons and reference tables and chart.
#'
#' @return html buttons and CSS
#'
#' @export

insert_table_toggle <- function(output_name) {

  if (length(output_name) > 1) {
    stop("Unexpected input - output name should be a single character string.")
  }

  if (!grepl("^[A-Za-z]+$", substring(output_name, 1, 1))) { # If the first character of output_name is not a letter
    stop("Invalid html ID - output_name should begin with a letter")
  } else if (!grepl("^[a-zA-Z0-9_.-]*$", output_name)) { # If output_name contains invalid characters for html id
    stop("invalid html ID - output_name should not include special characters other than underscores, hyphens or stops")
  }

  table_button_name <- glue::glue("show-table-{output_name}")
  chart_button_name <- glue::glue("show-chart-{output_name}")
  chart_name <- glue::glue("{output_name}-chart")
  table_name <- glue::glue("{output_name}-table")

  toggle_chart_button <- glue::glue(
    '<a role="button" class="toggle-button" id="{chart_button_name}" href="#{output_name}" onclick="show_chart(\'{output_name}\')"> Show chart </a>')

  toggle_table_button <- glue::glue(
    '<a role="button" class="toggle-button" id="{table_button_name}" href="#{output_name}" onclick="show_table(\'{output_name}\')"> Show table </a>')

  # Add style to hide table and show chart button by default
  open <- "{"
  close <- "}"

  style <- glue::glue('<style>
  #{chart_button_name} {open}display: none;{close}
  #{table_name} {open}display: none;{close}
</style>')

  html <- glue::glue('{toggle_chart_button}\n{toggle_table_button}\n{style}')

  return(html)
}

accessible_tabs <- function() {
"<script>
  $('.nav-link').attr('tab-index', '0');
</script>"
}
