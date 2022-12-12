#'@title Plot frequency graph
#'
#'@description Produce bar chart (plotly) for single factor frequency data.
#'
#'@param data Frequency data (data frame). Expected input: data.frame(categories = c(), frequencies = c())
#'@param n sample size (optional)
#'@param bar_colour Colour name. Defaults to blue (see @get_gradient())
#'@param break_q_names_col applies break_q_names to the column. Not applied by default
#'@param max_lines maximum number of lines. Int, defaults to 2/ See carsurvey::break_q_names()
#'@param xlab X axis title (optional)
#'@param ylab Y axis title (optional)
#'@param font_size minimum font size for the plot (numeric).
#'@param orientation plot orientation ("h" = horizontal, "v" = verical). Vertical by default
#'@param ... additional plotly_ly arguments
#'
#'@return bar chart
#'
#'@export

plot_freqs <- function(data, n, bar_colour, break_q_names_col, max_lines = 2,  xlab = "", ylab = "", font_size = 12, orientation = c("v", "h"), ...) {

  # Set default bar colour
  if (missing(bar_colour)) {
    c <- get_gradient(1)
  } else if (!is.character(bar_colour) | length(bar_colour) != 1) {
    stop("Unexpected input - bar_colour should be a single colour name.")
  }

  # Validate data
  if (!is.data.frame(data)) {
    stop("Unexpected input - data is not a data.frame.")
  } else if (ncol(data) != 2) {
    stop("Unexpected input - data does not contain two columns.")
  } else if (!is.numeric(data[[2]])) {
    stop("Unexpected input - data column 2 is not numeric.")
  }

  # Validate labels
  if (!is.character(xlab) | !is.character(ylab) | length(xlab) > 1 | length(ylab) > 1) {
    stop("Unexpected input - labels should be single character strings.")
  }

  # Validate font size
  if (!is.numeric(font_size)) {
    stop("Unexpected input - font_size is not numeric.")
  }

  orientation <- match.arg(orientation)

  # Apply break_q_names to a column
  if(!missing(break_q_names_col)) {
    # Coerce to character type
    data[[break_q_names_col]] <- as.character(data[[break_q_names_col]])

    data[[break_q_names_col]] <- break_q_names(data[[break_q_names_col]], max_lines = max_lines)

    data[[break_q_names_col]] <- factor(data[[break_q_names_col]], levels = data[[break_q_names_col]])
  }

  x_axis <- list(
    title = xlab,
    tickfont = list(size = font_size),
    titlefont = list(size = font_size * 1.2)
  )

  y_axis <- list(
    title = ylab,
    tickfont = list(size = font_size),
    titlefont = list(size = font_size * 1.2)
  )

  if (orientation == "v") {
    data[[1]] <- factor(data[[1]], levels = unique(data[[1]]))
    x_vals <- data[[1]]
    y_vals <- data[[2]]
  } else if (orientation == "h") {
    data[[1]] <- factor(data[[1]], levels = rev(unique(data[[1]])))
    x_vals <- data[[2]]
    y_vals <- data[[1]]
  }

  y_axis$title <- "" # Y axis title is created as a caption instead

  fig <- plotly::plot_ly(
    x = x_vals,
    y = y_vals,
   # marker = list(color = bar_colour),
    type = "bar",
    orientation = orientation,
    ...
  )

  sample <- ifelse(!missing(n), paste0("Sample size = ", n), "")

  fig <- plotly::config(fig, displayModeBar = F)
  fig <- plotly::layout(fig,
                        xaxis = x_axis,
                        yaxis = y_axis,
                        margin = list(b = 100),
                        hoverlabel = list(bgcolor = "white", font = list(size = font_size)),
                        annotations = list(x = 1, y = 0, text = sample,
                                           showarrow = F, xanchor='right', yanchor='auto', xshift=0, yshift=-100,
                                           xref='paper', yref='paper', font=list(size = font_size))
  )

  fig <- plotly::layout(fig, annotations = create_y_lab(ylab, font_size))

  return(fig)

}

#'@title Plot stacked bar graph
#'
#'@description Produce stacked bar chart (plotly).
#'
#'@param data Frequency data for stacked bar chart (data frame). 3 columns: variable 1, variable 2 and values (tidy data)
#'@param n sample size
#'@param break_q_names_col applies break_q_names to the column. Not applied by default
#'@param max_lines maximum number of lines. Int, defaults to 2/ See carsurvey::break_q_names()
#'@param xlab X axis title
#'@param ylab Y axis title
#'@param colour_scale type of colour scale ("gradient", "scale" or "2gradients"). See get_gradient(), get_2colour_scale() and get_2colour_gradients().
#'@param font_size minimum font size for the plot (numeric).
#'@param neutral_mid whether the midpoint of the colour scale should be neutral ("2gradients" scale only). TRUE by default
#'@param orientation plot orientation ("h" = horizontal, "v" = verical). Vertical by default
#'@param ... additional plotly_ly arguments
#'
#'@return bar chart
#'
#'@export

plot_stacked <- function(data, n, break_q_names_col, max_lines = 2, xlab = "", ylab = "", colour_scale = c("2gradients", "gradient", "scale", "3scale"), font_size = 12, neutral_mid = TRUE, orientation = c("h", "v"), ...) {

  # Validate data
  if (!is.data.frame(data)) {
    stop("Unexpected input - data is not a data.frame.")
  } else if (ncol(data) != 3) {
    stop("Unexpected input - data should have three columns")
  }

  # Validate labels
  if (!is.character(xlab) | !is.character(ylab) | length(xlab) > 1 | length(ylab) > 1) {
    stop("Unexpected input - labels should be single character strings.")
  }

  # Validate font size
  if (!is.numeric(font_size)) {
    stop("Unexpected input - font_size is not numeric.")
  }

  # Apply break_q_names to a column
  if(!missing(break_q_names_col)) {
    # Coerce to character type
    data[[break_q_names_col]] <- as.character(data[[break_q_names_col]])

    data[[break_q_names_col]] <- break_q_names(data[[break_q_names_col]], max_lines = max_lines)

    data[[break_q_names_col]] <- factor(data[[break_q_names_col]], levels = data[[break_q_names_col]])
  }

  colour_scale <- match.arg(colour_scale)

  orientation <- match.arg(orientation)

  x <- list(
    title = xlab,
    tickfont = list(size = font_size),
    titlefont = list(size = font_size * 1.2)
  )

  y <- list(
    title = "",
    tickfont = list(size = font_size),
    titlefont = list(size = font_size * 1.2)
  )


  # Get bar colours
  ncolours <- length(unique(data[[2]]))
  if (colour_scale == "gradient") {
    colours <- get_gradient(ncolours)
  } else if (colour_scale == "scale") {
    colours <- get_2colour_scale(ncolours)
  } else if (colour_scale == "2gradients") {
    mid <- ceiling(ncolours/2)
    colours <- get_2colour_gradients(ncolours, mid = mid, neutral_mid = neutral_mid)
  } else if (colour_scale == "3scale") {
    colours <- get_3colour_scale(ncolours)
  }

  colours <- rep(colours, length(unique(data[[1]])))

  hovertext <- paste0(data[[2]], ": ", data[[3]], " <extra></extra>")

  if (orientation == "v") {
    data[[1]] <- factor(data[[1]], levels = unique(data[[1]]))
    x_vals <- data[[1]]
    y_vals <- data[[3]]
  } else if (orientation == "h") {
    data[[1]] <- factor(data[[1]], levels = rev(unique(data[[1]])))
    x_vals <- data[[3]]
    y_vals <- data[[1]]
  }


  fig <- plotly::plot_ly(data,
                         y = y_vals,
                         x = x_vals,
                         type = "bar",
                         orientation = orientation,
                         hovertemplate = hovertext,
                         marker = list(color = colours),
                         ...)

  fig <- plotly::config(fig, displayModeBar = F)

  sample <- ifelse(!missing(n), paste0("Sample size = ", n), "")

  fig <- plotly::layout(fig,
                        barmode = "stack",
                        clickmode = "none",
                        legend = list(orientation = orientation,   # show entries horizontally
                                      xanchor = "center",  # use center of legend as anchor
                                      yanchor = "bottom",
                                      x = 0.5,
                                      y = 1,
                                      traceorder = "normal",
                                      font = list(size = font_size)),
                        margin = list(b = 100),
                        xaxis = x,
                        yaxis = y,
                        hoverlabel = list(bgcolor = "white", font = list(size = font_size)),
                        annotations = list(x = 1, y = 0, text = sample,
                                           showarrow = F, xanchor='right', yanchor='auto', xshift=0, yshift=-100,
                                           xref='paper', yref='paper', font=list(size = font_size)
                        )
  )

  fig <- plotly::layout(fig, annotations = create_y_lab(ylab, font_size))

  return(fig)

}


#'@title Plot grouped frequency graph
#'
#'@description Produce bar chart (plotly) for frequency data with grouping variable.
#'
#'@param data Frequency data (data frame). 3 columns - cateogry names, groups and frequencies.
#'@param n sample size
#'@param xlab X axis title
#'@param ylab Y axis title
#'@param font_size minimum font size for the plot (numeric).
#'@param orientation plot orientation ("h" = horizontal, "v" = verical). Vertical by default.
#'@param ... additional plotly_ly arguments
#'
#'@return bar chart
#'
#'@export

plot_grouped <- function(data, n, break_q_names_col, max_lines = 2, xlab = "", ylab = "", font_size = 12, orientation = c("v", "h"), ...) {

  # Set default bar colours
  n_groups <- length(unique(data[[2]]))
  colours <- get_2colour_scale(n_groups)

  colour_list <- as.list(colours)
  names(colour_list) <- unique(data[[2]])

  colours <- dplyr::recode(data[[2]], !!!colour_list)

  # Validate data
  if (!is.data.frame(data)) {
    stop("Unexpected input - data is not a data.frame.")
  } else if (ncol(data) != 3) {
    stop("Unexpected input - data does not contain 3 columns.")
  } else if (!is.numeric(data[[3]])) {
    stop("Unexpected input - data column 3 is not numeric.")
  }

  # Validate labels
  if (!is.character(xlab) | !is.character(ylab) | length(xlab) > 1 | length(ylab) > 1) {
    stop("Unexpected input - labels should be single character strings.")
  }

  # Validate font size
  if (!is.numeric(font_size)) {
    stop("Unexpected input - font_size is not numeric.")
  }

  # Apply break_q_names to a column
  if(!missing(break_q_names_col)) {
    # Coerce to character type
    data[[break_q_names_col]] <- as.character(data[[break_q_names_col]])

    data[[break_q_names_col]] <- break_q_names(data[[break_q_names_col]], max_lines = max_lines)

    data[[break_q_names_col]] <- factor(data[[break_q_names_col]], levels = data[[break_q_names_col]])
  }

  orientation <- match.arg(orientation)

  x <- list(
    title = xlab,
    tickfont = list(size = font_size),
    titlefont = list(size = font_size * 1.2)
  )

  y <- list(
    tickfont = list(size = font_size),
    titlefont = list(size = font_size * 1.2)
  )

  if (orientation == "v") {
    data <- dplyr::arrange(data, data[,1])
    data[,1] <- factor(data[,1], levels = data[,1])
    x_vals <- data[[1]]
    y_vals <- data[[3]]
  } else if (orientation == "h") {
    data <- dplyr::arrange(data, dplyr::desc(data[,1]))
    data[,1] <- factor(data[,1], levels = data[,1])
    x_vals <- data[[3]]
    y_vals <- data[[1]]
  }

  fig <- plotly::plot_ly(
    x = x_vals,
    y = y_vals,
    color = data[[2]],
    marker = list(color = c("#004556","#004556", "#FF6900", "#FF6900")),
    type = "bar",
    ...
  )

  sample <- ifelse(!missing(n), paste0("Sample size = ", n), "")

  fig <- plotly::config(fig, displayModeBar = F)
  fig <- plotly::layout(fig,
                        xaxis = x,
                        yaxis = y,
                        margin = list(b = 100),
                        hoverlabel = list(bgcolor = "white", font = list(size = font_size)),
                        annotations = list(x = 1, y = 0, text = sample,
                                           showarrow = F, xanchor='right', yanchor='auto', xshift=0, yshift=-100,
                                           xref='paper', yref='paper', font=list(size = font_size))
  )

  fig <- plotly::layout(fig, annotations = create_y_lab(ylab, font_size))

  return(fig)

}


#'@title Plot likert graph
#'
#'@description Produce likert stacked bar chart (plotly). At least 2 questions per plot.
#'
#'@param data Frequency data for likert quesitons (data frame). 3 columns: question, answer option, frequency.
#'@param mid the mid-point of the scale. should be higher than 2 and lower than the number of answers.
#'@param xlab X axis title
#'@param ylab Y axis title
#'@param n sample size
#'@param max_lines changes maximum lines text can go over
#'@param font_size minimum font size for the plot (numeric).
#'@param neutral_mid whether the middle of the scale should be a neutral category (logical). TRUE by default
#'@param break_q_names_col applies break_q_names to the column. Not applied by default
#'@param ... additional plot_ly arguments
#'
#'@return bar chart
#'
#'@export

plot_likert <- function(data, mid, n, break_q_names_col, max_lines = 2, xlab = "", ylab = "", font_size = 12, neutral_mid = TRUE, ...) {

  # Validate data
  if (!is.data.frame(data)) {
    stop("Unexpected input - data is not a data.frame.")
  } else if (ncol(data) != 3) {
    stop("Unexpected input - data should have at three columns.")
  }

  # Validate labels
  if (!is.character(xlab) | !is.character(ylab) | length(xlab) > 1 | length(ylab) > 1) {
    stop("Unexpected input - labels should be single character strings.")
  }

  # Validate font size
  if (!is.numeric(font_size)) {
    stop("Unexpected input - font_size is not numeric.")
  }

  n_questions <- length(unique(data[[1]]))
  n_answers <- length(unique(data[[2]]))

  # Validate mid
  if (!is.numeric(mid)) {
    stop("Unexpected input - mid is not numeric.")
  } else if (mid < 2) {
    stop("Unexpected inout - mid is smaller than 2.")
  } else if (neutral_mid & mid > n_answers) {
    stop("Unexpected input - mid >= the number of answers.")
  }

  # Validate neutral mid
  if (!is.logical(neutral_mid)) {
    stop("Unexpected input - mid is not logical (TRUE/FALSE)")
  }

  # Apply break_q_names to a column
  if(!missing(break_q_names_col)) {
    data[[break_q_names_col]] <- break_q_names(data[[break_q_names_col]], max_lines)
    data[[break_q_names_col]] <- factor(data[[break_q_names_col]], levels = unique(data[[break_q_names_col]]))
  }

  x <- list(
    title = xlab,
    tickfont = list(size = font_size),
    titlefont = list(size = font_size * 1.2),
    range = list(-1, 1),
    tickformat = ".0%", title = "Percent"
  )

  y <- list(
    title = "",
    tickfont = list(size = font_size),
    titlefont = list(size = font_size * 1.2)
  )

  # Reorder data
  data[[1]] <- factor(data[[1]], levels = rev(unique(data[[1]])))

  data[[2]] <- factor(data[[2]], levels = unique(data[[2]]))

  # Calculate bases for bars
  bases <- calculate_bases(data, mid, neutral_mid)

  # Get bar colours
  if (neutral_mid) {
    colours <- get_2colour_gradients(n_answers, mid = mid, neutral_mid = neutral_mid)
  } else {
    colours <- get_2colour_gradients(n_answers, mid = mid-1, neutral_mid = neutral_mid)
  }

  colours <- unlist(lapply(colours, function(x) rep(x, n_questions)))

  hovertext <- paste0(data[[2]], ": ", round(abs(data[[3]]) * 100, 1), "%", " <extra></extra>")

  fig <- plotly::plot_ly(y = data[[1]],
                         x=data[[3]],
                         type="bar",
                         color = data[[2]],
                         orientation = "h",
                         base = bases,
                         hovertemplate = hovertext,
                         marker = list(color = colours),
                         ...)

  fig <- plotly::config(fig, displayModeBar = F)

  fig <- plotly::layout(fig,
                        barmode = "stack",
                        clickmode = "none",
                        margin = list(b = 100),
                        annotations = list(x = 1, y = 0, text = paste0("Sample size = ", n),
                                           showarrow = F, xanchor='right', yanchor='auto', xshift=0, yshift=-100,
                                           xref='paper', yref='paper', font=list(size = font_size)),
                        xaxis = x,
                        yaxis = y,
                        hoverlabel = list(bgcolor = "white", font = list(size = font_size)))

  fig <- plotly::layout(fig, annotations = create_y_lab(ylab, font_size))

  fig <- plotly::layout(fig, legend = list(xanchor = "left",
                                           yanchor = "bottom",
                                           orientation = "h",
                                           y = 1,
                                           traceorder = "normal",
                                           font = list(size = font_size))
  )

  # Disable interactive legend

  id <- paste0("plot", stringi::stri_rand_strings(1, 10))
  javascript <- paste0(id, ".on('plotly_legenddoubleclick', function(d, i) {return false});",
                       id, ".on('plotly_legendclick', function(d, i) {return false});")

  fig$elementId <- id
  fig <- htmlwidgets::prependContent(fig, htmlwidgets::onStaticRenderComplete(javascript), data=list(''))

  return(fig)

}

#'@title Calculate bases for Likert chart
#'
#'@description Calculates starting locations for bars on the Likert chart.
#'
#'@param data see @plot_likert
#'@param mid see @plot_likert
#'@param neutral_mid see @plot_likert
#'
#'@return bar starting locations

calculate_bases <- function(data, mid, neutral_mid) {
  # Bases are needed as a vector for plotly
  data[[2]] <- factor(data[[2]], unique(data[[2]]))

  # Remove the values corresponding to the last response option so the base for the final response option
  # is the cumulative sum up to and including the previous response option.
  filtered_data <- data[!data[[2]] %in%
                 levels(data[[2]])[length(levels(data[[2]]))], ]

  n_questions <- length(unique(data[[1]]))

  cumulative_sums <- filtered_data %>%
    dplyr::group_by_at(1) %>%
    dplyr::mutate_at(3, cumsum)

  # Add bases of 0 for the first response option, for each question
  n_questions <- length(unique(data[[1]]))

  bases <- split(cumulative_sums[[3]], ceiling(seq_along(cumulative_sums[[3]])/n_questions)) %>%
    lapply(function(x) c(0, x)) %>% unlist %>% unname

  get_neg_bases <- function(x, mid, neutral_mid) {
    if (neutral_mid) {
      sum(x[c(1:(mid-1))]) + x[mid]/2
    } else {
      sum(x[c(1:(mid-1))])
    }
  }

  negative_bases <- data %>% dplyr::group_by_at(1) %>% dplyr::mutate_at(3, get_neg_bases, mid = mid, neutral_mid = neutral_mid) %>% data.frame

  bases <- bases - negative_bases[[3]]

  return(bases)
}



#'@title Create custom Y axis label
#'
#'@description Create a custom y axis label (plotly annotation). This label is placed just above the y axis
#' and is horizontal, to replace the vertically flipped label produced by default.
#'
#'@param ylab Y axis label
#'@param font_size font size used in the chart. This function will return a slightly larger font.
#'
#'@return list of parameters for plotly annotation
#'
#'@export

create_y_lab <- function(ylab, font_size) {
  annotation <- list(text = ylab, # Custom Y axis label
                     y = 1,
                     x = "min",
                     showarrow = FALSE,
                     yshift = 30,
                     xref = "paper",
                     yref = "paper",
                     font = list(size = font_size * 1.2)
  )

  return(annotation)

}

