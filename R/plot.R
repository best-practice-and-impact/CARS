
#' @title Plot frequency subplots
#'
#' @description Produce multiple bar chart (plotly) for single factor frequency data.
#'Intended for use with frequencies represented as percentages.
#'Incomplete validation checks: some combinations of number of plots and number of rows/columns will fail:
#'Each column should contain at least two charts.
#'
#' @param data Frequency table (data frame). Expects a tidy dataset with 3 columns: factor 1 (categorical axes), factor 2 (one plot per factor level) and values.
#' @param xlab X axis title
#' @param ylab Y axis title
#' @param height plot height in pixels
#' @param width plot width in pixels
#' @param nrows number of rows in plot (min 2, 3 by default)
#' @param y_margin margin between rows (chart proportion), 0.1 by default.
#' @param x_margin margin between columns (chart proportion). 0.1 by default.
#' @param bar_colour Colour name. Defaults to blue (see carsurvey2::get_gradient())
#' @param font_size minimum font size for the plot (numeric).
#' @param orientation plot orientation ("h" = horizontal, "v" = verical). Vertical by default
#'
#' @return subplots
#'
#' @export

freq_subplots <- function(data, xlab, ylab, height, width, bar_colour, nrows = 3,
                          y_margin = .1, x_margin = .1, font_size = 12, orientation = "h") {

  if (nrows == 1) {
    stop("Unexpected input: n_rows should be 2 or greater.")
  }

  n_plots <- length(unique(data[[2]]))
  ncols <- ceiling(n_plots / nrows)

  # The R plotly implementation handles margins poorly. Custom margins are created here by filling
  # spaces with blank plotly objects. This is done by the loops below, as well as the height and width settings.
  plot_positions <- sapply(1:n_plots, function(i) {
    row <- ceiling(i / ncols)
    col <- i - (row - 1) * ncols

    # Position = chart index + space occupied by previous blank rows + 1 space per previous spaces between charts
    pos <- i + (row - 1) * ncols + 2 * (ncols - 1) * (row - 1) + col - 1

    return(pos)
  })

  n_cells <- plot_positions[length(plot_positions)]

  plots <- list()
  plot_index <- 1

  for (i in 1:n_cells) {

    if (i %in% plot_positions) {
      filter <- unique(data[[2]])[plot_index]

      plot_data <- data[data[[2]] == filter, c(1, 3)]

      plot <- plot_freqs(plot_data,
                         ylab = ylab,
                         xlab = xlab,
                         title = filter,
                         font_size = font_size,
                         orientation = orientation,
                         height = height,
                         width = width)

      if (orientation == "h") {
        plot <- plot %>% set_axis_range(0, 1, axis = "x")
      } else if (orientation == "v") {
        plot <- plot %>% set_axis_range(0, 1, axis = "y")
      }

      plots[[i]] <- plotly::plotly_build(plot)

      plot_index <- plot_index + 1
    } else {
      plots[[i]] <- plotly::plotly_build(plotly::plotly_empty())
    }
  }

  # Available non-margin space / the number of non-empty rows
  plot_height <- (1 - y_margin * (nrows - 1)) / nrows
  heights <- c(plot_height, rep(c(y_margin, plot_height), nrows - 1))

  plot_width <- (1 - x_margin * (ncols - 1)) / ncols
  widths <- c(plot_width, rep(c(x_margin, plot_width), ncols - 1))

  # Annotation settings
  all_positions <- expand.grid(cumsum(widths), 1 - cumsum(heights) + plot_height / 2)
  annotation_locations <- all_positions[plot_positions, ]

  annotations <- lapply(1:n_plots, function(i) {
    return(
      list(
        x = annotation_locations[i, 1] - plot_width / 2,
        y = annotation_locations[i, 2] + plot_height / 2,
        text = unique(data[[2]])[i],
        xref = "paper",
        yref = "paper",
        xanchor = "center",
        yanchor = "bottom",
        showarrow = FALSE,
        font = list(size = font_size * 1.2)
      )
    )
  })

  return(subplot <- plotly::subplot(plots, heights = heights, widths = widths, nrows = nrows * 2 - 1,
                                    titleX = TRUE, titleY = TRUE, margin = 0) %>%
           plotly::layout(autosize = TRUE, x = list(), showlegend = FALSE, annotations = annotations) %>%
           plotly::config(displayModeBar = FALSE))

}

#' @title Plot frequency graph
#'
#' @description Produce bar chart (plotly) for single factor frequency data.
#'
#' @param data Frequency data (data frame). Expected input: data.frame(categories = c(), frequencies = c())
#' @param n sample size (optional)
#' @param colour Colour name. Defaults to blue (see @get_gradient())
#' @param break_q_names_col applies break_q_names to the column. Not applied by default
#' @param type optional: chart type ("bar" or "line").
#' @param max_lines maximum number of lines. Int, defaults to 2/ See carsurvey::break_q_names()
#' @param xlab X axis title (optional)
#' @param ylab Y axis title (optional)
#' @param font_size minimum font size for the plot (numeric).
#' @param orientation plot orientation ("h" = horizontal, "v" = verical). Vertical by default
#' @param ... additional plotly_ly arguments
#'
#' @return bar chart
#'
#' @export


plot_freqs <- function(data, n, colour, break_q_names_col, type = c("bar", "line"),
                       max_lines = 2,  xlab = "", ylab = "", font_size = 12,
                       orientation = c("v", "h"), ...) {

  # Set default bar colour
  if (missing(colour)) {
    colour <- get_gradient(1)[[1]]
  } else if (!is.character(colour) | length(colour) != 1) {
    stop("Unexpected input - colour should be a single colour name.")
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
  type <- match.arg(type)

  # Apply break_q_names to a column
  if(!missing(break_q_names_col)) {
    # Coerce to character type
    data[[break_q_names_col]] <- as.character(data[[break_q_names_col]])

    data[[break_q_names_col]] <- break_q_names(data[[break_q_names_col]], max_lines = max_lines)

    data[[break_q_names_col]] <- factor(data[[break_q_names_col]], levels = data[[break_q_names_col]])
  }

  axes <- axis_settings(xlab, ylab, font_size)

  if (orientation == "v") {
    data[[1]] <- factor(data[[1]], levels = unique(data[[1]]))
    x_vals <- data[[1]]
    y_vals <- data[[2]]
    x_axis <- axes$cat_axis
    y_axis <- axes$scale_axis
  } else if (orientation == "h") {
    data[[1]] <- factor(data[[1]], levels = rev(unique(data[[1]])))
    x_vals <- data[[2]]
    y_vals <- data[[1]]
    x_axis <- axes$scale_axis
    y_axis <- axes$cat_axis
    ylab <- xlab
  }

  y_axis$title <- "" # Y axis title is created as a caption instead

  sample <- ifelse(!missing(n), paste0("Sample size = ", n), "")

  if (type == "bar") {
    fig <- plotly::plot_ly(
      x = x_vals,
      y = y_vals,
      marker = list(color = colour),
      type = "bar",
      orientation = orientation,
      ...
    )
  } else if (type == "line") {
    fig <- plotly::plot_ly(
      x = x_vals,
      y = y_vals,
      marker = list(color = colour),
      line = list(color = colour),
      type = "scatter",
      mode = "markers+lines",
      orientation = orientation,
      ...
    )
  }


  fig <- plotly::config(fig, displayModeBar = F)
  fig <- plotly::layout(fig,
                        xaxis = x_axis,
                        yaxis = y_axis,
                        margin = list(b = 100, t = font_size * 2),
                        hoverlabel = list(bgcolor = "white", font = list(size = font_size)),
                        annotations = list(x = 1, y = 0, text = sample,
                                           showarrow = F, xanchor='right', yanchor='auto', xshift=0, yshift=-100,
                                           xref='paper', yref='paper', font=list(size = font_size))
  )

  fig <- plotly::layout(fig, annotations = create_y_lab(ylab, font_size))

  return(fig)

}


#' @title Plot stacked bar graph
#'
#' @description Produce stacked bar chart (plotly).
#'
#' @param data Frequency data for stacked bar chart (data frame). 3 columns: variable 1, variable 2 and values (tidy data)
#' @param n sample size
#' @param break_q_names_col applies break_q_names to the column. Not applied by default
#' @param type optional: chart type ("bar" or "line").
#' @param max_lines maximum number of lines. Int, defaults to 2/ See carsurvey::break_q_names()
#' @param xlab X axis title
#' @param ylab Y axis title
#' @param colour_scale type of colour scale ("gradient", "scale" or "2gradients"). See get_gradient(), get_2colour_scale() and get_2colour_gradients().
#' @param font_size minimum font size for the plot (numeric).
#' @param neutral_mid whether the midpoint of the colour scale should be neutral ("2gradients" scale only). TRUE by default
#' @param orientation plot orientation ("h" = horizontal, "v" = verical). Vertical by default
#' @param ... additional plotly_ly arguments
#'
#' @return bar chart
#'
#' @export

plot_stacked <- function(data, n, break_q_names_col, type = c("bar", "line"), max_lines = 2, xlab = "", ylab = "", colour_scale = c("2gradients", "gradient", "scale", "3scale"), font_size = 12, neutral_mid = TRUE, orientation = c("h", "v"), ...) {

  # Validate data
  if (!is.data.frame(data)) {
    stop("Unexpected input - data is not a data.frame.")
  } else if (ncol(data) != 3) {
    stop("Unexpected input - data should have three columns.")
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

    data[[break_q_names_col]] <- factor(data[[break_q_names_col]], levels = unique(data[[break_q_names_col]]))
  }

  colour_scale <- match.arg(colour_scale)
  orientation <- match.arg(orientation)
  type <- match.arg(type)

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

  colours <- lapply(colours, rep, length(unique(data[[1]]))) %>% unlist

  axes <- axis_settings(xlab, ylab, font_size)

  if (orientation == "v") {
    data[[1]] <- factor(data[[1]], levels = unique(data[[1]]))
    x_vals <- data[[1]]
    y_vals <- data[[3]]
    x_axis <- axes$cat_axis
    y_axis <- axes$scale_axis
  } else if (orientation == "h") {
    data[[1]] <- factor(data[[1]], levels = rev(unique(data[[1]])))
    x_vals <- data[[3]]
    y_vals <- data[[1]]
    x_axis <- axes$scale_axis
    y_axis <- axes$cat_axis
    ylab <- xlab
  }

  y_axis$title <- "" # Y axis title is created as a caption instead

  sample <- ifelse(!missing(n), paste0("Sample size = ", n), "")

  if (type == "bar") {
  fig <- plotly::plot_ly(y = y_vals,
                         x = x_vals,
                         color = data[[2]],
                         type = "bar",
                         orientation = orientation,
                         marker = list(color = colours),
                         ...)
  } else if (type == "line") {
    fig <- plotly::plot_ly(y = y_vals,
                           x = x_vals,
                           color = data[[2]],
                           type = "scatter",
                           mode = "markers+lines",
                           orientation = orientation,
                           marker = list(color = colours),
                           line = list(color = colours),
                           ...)
  }

  fig <- plotly::config(fig, displayModeBar = F)

  fig <- plotly::layout(fig,
                        barmode = "stack",
                        clickmode = "none",
                        showlegend = TRUE,
                        legend = list(orientation = orientation,   # show entries horizontally
                                      xanchor = "center",  # use center of legend as anchor
                                      yanchor = "bottom",
                                      x = 0.5,
                                      y = 1,
                                      traceorder = "normal",
                                      font = list(size = font_size)),
                        margin = list(b = 100, t = font_size * 2),
                        xaxis = x_axis,
                        yaxis = y_axis,
                        hoverlabel = list(bgcolor = "white", font = list(size = font_size)),
                        annotations = list(x = 1, y = 0, text = sample,
                                           showarrow = F, xanchor='right', yanchor='auto', xshift=0, yshift=-100,
                                           xref='paper', yref='paper', font=list(size = font_size)
                        )
  )

  fig <- plotly::layout(fig, annotations = create_y_lab(ylab, font_size))

  return(fig)

}


#' @title Plot grouped frequency graph
#'
#' @description Produce bar chart (plotly) for frequency data with grouping variable.
#'
#' @param data Frequency data (data frame). 3 columns - category names, groups and frequencies.
#' @param n sample size
#' @param break_q_names_col applies break_q_names to the column. Not applied by default
#' @param max_lines maximum number of lines. Int, defaults to 2/ See carsurvey::break_q_names()
#' @param xlab X axis title
#' @param ylab Y axis title
#' @param font_size minimum font size for the plot (numeric).
#' @param orientation plot orientation ("h" = horizontal, "v" = verical). Vertical by default.
#' @param ... additional plotly_ly arguments
#'
#' @return bar chart
#'
#' @export

plot_grouped <- function(data, n, break_q_names_col, max_lines = 2, xlab = "", ylab = "", font_size = 12, orientation = c("v", "h"), ...) {

  # Set default bar colours
  n_groups <- length(unique(data[[2]]))
  colours <- get_2colour_gradients(n_groups)

  colour_list <- rev(as.list(colours))
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

  data[[1]] <- factor(data[[1]], levels = unique(data[[1]]))
  data[[2]] <- factor(data[[2]], levels = unique(data[[2]]))

  orientation <- match.arg(orientation)

  axes <- axis_settings(xlab, ylab, font_size)

  if (orientation == "v") {
    data[[1]] <- factor(data[[1]], levels = unique(data[[1]]))
    data[[2]] <- factor(data[[2]], levels = unique(data[[2]]))
    x_vals <- data[[1]]
    y_vals <- data[[3]]
    x_axis <- axes$cat_axis
    y_axis <- axes$scale_axis
    legend = list(traceorder = 'normal')
    hovertext <- paste0(data[[1]], ": ", round(abs(y_vals) * 100, 1), "%", " <extra></extra>")
  } else if (orientation == "h") {
    data[[1]] <- factor(rev(data[[1]]), levels = rev(unique(data[[1]])))
    data[[2]] <- factor(rev(data[[2]]), levels = rev(unique(data[[2]])))
    colours <- rev(colours)
    x_vals <- rev(data[[3]])
    y_vals <- data[[1]]
    x_axis <- axes$scale_axis
    y_axis <- axes$cat_axis
    legend = list(traceorder = 'reversed')
    ylab <- xlab
    hovertext <- paste0(data[[1]], ": ", round(abs(x_vals) * 100, 1), "%", " <extra></extra>")
  }

  y_axis$title <- ""

  sample <- ifelse(!missing(n), paste0("Sample size = ", n), "")

  fig <- plotly::plot_ly(
    x = x_vals,
    y = y_vals,
    color = data[[2]],
    marker = list(color = colours),
    type = "bar",
    hovertemplate = hovertext,
    ...
  )

  fig <- plotly::config(fig, displayModeBar = F)
  fig <- plotly::layout(fig,
                        xaxis = x_axis,
                        yaxis = y_axis,
                        legend = legend,
                        margin = list(b = 100, t = font_size * 2),
                        hoverlabel = list(bgcolor = "white", font = list(size = font_size)),
                        annotations = list(x = 1, y = 0, text = sample,
                                           showarrow = F, xanchor='right', yanchor='auto', xshift=0, yshift=-100,
                                           xref='paper', yref='paper', font=list(size = font_size))
  )

  fig <- plotly::layout(fig, annotations = create_y_lab(ylab, font_size))

  return(fig)

}


#' @title Plot Likert graph
#'
#' @description Produce Likert stacked bar chart (plotly). At least 2 questions per plot.
#'
#' @param data Frequency data for likert quesitons (data frame). 3 columns: question, answer option, frequency.
#' @param mid the mid-point of the scale. should be higher than 2 and lower than the number of answers.
#' @param xlab X axis title
#' @param ylab Y axis title
#' @param n sample size
#' @param max_lines changes maximum lines text can go over
#' @param font_size minimum font size for the plot (numeric).
#' @param neutral_mid whether the middle of the scale should be a neutral category (logical). TRUE by default
#' @param break_q_names_col applies break_q_names to the column. Not applied by default
#' @param ... additional plot_ly arguments
#'
#' @return bar chart
#'
#' @export

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

  # Validate neutral mid
  if (!is.logical(neutral_mid)) {
    stop("Unexpected input - neutral_mid is not logical.")
  }

  # Validate mid
  if (!is.numeric(mid)) {
    stop("Unexpected input - mid is not numeric.")
  } else if (mid < 2) {
    stop("Unexpected input - mid is smaller than 2.")
  } else if (neutral_mid & mid >= n_answers) {
    stop("Unexpected input - mid >= the number of answers.")
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
    title = "Percent"
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

  sample <- ifelse(!missing(n), paste0("Sample size = ", n), "")

  fig <- plotly::plot_ly(y = data[[1]],
                         x = data[[3]],
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
                        margin = list(b = 100, t = font_size * 2),
                        annotations = list(x = 1, y = 0, text = sample,
                                           showarrow = F, xanchor='right', yanchor='auto', xshift=0, yshift=-100,
                                           xref='paper', yref='paper', font=list(size = font_size)),
                        xaxis = x,
                        yaxis = y,
                        hoverlabel = list(bgcolor = "white", font = list(size = font_size)))

  tick_text <- paste0(round(abs(seq(-1, 1, .5)) * 100, digits = 2), "%")
  tick_values <- seq(-1, 1, .5)

  fig <- plotly::layout(fig,
                        annotations = create_y_lab(ylab, font_size),
                        xaxis = list(ticktext = tick_text,
                                      tickvals = tick_values,
                                      title = xlab))

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

#' @title Calculate bases for Likert chart
#'
#' @description Calculates starting locations for bars on the Likert chart.
#'
#' @param data see @plot_likert
#' @param mid see @plot_likert
#' @param neutral_mid see @plot_likert
#'
#' @return bar starting locations

calculate_bases <- function(data, mid, neutral_mid) {

  data[[2]] <- factor(data[[2]], unique(data[[2]]))

  n_questions <- length(unique(data[[1]]))

  modified_cumsum <- function(x) {
    slice <- x[1:(length(x)-1)]

    return(
      c(0, cumsum(slice))
    )
  }

  cumulative_sums <- data %>%
    dplyr::group_by_at(1) %>%
    dplyr::mutate_at(3, modified_cumsum)

  bases <- cumulative_sums[[3]]

  get_neg_bases <- function(x, mid, neutral_mid) {
    if (neutral_mid) {
      sum(x[c(1:(mid-1))]) + x[mid]/2
    } else {
      sum(x[c(1:(mid-1))])
    }
  }

  negative_bases <- data %>%
    dplyr::group_by_at(1) %>%
    dplyr::mutate_at(3, get_neg_bases, mid = mid, neutral_mid = neutral_mid) %>%
    data.frame()

  bases <- bases - negative_bases[[3]]

  return(bases)
}


#' @title Set axis range
#'
#' @description Sets x or y axis range for plotly objects
#'
#' @param plot plotly object
#' @param min minimum value
#' @param max maximum value
#' @param axis optional: defaults to "x"
#'
#' @return list of parameters for plotly annotation
#'
#' @export

set_axis_range <- function(plot, min, max, axis = c("x", "y")) {
  axis <- match.arg(axis)

  if (axis == "x") {
    plot <- plot %>% plotly::layout(xaxis = list(zerolinecolor = '#ffff',
                                                 range = list(min, max)))
  } else if (axis == "y") {
    plot <- plot %>% plotly::layout(yaxis = list(zerolinecolor = '#ffff',
                                                 range = list(min, max)))
  }

  return(plot)
}


#' @title Set up error bars
#'
#' @description Formats plotly error bar settings. Can be used with plots
#'
#' @param lower_ci lower confidence interval
#' @param upper_ci upper confidence interval
#'
#' @return list of parameters for plotly error bars
#'
#' @export

set_error_bars <- function(lower_ci, upper_ci) {
  return(
    list(
      symmetric = "false",
      array = upper_ci,
      arrayminus = lower_ci,
      color = "black"
    )
  )
}


#' @title Set axis range
#'
#' @description Sets x or y axis range for plotly objects
#'
#' @param plot plotly object
#' @param min minimum value
#' @param max maximum value
#' @param axis optional: defaults to "x"
#'
#' @return list of parameters for plotly annotation
#'
#' @export

set_axis_range <- function(plot, min, max, axis = c("x", "y")) {
  axis <- match.arg(axis)

  if (axis == "x") {
    plot <- plot %>% plotly::layout(xaxis = list(zerolinecolor = '#ffff',
                                               range = list(min, max)))
  } else if (axis == "y") {
    plot <- plot %>% plotly::layout(yaxis = list(zerolinecolor = '#ffff',
                                                 range = list(min, max)))
  }

  return(plot)
}


#' @title Create custom Y axis label
#'
#' @description Create a custom y axis label (plotly annotation). This label is placed just above the y axis
#'  and is horizontal, to replace the vertically flipped label produced by default.
#'
#' @param ylab Y axis label
#' @param font_size font size used in the chart. This function will return a slightly larger font.
#'
#' @return list of parameters for plotly annotation
#'
#' @export

create_y_lab <- function(ylab, font_size) {
  annotation <- list(text = ylab, # Custom Y axis label
                     y = 1,
                     x = "min",
                     showarrow = FALSE,
                     yshift = font_size * 2,
                     xref = "paper",
                     yref = "paper",
                     font = list(size = font_size * 1.2)
  )

  return(annotation)

}

#' @title Axis settings
#'
#' @description Standard X (categorical) and Y (percentage scale) axis settings for use with the plotting functions.
#'
#' @param xlab see @plot_freqs
#' @param ylab see @plot_freqs
#' @param font_size see @plot_freqs
#'
#' @return list of axis settings.

axis_settings <- function(xlab, ylab, font_size) {
  return(
    list(
      scale_axis = list(
        title = ylab,
        tickfont = list(size = font_size),
        titlefont = list(size = font_size * 1.2),
        tickformat = ".0%",
        title = "Percent"
      ),
      cat_axis = list(
        title = xlab,
        tickfont = list(size = font_size),
        titlefont = list(size = font_size * 1.2)
      )
    )
  )
}
