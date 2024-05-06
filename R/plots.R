# custom theme for plots, based on theme_bw()
#' @importFrom ggplot2 %+replace%
theme_lac <- function(){
  ggplot2::theme_bw() %+replace% 
    ggplot2::theme(panel.grid = ggplot2::element_blank()
                   )
}

# simple validator function for checking data.frames prior to plotting
check_dataframes <- function(x, columns){
  # generate logical vectors to check the inputs
  check_ident <- sapply(x, is.data.frame) # is each input a data.frame?
  checkcols <- sapply(x, function(x) {all(columns %in% colnames(x))}) # do they have the required columns?
  
  # throw error if any input is not a data.frame
  if (!all(check_ident)) {
    dataframe_error <- simpleError("Input must be a data.frame",
                                   call = sys.call(-1))
    stop(dataframe_error)
  }
  
  # throw error if any input is missing the required columns
  if (!all(checkcols)) {
    column_error <- simpleError(paste("Input data.frame(s) missing one or more of the required columns: \n\t",
                                   paste0("$", columns, 
                                          collapse = ", ")),
                             call = sys.call(-1))
    stop(column_error)
  }
}

#' Plot lacunarity curve(s)
#'
#' @param ... One or more [`data.frames`][data.frame()] containing lacunarity
#'   curve data. Must contain columns named `$box_size`, `$lacunarity`,
#'   `$lac_norm`, and `$H_r`.
#' @param log A Boolean. `TRUE` (default) displays the axes on a logarithmic
#'   scale, `FALSE` displays them on a linear scale. For `lacnorm_plot()` this
#'   only controls the x axis, as normalized lacunarity is by definition on a
#'   log scale.
#' @param group_names A character [`vector`][vector()] containing labels for any
#'   data.frames passed to `...`. These labels will appear on the plot legend.
#'   If `group_names` is left empty, the legend uses the names of the
#'   data.frames as supplied to `...`
#'
#' @return A `ggplot` object displaying the lacunarity or H(r) curve(s). If
#'   multiple curves are supplied, their ordering in the plot legend will
#'   reflect the order they were listed in the function call.
#'
#' @examples
#' # generate array
#' a <- array(data = rep(c(1,0), 125), dim = c(5,5,5))
#' # calculate lacunarity at all box sizes
#' lac_curve <- lacunarity(a, box_sizes = "all")
#' # plot Λ(r) curve
#' lac_plot(lac_curve)
#'
#' @export
#'
#' @importFrom rlang .data
#' @rdname lac_plot
lac_plot <- function(..., log = TRUE, group_names = NULL){
  # check the input data.frames
  check_dataframes(list(...), columns = c("box_size", "lacunarity"))
  
  # convert ellipsis arguments to named list
  if (!is.null(group_names)){
    args <- stats::setNames(list(...), group_names)
  } else {
    args <- stats::setNames(list(...), as.character(substitute(...())))
  }
  
  # concatenate data.frames, passing variable names as new column
  lac <- data.table::rbindlist(args, idcol = "Source")
  
  # reorder the levels of Source column to reflect input order
  lac$Source <- factor(lac$Source, levels = as.character(substitute(...())))
  
  # determine x axis breaks
  box_max <- max(lac$box_size) # find the max x value
  if (log == TRUE){
    xbreaks <- unique(c( 2^( 0:floor(log2(box_max)) ), box_max)) # log2 scale
  } else {
    xbreaks <- ggplot2::waiver() # linear scale
  }
  
  # set grouping variable in case of multiple data.frames
  if (length(args) > 1){ 
    group <- rlang::quo(.data$Source) 
  } else { 
    group <- NULL 
  }
  
  # generate plot
  p <- ggplot2::ggplot(lac, 
                       ggplot2::aes(x = .data$box_size,
                                    y = .data$lacunarity,
                                    shape = !!group,
                                    color = !!group,
                                    linetype = !!group)) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::scale_x_continuous(trans = ifelse(log == TRUE, 
                                               "log", 
                                               "identity"),
                                breaks = xbreaks) +
    ggplot2::scale_y_continuous(trans = ifelse(log == TRUE, 
                                               "log", 
                                               "identity"),
                                limits = c(1,max(lac$lacunarity)),
                                breaks = 1:floor(max(lac$lacunarity))) +
    ggplot2::labs(x = "Box size",
                  y = expression("Lacunarity"~Lambda*"(r)")) +
    theme_lac()
  
  return(p)
}

#' @export
#' @rdname lac_plot
lacnorm_plot <- function(..., log = TRUE, group_names = NULL){
  # check the input data.frames
  check_dataframes(list(...), columns = c("box_size", "lac_norm"))
  
  # convert ellipsis arguments to named list
  if (!is.null(group_names)){
    args <- stats::setNames(list(...), group_names)
  } else {
    args <- stats::setNames(list(...), as.character(substitute(...())))
  }
  
  # concatenate data.frames, passing variable names as new column
  lac <- data.table::rbindlist(args, idcol = "Source")
  
  # reorder the levels of Source column to reflect input order
  lac$Source <- factor(lac$Source, levels = as.character(substitute(...())))
  
  # determine x axis breaks
  box_max <- max(lac$box_size) # find the max x value
  if (log == TRUE){
    xbreaks <- unique(c( 2^( 0:floor(log2(box_max)) ), box_max)) # log2 scale
  } else {
    xbreaks <- ggplot2::waiver() # linear scale
  }
  
  # set grouping variable in case of multiple data.frames
  if (length(args) > 1){ 
    group <- rlang::quo(.data$Source) 
  } else { 
    group <- NULL 
  }
  
  # generate plot
  p <- ggplot2::ggplot(lac, 
                       ggplot2::aes(x = .data$box_size,
                                    y = .data$lac_norm,
                                    shape = !!group,
                                    color = !!group,
                                    linetype = !!group)) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::scale_x_continuous(trans = ifelse(log == TRUE, 
                                               "log", 
                                               "identity"),
                                breaks = xbreaks) +
    ggplot2::scale_y_continuous(trans = "identity",
                                limits = c(0,1),
                                breaks = ggplot2::waiver()) +
    ggplot2::labs(x = "Box size",
                  y = expression("Normalized"~log~Lambda*"(r)")) +
    theme_lac()
  
  return(p)
}

#' @export
#' @rdname lac_plot
hr_plot <- function(..., group_names = NULL){
  # check the input data.frames
  check_dataframes(list(...), columns = c("box_size", "H_r"))
  
  # convert ellipsis arguments to named list
  if (!is.null(group_names)){
    args <- stats::setNames(list(...), group_names)
  } else {
    args <- stats::setNames(list(...), as.character(substitute(...())))
  }
  
  # concatenate data.frames, passing variable names as new column
  lac <- data.table::rbindlist(args, idcol = "Source")
  
  # reorder the levels of Source column to reflect input order
  lac$Source <- factor(lac$Source, levels = as.character(substitute(...())))
  
  # determine x axis breaks
  box_max <- max(lac$box_size) # find the max x value
  xbreaks <- unique(c( 2^( 0:floor(log2(box_max)) ), box_max)) # log scale
  
  # set grouping variable in case of multiple data.frames
  if (length(args) > 1){ 
    group <- rlang::quo(.data$Source) 
  } else { 
    group <- NULL 
  }
  
  # generate plot
  p <- ggplot2::ggplot(lac, 
                       ggplot2::aes(x = .data$box_size,
                                    y = .data$H_r,
                                    shape = !!group,
                                    color = !!group,
                                    linetype = !!group)) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::geom_hline(yintercept =  0.5, linetype = "dashed") +
    ggplot2::scale_x_continuous(trans = "log",
                                breaks = xbreaks) +
    ggplot2::scale_y_continuous(trans = "identity",
                                limits = c(-0.1,1.1),
                                breaks = c(0, 0.25, 0.5, 0.75, 1)) +
    ggplot2::labs(x = "Box size",
                  y = "H(r)") +
    theme_lac()
  
  return(p)
}