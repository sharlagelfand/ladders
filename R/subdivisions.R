#' @export
subdivide_rectangle <- function(data, min_size = NULL) {
  x_length <- data$xmax - data$xmin
  y_length <- data$ymax - data$ymin

  y_split <- stats::runif(1, data$ymin, data$ymax)

  if (!is.null(min_size)) {
    smallest_size <- min(c(
      y_split - data$ymin,
      data$ymax - y_split
    ))

    if (smallest_size < min_size) {
      return(
        list(data)
      )
    }
  }

  one <- data %>%
    dplyr::mutate(ymax = y_split)

  two <- data %>%
    dplyr::mutate(ymin = y_split)

  list(one, two)
}

#' @export
buncha_subdivisions <- function(rectangle, n = 2, min_size = NULL, seed = NULL) {
  if (!is.null(seed)) {
    set.seed(seed)
  }
  rectangle <- list(rectangle)

  for (i in 1:n) {
    rectangle <- purrr::map(rectangle, subdivide_rectangle, min_size = min_size) %>%
      unlist(recursive = FALSE)
  }

  return(rectangle)
}
