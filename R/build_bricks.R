build_bricks <- function(data) {
  n_rows <- sample(3:6, 1)
  y_size <- (data[["ymax"]] - data[["ymin"]]) / n_rows
  x_size <- y_size * 2

  ymin <- dplyr::tibble(ymin = seq(data[["ymin"]], data[["ymax"]] - y_size, by = y_size)) %>%
    dplyr::mutate(y_rank = dplyr::row_number())
  # Add excess for shifting x on odd rows
  xmin <- seq(data[["xmin"]] - x_size, data[["xmax"]] + x_size, by = x_size)

  df <- tidyr::crossing(
    xmin = xmin,
    ymin = ymin[["ymin"]]
  ) %>%
    dplyr::left_join(ymin, by = "ymin") %>%
    dplyr::mutate(
      xmax = xmin + x_size,
      ymax = ymin + y_size,
      # Offset x if odd rows of y
      across(
        c(xmin, xmax),
        function(x) {
          ifelse(y_rank %% 2 == 0, xmin + x_size / 2, x)
        }
      )
    ) %>%
    # Trim excess, e.g < xmin or > xmax
    dplyr::mutate(
      xmin = ifelse(xmin < data[["xmin"]], data[["xmin"]], xmin),
      xmax = ifelse(xmax > data[["xmax"]], data[["xmax"]], xmax)
    ) %>%
    dplyr::mutate(
      fill = lightpink,
      colour = darkpink,
      geom = "rect"
    )

  df
}
