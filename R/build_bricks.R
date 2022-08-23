build_bricks <- function(data, palette, palette_style) {
  n_rows <- sample(3:6, 1)
  y_size <- (data[["ymax"]] - data[["ymin"]]) / n_rows
  x_size <- y_size * 2

  ymin <- dplyr::tibble(ymin = seq(data[["ymin"]], data[["ymax"]] - y_size, by = y_size)) %>%
    dplyr::mutate(y_rank = dplyr::row_number())
  # Add excess for shifting x on odd rows
  xmin <- seq(data[["xmin"]] - x_size, data[["xmax"]] + x_size, by = x_size)

  rect <- tidyr::crossing(
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
          ifelse(y_rank %% 2 == 0, x + x_size / 2, x)
        }
      )
    ) %>%
    dplyr::filter(
      !(
        (xmin < data[["xmin"]] & xmax < data[["xmin"]]) |
          (xmin > data[["xmax"]] & xmax > data[["xmax"]]))
    ) %>%
    # Trim excess, e.g < xmin or > xmax
    dplyr::mutate(
      xmin = ifelse(xmin < data[["xmin"]], data[["xmin"]], xmin),
      xmax = ifelse(xmax > data[["xmax"]], data[["xmax"]], xmax)
    ) %>%
    dplyr::mutate(
      fill = colours[["lightpink"]],
      colour = NA_character_,
      geom = "rect"
    )

  segment_horizontal <- tidyr::expand_grid(
    ymin = ymin %>%
      dplyr::filter(y_rank != 1) %>%
      dplyr::pull(ymin),
    xmin = data[["xmin"]],
    xmax = data[["xmax"]]
  ) %>%
    dplyr::mutate(ymax = ymin)

  segment_vertical <- rect %>%
    dplyr::select(xmin, ymin, ymax) %>%
    dplyr::mutate(xmax = xmin) %>%
    dplyr::filter(!xmin %in% c(data[["xmin"]], data[["xmax"]]))

  segment <- dplyr::bind_rows(
    segment_horizontal,
    segment_vertical
  ) %>%
    dplyr::mutate(
      geom = "segment",
      colour = colours[["darkpink"]]
    )

  dplyr::bind_rows(
    segment,
    rect
  )
}
