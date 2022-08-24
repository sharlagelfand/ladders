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
      geom = "segment"
    )


  if (palette_style == "mono") {
    palette <- c(palette, "black")

    if (true_or_false()) {
      palette <- rev(palette)
    }

    rect_colour <- palette[[1]]
    segment_colour <- palette[[2]]

    if (true_or_false()) {
      rect_colour <- sample(palette, nrow(rect), replace = TRUE)
    }
  } else if (palette_style == "duo") {

    if (true_or_false()) {
      palette <- rev(palette)
    }

    style <- sample(c("black outline", "colour outline"), 1)

    if (style == "black outline") {
      segment_colour <- "black"

      fill_method <- sample(c("random", "perlin"), 1)

      if (fill_method == "random") {
        rect_colour <- sample(palette, nrow(rect), replace = TRUE)
      } else if (fill_method == "perlin") {
        rect_colour <- rect %>%
          dplyr::select(x = xmin, y = ymin) %>%
          generate_noise("perlin", runif(1, 0.1, 3), seed = NULL) %>%
          option_from_noise(palette) %>%
          dplyr::pull(option)
      }

    } else if (style == "colour outline") {
      rect_colour <- palette[[1]]
      segment_colour <- palette[[2]]
    }
  }

  segment <- segment %>%
    dplyr::mutate(
      colour = segment_colour
    )

  rect <- rect %>%
    dplyr::mutate(
      fill = rect_colour
    )

  dplyr::bind_rows(
    segment,
    rect
  )
}
