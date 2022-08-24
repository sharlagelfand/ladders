#' @export
ladder <- function(seed = NULL, width = 8.5, height = 11) {
  if (!is.null(seed)) {
    set.seed(seed)
  }

  original_df <- tibble::tibble(
    xmin = 0, xmax = width,
    ymin = 0, ymax = height
  )

  outline_df <- original_df %>%
    buncha_subdivisions(n = 10, min_size = 0.5) %>%
    dplyr::bind_rows(.id = "id") %>%
    dplyr::mutate(
      height = ymax - ymin,
      x = xmin, y = ymin
    ) %>%
    generate_noise("perlin", 0.9) %>%
    dplyr::select(-x, -y, -height) %>%
    option_from_noise(options = c(
      "striped", "piano keys", "bricks", "checkerboard"
    ))

  # palette_style <- sample(c("mono", "duo", "multi"), 1,
  #   prob = c(1, 1, 3)
  # )
  palette_style <- sample(c("mono", "duo"), 1, prob = c(1, 3))

  palette <- switch(palette_style,
    mono = sample(colours, 1)[[1]],
    duo = sample(colours, 2),
    multi = colours
  )

  while (any(paste0(sort(names(palette)), collapse = "") == banned_combos) &
    palette_style == "duo") {
    palette <- sample(colours, 2)
  }

  options_df <- outline_df %>%
    dplyr::group_split(id) %>%
    purrr::map_dfr(function(data) {
      switch(data[["option"]],
        "striped" = build_striped(data, palette, palette_style),
        "piano keys" = build_piano_keys(data, palette, palette_style),
        "checkerboard" = build_checkerboard(data, palette, palette_style),
        "bricks" = build_bricks(data, palette, palette_style)
      ) %>%
        dplyr::mutate(option = data[["option"]])
    })

  ggplot2::ggplot() +
    ggplot2::geom_rect(
      data = options_df %>% dplyr::filter(geom == "rect", is.na(order)),
      ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = fill, colour = colour),
      na.rm = TRUE
    ) +
    ggplot2::geom_segment(
      data = options_df %>% dplyr::filter(geom == "segment", is.na(order)),
      ggplot2::aes(x = xmin, xend = xmax, y = ymin, yend = ymax, colour = colour),
      size = 0.4,
      na.rm = TRUE
    ) +
    ggplot2::geom_rect(
      data = options_df %>% dplyr::filter(geom == "rect", !is.na(order)),
      ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = fill, colour = colour),
      na.rm = TRUE
    ) +
    ggplot2::geom_segment(
      data = options_df %>% dplyr::filter(geom == "segment", !is.na(order)),
      ggplot2::aes(x = xmin, xend = xmax, y = ymin, yend = ymax, colour = colour),
      size = 0.25,
      na.rm = TRUE
    ) +
    ggplot2::geom_rect(
      data = outline_df,
      ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
      colour = "black",
      fill = NA,
      size = 0.25
    ) +
    ggplot2::scale_colour_identity() +
    ggplot2::scale_fill_identity() +
    ggplot2::coord_fixed() +
    ggplot2::theme_void() +
    ggplot2::theme(plot.background = ggplot2::element_rect(fill = colours[["linen"]], colour = colours[["linen"]]))
}
