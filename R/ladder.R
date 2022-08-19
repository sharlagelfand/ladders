ladder <- function(seed = NULL, width = 8.5, height = 11) {

  if(!is.null(seed)) {
    set.seed(seed)
  }

  original_df <- tibble::tibble(
    xmin = 0, xmax = width,
    ymin = 0, ymax = height
  )

  outline_df <- original_df %>%
    buncha_subdivisions(n = 15, min_size = 1) %>%
    dplyr::bind_rows() %>%
    dplyr::mutate(
      height = ymax - ymin,
      x = xmin, y = ymin
    ) %>%
    generate_noise("perlin", 0.9) %>%
    dplyr::select(-x, -y, -height) %>%
    option_from_noise(options = c(
      "striped", "piano keys",
      # "bricks",
      "checkerboard"
    ))

  options_df <- outline_df %>%
    dplyr::group_split(ymin) %>%
    purrr::map_dfr(function(data) {
      switch(data[["option"]],
        "striped" = build_striped(data),
        "piano keys" = build_piano_keys(data),
        "checkerboard" = build_checkerboard(data)
      ) %>%
        dplyr::mutate(option = data[["option"]])
    })

  ggplot2::ggplot() +
    ggplot2::geom_segment(
      data = options_df %>% dplyr::filter(geom == "segment"),
      ggplot2::aes(x = xmin, xend = xmax, y = ymin, yend = ymax, colour = colour)
    ) +
    ggplot2::geom_rect(
      data = options_df %>% dplyr::filter(geom == "rect"),
      ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = colour)
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
    ggplot2::theme_void()
}
