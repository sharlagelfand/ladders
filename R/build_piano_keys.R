#' @export
build_piano_keys <- function(data, palette, palette_style) {
  if (palette_style == "mono") {
    tone_colour <- interval_colour <- palette
    tone_outline <- interval_outline <- "black"
  } else if (palette_style == "duo") {
    style <- sample(c("solid intervals", "outline intervals", "colour tones"), 1)

    if (true_or_false()) {
      palette <- rev(palette)
    }

    tone_colour <- palette[[1]]
    interval_colour <- palette[[2]]

    if (style == "solid intervals") {
      tone_outline <- "black"
      interval_outline <- palette[[2]]
    } else if (style == "outline intervals") {
      tone_outline <- "black"
      interval_outline <- "black"
    } else if (style == "colour tones") {
      tone_outline <- palette[[2]]
      interval_outline <- palette[[2]]
    }
  } else if (palette_style == "multi") {

  }

  tones <- dplyr::tibble(
    xmin = seq(data$xmin, data$xmax, length.out = sample(20:40, 1)),
    xmax = xmin
  ) %>%
    dplyr::mutate(
      geom = "segment",
      ymin = data$ymin,
      ymax = data$ymax,
      colour = tone_outline
    )

  tones_rect <- tones %>%
    dplyr::mutate(
      xmax = dplyr::lead(xmax),
      xmax = dplyr::coalesce(xmax, data$xmax),
      fill = tone_colour,
      colour = NA,
      geom = "rect"
    )

  tones <- tones %>%
    dplyr::mutate(x = xmin, y = xmax) %>%
    generate_noise("simplex", 1) %>%
    dplyr::mutate(noise_ntile = dplyr::ntile(noise, 10)) %>%
    dplyr::filter(noise_ntile %in% sample(1:10, 9)) %>%
    dplyr::select(-x, -y, -noise)

  intervals <- tones %>%
    dplyr::mutate(x_prev = dplyr::lag(xmin), x_next = dplyr::lead(xmin)) %>%
    dplyr::filter(noise_ntile %in% sample(1:10, 3)) %>%
    dplyr::mutate(
      geom = "rect",
      x_size = pmin(xmin - x_prev, x_next - xmin),
      x_size = x_size / 3,
      xmin = xmin - x_size,
      xmax = xmin + x_size * 2,
      ymin = data$ymax - (data$ymax - data$ymin) * 0.4,
      ymax = data$ymax,
      fill = interval_colour,
      colour = NA
    ) %>%
    dplyr::select(xmin, xmax, ymin, ymax, geom, fill, colour) %>%
    dplyr::mutate(order = 2)

  tones_segment <- dplyr::bind_rows(
    intervals %>% dplyr::select(xmin, ymin, ymax, order) %>% dplyr::mutate(xmax = xmin),
    intervals %>% dplyr::select(xmax, ymin, ymax, order) %>% dplyr::mutate(xmin = xmax),
    intervals %>% dplyr::select(xmax, xmin, ymin, order) %>% dplyr::mutate(ymax = ymin),
  ) %>%
    dplyr::mutate(
      geom = "segment",
      colour = interval_outline
    )

  dplyr::bind_rows(
    tones %>%
      dplyr::filter(!dplyr::row_number() %in% c(1, dplyr::n())),
    tones_segment,
    tones_rect,
    intervals
  ) %>%
    dplyr::select(-noise_ntile)
}
