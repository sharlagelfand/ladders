#' @export
build_piano_keys <- function(data, palette, palette_style) {
  if (palette_style == "mono") {
    tone_colour <- interval_colour <- palette
    tone_outline <- interval_outline <- "black"

    if (true_or_false()) {
      interval_colour <- "black"
    }
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

  if (data$xmax - data$xmin < 0.75) {
    tones_sample <- 3:8
  } else if (data$xmax - data$xmin < 2) {
    tones_sample <- 5:15
  } else {
    tones_sample <- 20:40
  }

  tones <- dplyr::tibble(
    xmin = seq(data$xmin, data$xmax, length.out = sample(tones_sample, 1)),
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
    dplyr::rowwise() %>%
    dplyr::mutate(
      interval_prop = runif(1, 0.3, 0.7)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      geom = "rect",
      x_size = pmin(xmin - x_prev, x_next - xmin),
      x_size = x_size / 3,
      xmin = xmin - x_size,
      xmax = xmin + x_size * 2,
      ymin = data$ymax - (data$ymax - data$ymin) * interval_prop,
      ymax = data$ymax,
      fill = interval_colour,
      colour = NA
    ) %>%
    dplyr::select(xmin, xmax, ymin, ymax, geom, fill, colour) %>%
    dplyr::mutate(order = 2)

  # Optionally have keys come from above and below
  if (true_or_false()) {
    intervals <- intervals %>%
      dplyr::rowwise() %>%
      dplyr::mutate(
        flip_key = true_or_false(),
        ymin_new = ifelse(flip_key & ymin, data[["ymin"]], ymin),
        ymax_new = ifelse(flip_key, ymin, ymax)
      ) %>%
      dplyr::ungroup() %>%
      dplyr::select(-ymin, -ymax) %>%
      dplyr::rename(ymin = ymin_new, ymax = ymax_new)
  } else {
    intervals <- intervals %>%
      dplyr::mutate(flip_key = FALSE)
  }

  tones_segment <- dplyr::bind_rows(
    intervals %>% dplyr::select(xmin, ymin, ymax, order) %>% dplyr::mutate(xmax = xmin),
    intervals %>% dplyr::select(xmax, ymin, ymax, order) %>% dplyr::mutate(xmin = xmax),
    intervals %>%
      dplyr::select(xmax, xmin, ymin, ymax, order, flip_key) %>%
      dplyr::mutate(
        ymin_new = ifelse(flip_key, ymax, ymin),
        ymax_new = ifelse(flip_key, ymax, ymin)
      ) %>%
      dplyr::select(-ymin, -ymax) %>%
      dplyr::rename(ymin = ymin_new, ymax = ymax_new),
  ) %>%
    dplyr::mutate(
      geom = "segment",
      colour = interval_outline
    ) %>%
    dplyr::select(-flip_key)

  dplyr::bind_rows(
    tones %>%
      dplyr::filter(!dplyr::row_number() %in% c(1, dplyr::n())),
    tones_segment,
    tones_rect,
    intervals
  ) %>%
    dplyr::select(-noise_ntile)
}
