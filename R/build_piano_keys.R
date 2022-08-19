#' @export
build_piano_keys <- function(data) {
  tones <- dplyr::tibble(
    xmin = seq(data$xmin, data$xmax, length.out = sample(20:40, 1)),
    xmax = xmin
  ) %>%
    dplyr::mutate(
      geom = "segment",
      ymin = data$ymin,
      ymax = data$ymax,
      colour = darkpink
    ) %>%
    dplyr::mutate(x = xmin, y = xmax) %>%
    generate_noise("simplex", 1) %>%
    dplyr::mutate(noise_ntile = dplyr::ntile(noise, 10)) %>%
    dplyr::filter(noise_ntile %in% sample(1:10, 9)) %>%
    dplyr::select(-x, -y, -noise)

  tones_rect <- tones %>%
    dplyr::mutate(
      xmax = dplyr::lead(xmax),
      xmax = dplyr::coalesce(xmax, data$xmax),
      fill = lightpink,
      geom = "rect"
    )

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
      fill = lightpink,
      colour = darkpink
    ) %>%
    dplyr::select(xmin, xmax, ymin, ymax, geom, fill, colour) %>%
    dplyr::mutate(order = 2)

  dplyr::bind_rows(
    tones %>%
      dplyr::filter(!dplyr::row_number() %in% c(1, dplyr::n())),
    tones_rect,
    intervals
  ) %>%
    dplyr::select(-noise_ntile)
}
