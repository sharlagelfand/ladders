#' @export
build_striped <- function(data) {
  df <- dplyr::tibble(
    xmin = seq(data$xmin, data$xmax, length.out = sample(20:30, 1)),
    xmax = dplyr::lead(xmin)
  ) %>%
    dplyr::mutate(
      xmax = dplyr::coalesce(xmax, data$xmax),
      fill = ifelse(dplyr::row_number() %% 2 == 0, lightpink, darkpink),
      geom = "rect",
      ymin = data$ymin,
      ymax = data$ymax
    )

  if (sample(c(TRUE, FALSE), 1)) {
    df <- df %>%
      dplyr::mutate(
        fill = ifelse(!dplyr::row_number() %% 2 == 0, lightpink, darkpink)
      )
  }

  df
}
