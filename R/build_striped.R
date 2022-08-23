#' @export
build_striped <- function(data, palette, palette_style) {

  df <- dplyr::tibble(
    xmin = seq(data$xmin, data$xmax, length.out = sample(20:50, 1)),
    xmax = dplyr::lead(xmin)
  ) %>%
    dplyr::mutate(
      xmax = dplyr::coalesce(xmax, data$xmax),
      fill = ifelse(dplyr::row_number() %% 2 == 0, colours[["lightpink"]], colours[["darkpink"]]),
      geom = "rect",
      ymin = data$ymin,
      ymax = data$ymax
    )

  if (sample(c(TRUE, FALSE), 1)) {
    df <- df %>%
      dplyr::mutate(
        fill = ifelse(!dplyr::row_number() %% 2 == 0, colours[["lightpink"]], colours[["darkpink"]])
      )
  }

  df
}
