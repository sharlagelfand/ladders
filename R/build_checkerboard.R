#' @export
build_checkerboard <- function(data, palette) {
  x <- dplyr::tibble(
    xmin = seq(data$xmin, data$xmax, length.out = sample(20:30, 1)),
    xmax = dplyr::lead(xmin),
    size = xmax - xmin
  ) %>%
    dplyr::mutate(x_id = dplyr::row_number())

  ymin <- seq(data$ymin, data$ymax, by = x[["size"]][[1]])
  if (!any(ymin == data$ymax)) {
    ymin <- c(ymin, data$ymax)
  }

  df <- tidyr::crossing(xmin = x$xmin, ymin = ymin) %>%
    dplyr::left_join(x, by = "xmin") %>%
    dplyr::group_by(xmin) %>%
    dplyr::mutate(
      fill = x_id %% 2 == dplyr::row_number() %% 2,
      ymax = dplyr::lead(ymin)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::filter(!is.na(ymax))

  if (true_or_false()) {
    palette <- rev(palette)
  }

  df <- df %>%
    dplyr::mutate(fill = ifelse(fill, palette[[1]], palette[[2]])) %>%
    dplyr::select(xmin, xmax, ymin, ymax, fill) %>%
    dplyr::mutate(geom = "rect")

  df
}
