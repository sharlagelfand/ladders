#' @export
build_striped <- function(data, palette) {
  if (data$xmax - data$xmin < 2) {
    rect_sample <- 5:15
  } else {
    rect_sample <- 20:50
  }

  rect <- dplyr::tibble(
    xmin = seq(data$xmin, data$xmax, length.out = sample(rect_sample, 1)),
    xmax = dplyr::lead(xmin)
  ) %>%
    dplyr::mutate(
      xmax = dplyr::coalesce(xmax, data$xmax),
      geom = "rect",
      ymin = data$ymin,
      ymax = data$ymax
    )

  segment <- dplyr::bind_rows(
    rect %>%
      dplyr::select(xmin, ymin, ymax) %>% dplyr::mutate(xmax = xmin),
    rect %>%
      dplyr::select(xmax, ymin, ymax) %>% dplyr::mutate(xmin = xmax)
  ) %>%
    dplyr::mutate(geom = "segment", colour = "black") %>%
    dplyr::arrange(xmin) %>%
    dplyr::distinct() %>%
    dplyr::filter(!dplyr::row_number() %in% c(1, dplyr::n()))

  style <- sample(c("alternating", "blocks"), 1)

  if (true_or_false()) {
    palette <- rev(palette)
  }

  if (style == "alternating") {
    rect <- rect %>%
      dplyr::mutate(
        fill = ifelse(dplyr::row_number() %% 2 == 0, palette[[1]], palette[[2]])
      )
  } else if (style == "blocks") {
    n_blocks <- sample(2:10, 1)
    block_starts <- sample(1:nrow(rect), n_blocks, replace = TRUE) %>%
      unique() %>%
      sort()
    block_starts <- dplyr::tibble(id = block_starts) %>%
      dplyr::mutate(
        block_section = dplyr::row_number(),
        fill = ifelse(block_section %% 2 == 0, palette[[1]], palette[[2]])
      )

    rect <- rect %>%
      dplyr::mutate(id = dplyr::row_number()) %>%
      dplyr::left_join(block_starts, by = "id") %>%
      tidyr::fill(fill, block_section, .direction = "downup")

    rect <- rect %>%
      dplyr::group_by(block_section, fill, geom) %>%
      dplyr::summarise(
        xmin = min(xmin), xmax = max(xmax),
        ymin = min(ymin), ymax = max(ymax)
      ) %>%
      dplyr::ungroup()

    segment <- segment %>%
      dplyr::sample_frac(runif(1, 0.7, 0.95))
  }

  dplyr::bind_rows(
    rect,
    segment
  )
}
