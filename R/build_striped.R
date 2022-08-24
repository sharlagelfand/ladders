#' @export
build_striped <- function(data, palette, palette_style) {
  rect <- dplyr::tibble(
    xmin = seq(data$xmin, data$xmax, length.out = sample(20:50, 1)),
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

  if (palette_style == "mono") {
    palette <- c("black", palette)

    if (true_or_false()) {
      palette <- rev(palette)
    }

    rect <- rect %>%
      dplyr::mutate(
        fill = ifelse(dplyr::row_number() %% 2 == 0, palette[[1]], palette[[2]])
      )
  } else if (palette_style == "duo") {
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
      block_starts <- sample(1:nrow(rect), n_blocks) %>%
        sort()
      block_starts <- dplyr::tibble(id = block_starts) %>%
        dplyr::mutate(fill = ifelse(dplyr::row_number() %% 2 == 0, palette[[1]], palette[[2]]))

      rect <- rect %>%
        dplyr::mutate(id = dplyr::row_number()) %>%
        dplyr::left_join(block_starts, by = "id") %>%
        tidyr::fill(fill, .direction = "downup")

      if (true_or_false()) {
        rect <- rect %>%
          dplyr::mutate(fill = ifelse(id %% 2 == 0, fill, "black"))
      } else {
        rect <- rect %>%
          dplyr::mutate(fill = ifelse(id %% 2 != 0, fill, "black"))
      }

      if (true_or_false()) {
        rect <- rect %>%
          dplyr::mutate(fill = ifelse(fill == "black", NA_character_, fill)) %>%
          tidyr::fill(fill, .direction = "downup")

        segment <- segment %>%
          dplyr::sample_frac(runif(1, 0.5, 0.7))
      }
    }
  }

  dplyr::bind_rows(
    rect,
    segment
  )
}
