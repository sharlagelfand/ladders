#' "Chute" a ladder into a file, read it back, make it noisy, and write it again
#'
#' @param ladder From \code{\link{ladder}}
#' @param file Yknow, a filename. png pls
#'
#' @export
chute <- function(ladder, file) {

  xlim <- ggplot2::layer_scales(ladder)$x$range$range
  ylim <- ggplot2::layer_scales(ladder)$y$range$range

  ggplot2::ggsave(
    filename = file, plot = ladder,
    width = xlim[[2]] - xlim[[1]],
    height = ylim[[2]] - ylim[[1]],
    dpi = 300
  )

  # img <- magick::image_read(file)

  # img <- img %>%
    # image_partial_noise()

  # magick::image_write(img, file)
}
