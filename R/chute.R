#' "Chute" a ladder into a file
#'
#' @param ladder From \code{\link{ladder}}
#' @param file Yknow, a filename. png pls
#'
#' @export
chute <- function(ladder, file) {
  ggplot2::ggsave(
    filename = file, plot = ladder,
    width = 8.5,
    height = 11,
    dpi = 300,
    background = "white"
  )
}
