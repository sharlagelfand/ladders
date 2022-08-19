#' @export
option_from_noise <- function(data, options) {
  data %>%
    # bin into groups and choose colour from there
    dplyr::mutate(
      option = dplyr::ntile(noise, length(options)),
      option = options[option]
    )
}
