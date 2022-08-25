#' @export
#' @example
#' chutes_and_ladders(1:101)
chutes_and_ladders <- function(seeds, out_folder = "outputs") {
  lapply(
    seeds,
    chute_and_ladder,
    out_folder
  )
}

chute_and_ladder <- function(seed, out_folder) {
  ladder(seed) %>%
    chute(here::here(out_folder, paste0(seed, ".jpg")))
}
