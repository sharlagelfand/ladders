colours <- c(
  darkgreen = "#297045",
  darkteal = "#204E4A",
  grassgreen = "#83B15D",
  rhubarb = "#D34F73",
  linen = "#F7EDE2",
  denim = "#3D518C",
  steel = "#ACBED8",
  grounds = "#1C2826",
  blurple = "#39375B",
  butter = "#D7AF60"
)

colours_hsl <- plotwidgets::col2hsl(colours) %>%
  t() %>%
  dplyr::as_tibble() %>%
  dplyr::mutate(hex = colours, name = names(colours))

banned_combos <- list(
  c("butter", "steel"),
  c("darkgreen", "denim"),
  c("darkteal", "denim"),
  c("darkteal", "grounds"),
  c("butter", "grassgreen"),
  c("blurple", "darkteal"),
  c("grassgreen", "steel"),
  c("blurple", "grounds"),
  c("butter", "rhubarb"),
  c("denim", "rhubarb"),
  c("darkgreen", "rhubarb")
)

banned_combos <- lapply(banned_combos, paste0, collapse = "")
