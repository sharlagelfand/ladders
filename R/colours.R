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
  butter = "#EBE0C6",
  mauve = "#d3a3b4"
)

colours_hsl <- plotwidgets::col2hsl(colours) %>%
  t() %>%
  dplyr::as_tibble() %>%
  dplyr::mutate(hex = colours, name = names(colours))

banned_combos <- list(
  c("butter", "steel"),
  c("butter", "grassgreen"),
  c("grassgreen", "steel"),
  c("grassgreen", "rhubarb"),
  c("butter", "rhubarb"),
  c("denim", "rhubarb"),
  c("blurple", "denim"),
  c("darkgreen", "darkteal"),
  c("grounds", "rhubarb"),
  c("rhubarb", "steel"),
  c("denim", "grounds"),
  c("grassgreen", "mauve"),
  c("mauve", "steel"),
  c("darkgreen", "mauve")
)

really_banned_combos <- list(
  c("darkteal", "denim"),
  c("blurple", "darkgreen"),
  c("darkgreen", "denim"),
  c("darkteal", "grounds"),
  c("blurple", "darkteal"),
  c("blurple", "grounds"),
  c("darkgreen", "rhubarb"),
  c("blurple", "grassgreen"),
  c("blurple", "darkgreen"),
  c("darkgreen", "denim"),
  c("grassgreen", "rhubarb")
)

banned_combos <- lapply(banned_combos, paste0, collapse = "")

really_banned_combos <- lapply(really_banned_combos, paste0, collapse = "")
