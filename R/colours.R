colours <- c(
  darkgreen = "#297045",
  darkteal = "#204E4A",
  grassgreen = "#81C14B",
  rhubarb = "#D34F73",
  linen = "#F7EDE2",
  denim = "#3D518C",
  steel = "#ACBED8",
  grounds = "#1C2826",
  blurple = "#39375B",
  butter = "#E9B44C"
)

# colours <- plotwidgets::col2hsl(colours)

banned_combos <- list(
  c("butter", "steel"),
  c("darkgreen", "denim"),
  c("darkteal", "denim"),
  c("butter", "grassgreen"),
  c("blurple", "darkteal"),
  c("grassgreen", "steel")
)

banned_combos <- lapply(banned_combos, paste0, collapse = "")
