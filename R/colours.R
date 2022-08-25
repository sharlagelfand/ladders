colours <- c(
  darkgreen = "#297045",
  darkteal = "#204E4A",
  grassgreen = "#81C14B",
  flame = "#E16036",
  linen = "#F7EDE2",
  denim = "#3D518C",
  steel = "#ACBED8",
  grounds = "#1C2826",
  blurple = "#39375B",
  butter = "#E9B44C"
)

banned_combos <- list(
  c("butter", "steel"),
  c("darkgreen", "denim"),
  c("darkgreen", "flame"),
  c("darkteal", "denim"),
  c("butter", "grassgreen"),
  c("blurple", "darkteal"),
  c("grassgreen", "steel")
)

banned_combos <- lapply(banned_combos, paste0, collapse = "")
