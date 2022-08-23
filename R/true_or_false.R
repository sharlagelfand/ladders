true_or_false <- function(prob = c(1, 1)) {
  sample(c(TRUE, FALSE), 1, prob = prob)
}
