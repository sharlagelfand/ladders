
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ladders

``` r
library(ladders)
library(patchwork)

set.seed(12345)

seeds <- sample(1:1000, 6)

lapply(seeds, ladder) %>%
  wrap_plots(ncol = 3)
```

<img src="man/figures/README-unnamed-chunk-2-1.png" width="100%" />
