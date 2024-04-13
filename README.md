
  - [Part 0. Proposal](#part-0-proposal)
  - [Part I. Work out functionality 🚧
    ✅](#part-i-work-out-functionality--)
      - [Try out compute function](#try-out-compute-function)
      - [Trying out Stat within plot](#trying-out-stat-within-plot)
  - [Pass stat to user-facing
    function](#pass-stat-to-user-facing-function)
  - [Part II. Packaging and documentation 🚧
    ✅](#part-ii-packaging-and-documentation--)
      - [Phase 1. Minimal working
        package](#phase-1-minimal-working-package)
      - [Phase 3: Settling and testing 🚧
        ✅](#phase-3-settling-and-testing--)
      - [Phase 4. Promote to wider audience… 🚧
        ✅](#phase-4-promote-to-wider-audience--)
      - [Phase 5: Harden/commit: Submit to CRAN/RUniverse 🚧
        ✅](#phase-5-hardencommit-submit-to-cranruniverse--)
  - [Appendix: Reports, Environment](#appendix-reports-environment)
      - [Description file complete? 🚧 ✅](#description-file-complete--)
      - [Environment 🚧 ✅](#environment--)
      - [`devtools::check()` report](#devtoolscheck-report)
      - [Package directory file tree](#package-directory-file-tree)

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

# Part 0. Proposal

Proposing the {ggcallout} package\! 🦄
<!-- (typical package introduction write up; but actually aspirational) -->

The goal of {ggcallout} is to make callouts easier. Maybe just a proof
of concept and we’ll just look at scatterplots to start.

Without the package, we live in the effort-ful world that follows 🏋:
It’s so effortful, I’m not gonna save filling this out for later…

``` r
x <- 4

2*x
#> [1] 8
```

With the {ggcallout} package, we’ll live in a different world (🦄 🦄 🦄)
where the task is a snap 🫰:

Proposed API:

``` 

library(ggcallout)

gapminder::gapminder |>
  filter(year == 2002) |>
ggplot() + 
  aes(gdpPercap, lifeExp, id = country) +
  geom_point(color = "darkgray") + 
  # labels as 'Norway' with default link length and padding
  geom_labellink(which_id = "Norway",
                 label_direction = -120) +
  # label is specified by the user               
  geom_labellink(which_id = "Brazil",
                 label = "People want to\nknow about Brazil",
                 label_direction = -70,
                 prop_range = .2) + 
```

# Part I. Work out functionality 🚧 ✅

Here is a function that will do some work…

``` r
readme2pkg::chunk_to_r("StatLabellink")
```

``` r
compute_labellink <- function(data, scales, label_direction = 180 + 45, prop_range = .1, prop_pointer_pad = .175, hjust = NULL, vjust = NULL, which_index = NULL, which_id = NULL){
  
  if(is.null(data$id)){data$id <- "hello world"}
  if(is.null(which_index)){which_index <- which(data$id == which_id)}
  
  data$default_label <- data$id
  
  xmean <- mean(data$x)
  ymean <- mean(data$y)
  
  range_x <- diff(range(data$x))
  range_y <- diff(range(data$y)) # look at range of plot?
  xdir <- cos(pi*label_direction/180)
  ydir <- sin(pi*label_direction/180)
  xpush <- range_x * prop_range * xdir
  ypush <- range_y * prop_range * ydir
  
  more_x_than_y <- abs(xdir) > abs(ydir)
  
  if(is.null(hjust)){hjust <- ifelse(more_x_than_y, sign(xdir) != 1, .5)}
  if(is.null(vjust)){vjust <- ifelse(more_x_than_y, .5, sign(ydir) != 1)}

  data |> 
    dplyr::mutate(x = x + xpush) |>
    dplyr::mutate(y = y + ypush) |>
    dplyr::mutate(xend = .data$x - xpush*(1-prop_pointer_pad)) |>
    dplyr::mutate(yend = .data$y - ypush*(1-prop_pointer_pad)) |>
    dplyr::mutate(hjust = hjust) |>
    dplyr::mutate(vjust = vjust) |> 
    dplyr::slice(which_index)
  
}

StatLabellink <- ggplot2::ggproto("Labellink",
                         ggplot2::Stat,
                         compute_panel = compute_labellink,
                         default_aes = 
                           ggplot2::aes(label = ggplot2::after_stat(default_label)))
```

## Try out compute function

``` r
library(tidyverse)
#> ── Attaching core tidyverse packages ─────────────────── tidyverse 2.0.0.9000 ──
#> ✔ dplyr     1.1.0          ✔ readr     2.1.4     
#> ✔ forcats   1.0.0          ✔ stringr   1.5.0     
#> ✔ ggplot2   3.4.4.9000     ✔ tibble    3.2.1     
#> ✔ lubridate 1.9.2          ✔ tidyr     1.3.0     
#> ✔ purrr     1.0.1          
#> ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
#> ✖ dplyr::filter() masks stats::filter()
#> ✖ dplyr::lag()    masks stats::lag()
#> ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors
gapminder::gapminder |>
  filter(year == 2002) |> 
  select(id = country, x = lifeExp, y = gdpPercap) |>
  compute_labellink(which_id = "Chile")
#> # A tibble: 1 × 8
#>   id        x     y default_label  xend   yend hjust vjust
#>   <fct> <dbl> <dbl> <fct>         <dbl>  <dbl> <lgl> <dbl>
#> 1 Chile  74.8 7636. Chile          77.3 10229. TRUE    0.5

gapminder::gapminder |>
  filter(year == 2002) |> 
  select(id = country, x = lifeExp, y = gdpPercap) |>
  compute_labellink(which_index = 3)
#> # A tibble: 1 × 8
#>   id          x     y default_label  xend  yend hjust vjust
#>   <fct>   <dbl> <dbl> <fct>         <dbl> <dbl> <lgl> <dbl>
#> 1 Algeria  68.0 2145. Algeria        70.5 4738. TRUE    0.5

gapminder::gapminder |>
  filter(year == 2002) |> 
  select(x = lifeExp, y = gdpPercap) |>
  compute_labellink(which_index = 3)
#> Warning: Unknown or uninitialised column: `id`.
#> # A tibble: 1 × 8
#>       x     y id          default_label  xend  yend hjust vjust
#>   <dbl> <dbl> <chr>       <chr>         <dbl> <dbl> <lgl> <dbl>
#> 1  68.0 2145. hello world hello world    70.5 4738. TRUE    0.5
```

## Trying out Stat within plot

``` r
gapminder::gapminder |>
  filter(year == 2002) |> 
  ggplot() + 
  aes(id = country, x = lifeExp, y = gdpPercap) + 
  geom_point() + 
  layer("label", "labellink", position = "identity",
        params = list(which_id = "Chile")) + 
  layer("segment", "labellink", position = "identity",
        params = list(which_id = "Chile")) + 
  scale_x_log10()
```

![](README_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

``` r

ggplot(cars) + 
  aes(speed, dist) + 
  geom_point() + 
  layer("segment", 
        "labellink", 
        position = "identity", 
        # data = cars[23,], 
        params = list(which_index = 23, arrow = 
                        arrow(ends = "last", 
                              length = unit(.1, "inches"), 
                              type = "closed"))) +
  layer("label", 
        "labellink", 
        position = "identity",  
        # data = cars[23,],
        params = list(which_index = 23, 
                      label = "let me tell you about this point" |> str_wrap(15),
                      alpha = 0,
                      lineheight = .8,
                      label.size = 0,
                      label.padding = unit(0.7, "lines"))) + 
  layer("point",
        "labellink",
        position = "identity",
        # data = cars[23,],
        params = list(which_index = 23, 
                      color = "red"))
```

![](README_files/figure-gfm/unnamed-chunk-5-2.png)<!-- -->

# Pass stat to user-facing function

``` r
readme2pkg::chunk_to_r("geom_labellink")
```

``` r
geom_labellink <- function(  mapping = NULL,
  data = NULL,
  position = "identity",
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE, ...){

  
  list( 
    
    ggplot2::layer("segment", 
        "labellink", 
        position = position, 
        data = data, 
        mapping = mapping,
            show.legend = show.legend,
    inherit.aes = inherit.aes,
        params = list(arrow = 
                        arrow(ends = "last", 
                              length = unit(.1, "inches"), 
                              type = "closed"), na.rm = na.rm,
                      ...)),
    
  ggplot2::layer("label", 
        "labellink", 
        position = position, 
        data = data, 
        mapping = mapping, 
            show.legend = show.legend,
    inherit.aes = inherit.aes,
        params = list(
                      alpha = 0,
                      lineheight = .8,
                      label.size = 0,
                      label.padding = unit(0.4, "lines"), 
                      na.rm = na.rm,
                      ...)) 
  )

  
}
```

# Part II. Packaging and documentation 🚧 ✅

## Phase 1. Minimal working package

### Bit A. Created package archetecture, running `devtools::create(".")` in interactive session. 🚧 ✅

``` r
devtools::create(".") # Bit 1. 1X
### Bit 2a: dependencies to functions using '::' syntax to pkg functions 
usethis::use_package("ggplot2") # Bit 2b: document dependencies
usethis::use_package("dplyr")
# Bit 3: send code chunk with function to R folder
devtools::check(pkg = ".")  # Bit 4: check that package is minimally viable
devtools::install(pkg = ".", upgrade = "never") # Bit 5: install package locally
usethis::use_lifecycle_badge("experimental") # Bit 6: add lifecycle badge
# Bit 7 (below): Write traditional readme
# Bit 8: Compile readme
# Bit 9: Push to github
# Bit 10: listen and iterate
```

### Bit 7. Write traditional README that uses built package (also serves as a test of build). 🚧 ✅

The goal of the {ggcallout} package is to …

Install package with:

    remotes::install_github("EvaMaeRey/ggcallout")

Once functions are exported you can remove go to two colons, and when
things are are really finalized, then go without colons (and rearrange
your readme…)

``` r
library(tidyverse)
library(ggcallout)  ##<< change to your package name here
gapminder::gapminder |> 
  filter(year == 2002) |>
  ggplot() + 
  aes(x = gdpPercap, y = lifeExp, id = country) + 
  geom_point(color = "darkgrey") + 
  ggcallout:::geom_labellink(which_id = "Chile",
                             label_direction = 45) + 
  ggcallout:::geom_labellink(which_id = "Brazil",
                             label_direction = -65,
                             label = "Brazil is a pretty\n interesting case")
#> Warning in ggcallout:::geom_labellink(which_id = "Brazil", label_direction =
#> -65, : Ignoring unknown parameters: `label`
```

![](README_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

``` r

last_plot() + 
  scale_x_log10()
```

![](README_files/figure-gfm/unnamed-chunk-8-2.png)<!-- -->

## Phase 3: Settling and testing 🚧 ✅

### Bit A. Added a description and author information in the [DESCRIPTION file](https://r-pkgs.org/description.html) 🚧 ✅

### Bit B. Added [roxygen skeleton](https://r-pkgs.org/man.html)? 🚧 ✅

### Bit C. Chosen a [license](https://r-pkgs.org/license.html)? 🚧 ✅

``` r
usethis::use_mit_license()
```

### Bit D. Settle on [examples](https://r-pkgs.org/man.html#sec-man-examples). Put them in the roxygen skeleton and readme. 🚧 ✅

### Bit E. Written formal [tests](https://r-pkgs.org/testing-basics.html) of functions and save to test that folders 🚧 ✅

That would look like this…

``` r
library(testthat)

test_that("calc times 2 works", {
  expect_equal(times_two(4), 8)
  expect_equal(times_two(5), 10)
  
})
```

``` r
readme2pkg::chunk_to_tests_testthat("test_calc_times_two_works")
```

### Bit F. Check again. Addressed notes, warnings and errors. 🚧 ✅

``` r
devtools::check(pkg = ".")
```

## Phase 4. Promote to wider audience… 🚧 ✅

### Bit A. Package website built? 🚧 ✅

### Bit B. Package website deployed? 🚧 ✅

## Phase 5: Harden/commit: Submit to CRAN/RUniverse 🚧 ✅

# Appendix: Reports, Environment

## Description file complete? 🚧 ✅

``` r
readLines("DESCRIPTION")
```

## Environment 🚧 ✅

Here I just want to print the packages and the versions

``` r
all <- sessionInfo() |> print() |> capture.output()
all[11:17]
#> [1] ""                                                                         
#> [2] "attached base packages:"                                                  
#> [3] "[1] stats     graphics  grDevices utils     datasets  methods   base     "
#> [4] ""                                                                         
#> [5] "other attached packages:"                                                 
#> [6] " [1] ggcallout_0.0.0.9000 lubridate_1.9.2      forcats_1.0.0       "      
#> [7] " [4] stringr_1.5.0        dplyr_1.1.0          purrr_1.0.1         "
```

## `devtools::check()` report

``` r
devtools::check(pkg = ".")
```

## Package directory file tree

``` r
fs::dir_tree(recurse = T)
#> .
#> ├── DESCRIPTION
#> ├── NAMESPACE
#> ├── R
#> │   ├── StatLabellink.R
#> │   └── geom_labellink.R
#> ├── README.Rmd
#> ├── README.md
#> ├── README_files
#> │   └── figure-gfm
#> │       ├── unnamed-chunk-5-1.png
#> │       ├── unnamed-chunk-5-2.png
#> │       ├── unnamed-chunk-8-1.png
#> │       └── unnamed-chunk-8-2.png
#> ├── ggcallout.Rproj
#> ├── man
#> └── readme2pkg.template.Rproj
```
