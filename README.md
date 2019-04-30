
<!-- README.md is generated from README.Rmd. Please edit that file -->

# aha

<!-- badges: start -->

# aha <img src='man/figures/logo.png' align="right" height="138.5" />

<!-- badges: end -->

The aha package is a set of statistical analysis and teaching tools
written by Guy A. Prochilo. Many of these tools have been written to
implement a parameter estimation approach to planning research and
analyzing data. These tools also serve a teaching purpose, and can be
used to clarify introductory statistical concepts. The aha package is
named after the interjection ‘aha\!’, which refers to the human
experience of understanding a previously incomprehensible problem or
concept.

The aha package currently only contains one function:
`ci.es`.

## Installation

<!-- You can install the released version of aha from [CRAN](https://CRAN.R-project.org) with: -->

<!-- ``` r -->

<!-- install.packages("aha") -->

<!-- ``` -->

You can install the development version of aha from
[GitHub](https://github.com/gprochilo) with:

``` r
# install.packages("devtools")
devtools::install_github("gprochilo/aha")
```

## Example

The `ci.es` function computes a confidence interval (CI) and confidence
interval half-width (i.e., margin-of-error; hereby: *MoE*) for Cohen’s
*d*<sub>z</sub>, Cohen’s *d*<sub>s</sub>, or Pearson’s *r*. The CI for
Cohen’s *d*<sub>z</sub> and Cohen’s *d*<sub>s</sub> is computed using an
exact method based on the noncentral t distribution. The CI for
Pearson’s *r* is computed using Fisher’s transformation. It also
returns *t* and *p* values for each combination of effect size and
sample size.

``` r
library(aha)
#> Welcome to the aha package!
ci.es(es = 0.6, N = 20, effect = "dz", conf = 0.95)
#> 2-tailed test: dz = 0.60, 95%CI [0.12, 1.07], MoE = 0.48, t(19) = 2.68, p = 0.015 
#> Note: This dz may have low precision
#> $es
#> [1] 0.6
#> 
#> $ci
#> [1] 0.115906 1.070834
#> 
#> $moe
#> [1] 0.477464
#> 
#> $df
#> [1] 19
#> 
#> $tval
#> [1] 2.683282
#> 
#> $pval
#> [1] 0.01470991
```

<!-- What is special about using `README.Rmd` instead of just `README.md`? You can include R chunks like so: -->

<!-- ```{r cars} -->

<!-- summary(cars) -->

<!-- ``` -->

<!-- You'll still need to render `README.Rmd` regularly, to keep `README.md` up-to-date. -->

<!-- You can also embed plots, for example: -->

<!-- ```{r pressure, echo = FALSE} -->

<!-- plot(pressure) -->

<!-- ``` -->

<!-- In that case, don't forget to commit and push the resulting figure files, so they display on GitHub! -->
