
<!-- README.md is generated from README.Rmd. Please edit that file -->

# aha

<!-- badges: start -->

# aha <img src='man/figures/logo.png' align="right" height="138.5" />

<!-- badges: end -->

The aha package is a set of statistical analysis and teaching tools that
can be used to clarify introductory statistical concepts. The aha
package is named after the interjection ‘aha\!’, which refers to the
human experience of understanding a previously incomprehensible problem
or concept.

The aha package currently contains one function:
`r_ci`.

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

## Summary

The `r_ci` function computes a confidence interval (CI) and confidence
interval half-width (i.e., margin-of-error; hereby: *MoE*) for the
population Pearson’s correlation coefficient (*r*) based on summary
reports of data. If the *MoE* is greater than half the size of *r* the
function will alert you that this *r* may have been estimated with low
precision.

For completeness, the function also computes the exact *t* value and *p*
value for a given combination of *r* and sample size (*N*). This is
useful if the summary report has not included these values.

*Important note*: this function assumes the sample data are outlier free
and have a bivariate normal distribution. Ensure that these assumptions
match those of the reporting publication. There will also be a small
loss of precision when using summary data to compute confidence
intervals and test statistics. For example, if you input an *r* value
that has been rounded to two decimal places, the *t* and *p* values may
be slightly different from the true values. These differences, however,
will be relatively minor.

### Example use

``` r
library(aha)
#> Warning: This package is under development
r_ci(r = 0.49, n = 17, conf_level = 0.95)
#> r = .49, 95% CI [0.01, 0.79], MoE = 0.39, t(15) = 2.18, p = .046
#> hypothesis:        true correlation is not equal to 0
#> test statistic:    t value
#> note:              r may have low precision
#> $r
#> [1] 0.49
#> 
#> $ci
#> [1] 0.01223732 0.78561898
#> 
#> $n
#> [1] 17
#> 
#> $statistic
#> [1] 2.177025
#> 
#> $p_value
#> [1] 0.04585992
#> 
#> $moe
#> [1] 0.3866908
```

### Print *P* Value Function plot

``` r
library(aha)
r_ci(r = 0.49, n = 17, conf_level = 0.95, print_plot = TRUE)
#> r = .49, 95% CI [0.01, 0.79], MoE = 0.39, t(15) = 2.18, p = .046
#> hypothesis:        true correlation is not equal to 0
#> test statistic:    t value
#> note:              r may have low precision
#> $r
#> [1] 0.49
#> 
#> $ci
#> [1] 0.01223732 0.78561898
#> 
#> $n
#> [1] 17
#> 
#> $statistic
#> [1] 2.177025
#> 
#> $p_value
#> [1] 0.04585992
#> 
#> $moe
#> [1] 0.3866908
```

<img src="man/figures/README-unnamed-chunk-3-1.png" width="80%" style="display: block; margin: auto;" />

# Warning

This package is under development. Backwards compatibility is **not
guaranteed**, and both functions and syntax may be **subject to change**
(or completely removed) in future updates. Exercise extreme caution if
you choose to use this package within an analysis pipeline.
