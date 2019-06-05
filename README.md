
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

The aha package currently only contains one function:
`ci.pearson.r`.

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

The `ci.pearson.r` function computes a confidence interval (CI) and
confidence interval half-width (i.e., margin-of-error; hereby: *MoE*)
for the population Pearson’s correlation coefficient (*r*) based on
summary reports of data. If the *MoE* is greater than half the size of
*r* the function will alert you that this *r* may have been estimated
with low precision.

For completeness, the function also computes the exact *t* value and *p*
value for a given combination of (*r*) and sample size (*N*). This is
useful if the summary report has not included these values.

*Note*: this function assumes the sample data are outlier free and have
a bivariate normal distribution. Ensure that these assumptions match
those of the reporting publication.

``` r
library(aha)
#> aha!
ci.pearson.r(r = 0.49, n = 17, alternative = "two.sided")
#> alternative hypothesis: true correlation is not equal to 0 
#> r(15) = 0.49, 95% CI [0.01, 0.79], t = 2.18, p = 0.046 
#> Note: This r may have been estimated with low precision
#> $r
#> [1] 0.49
#> 
#> $n
#> [1] 17
#> 
#> $ci
#> [1] 0.01223732 0.78561898
#> 
#> $moe
#> [1] 0.3866908
#> 
#> $df
#> [1] 15
#> 
#> $tval
#> [1] 2.177025
#> 
#> $pval
#> [1] 0.04585992
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
