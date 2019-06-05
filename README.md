
<!-- README.md is generated from README.Rmd. Please edit that file -->

# aha

<!-- badges: start -->

# aha <img src='man/figures/logo.png' align="right" height="138.5" />

<!-- badges: end -->

The aha package is a set of statistical analysis and teaching tools
written by Guy A. Prochilo. These tools primarily serve a teaching
purpose, and can be used to clarify introductory statistical concepts.
The aha package is named after the interjection ‘aha\!’, which refers to
the human experience of understanding a previously incomprehensible
problem or concept.

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
for the population Pearson’s correlation coefficient based on summary
reports of data. For completeness, it also computes the exact *t* value
and *p* value for a given combination of correlation magnitude (*r*) and
sample size (*N*). This is useful if the reporting publication has not
included these values.

Note that this function assumes the sample data are outlier free and
have a bivariate normal distribution. Ensure that these assumptions
match those of the reporting publication.

``` r
library(aha)
#> Welcome to the aha package!
# ci.pearson.r(r = 0.49, n = 17, alternative = "two.sided")
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
