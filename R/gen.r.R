#' @title Generate paired data that gives a specific Pearson correlation
#'
#' @description The gen.r function generates paired (bivariate) data of a
#' given sample size with a specific Pearson correlation coefficient. Data are
#' drawn from a bivariate normal distribution with no outliers.
#'
#' @param r Desired Pearson's correlation coefficient value.
#' @param n Total sample size.
#' @param mean.x Desired mean of x vector
#' @param mean.y Desired mean of y vector
#' @param seed Set seed = TRUE for reproducible datasets
#' @param silent A logical indicating whether you want the function to return
#' print output or suppress print output.
#'
#' @keywords Pearson's r, correlation coefficient, data generation
#'
#' @export
#' @examples
#' # Generate 26 bivariate observations that give a Pearson correlation of 0.32
#' gendat.r(r = 0.32, n = 26)

gen.r <- function(r,
                  n,
                  mean.x = 0,
                  mean.y = 0,
                  seed = TRUE,
                  silent = FALSE){

  # If seed = TRUE set the seed for reproducible data generation

  if(seed == TRUE){set.seed(2)}

  # Generate data from a bivariate normal distribution with a specific Pearson
  # correlation

  dat = MASS::mvrnorm(n=n,
                      mu=c(mean.x, mean.y),
                      Sigma=matrix(c(1, r, r, 1), nrow=2),
                      empirical=TRUE)

  # Return data as a dataframe

  dat = as.data.frame(dat)
  colnames(dat) = c("x", "y")

  # Print a statement on the data regarding r, confidence limits, and difference
  # from a null hypothesis of zero

  if(silent == FALSE){aha::test.pearson.r(x = dat$x, y = dat$y)}

  return(dat)

}
