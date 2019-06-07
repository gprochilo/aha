#' @title Test for Correlation Between Bivariate Samples Using Pearson Correlation
#' Analysis.
#'
#' @description The test.pearson.r function performs a Pearson correlation analysis
#' between bivariate samples. You may specify a specific null hypothesis (defaults
#' to zero). It returns a test statistic (t for a zero null; z for a nonzero null),
#' p value, confidence interval, and margin-of-error (confidence interval half-width).
#' Supports both 1-sided and 2-sided tests, and can report confidence intervals of
#' different coverage if specified.
#'
#' Note: this function assumes the sample data are outlier free and have a
#' bivariate normal distribution.
#'
#' @param x,y numeric vectors of data values. x and y must have the same length.
#' @param dataset dataframe that stores x and y vectors
#' @param null The null hypothesis value for the Pearson correlation test.
#' Defaults to 0 if not specified.
#' @param alternative Character string specifying the alternative hypothesis,
#' must be one of "two.sided" (default), "greater" or "less".
#' @param conf.level confidence level of the interval.
#' @param silent A logical indicating whether you want the function to return
#' print output or suppress print output.
#'
#' @keywords Pearson's r, correlation coefficient, confidence intervals, nonzero
#' null
#'
#' @export
#' @examples
#' test.pearson.r(x = X, y = Y, dataset = df, null = 0.2)


test.pearson.r <- function(x,
                           y,
                           dataset,
                           null = 0,
                           alternative = "two.sided",
                           conf.level = 0.95,
                           silent = FALSE){

  if(missing(x) || missing(y) || missing(dataset)){
    stop("Please input x and y variable names and dataset where they are stored",
         call. = FALSE)
  }

  # Capture input without evaluation

  args <- as.list(match.call())

  # Evaluate x

  x = eval(args$x, dataset)
  if(is.character(x)){x = get(x, dataset)}

  # Evaluate y

  y = eval(args$y, dataset)
  if(is.character(y)){y = get(y, dataset)}

  # Stop warning

  if (!is.numeric(x) || !is.numeric(y)){
    stop("'x' and 'y' must be numeric")}

  # Define data frame of x and y and remove na pairs

  df = stats::na.omit(data.frame(x,y))

  # Define r as pearson correlation of x and y

  r = stats::cor(x,y)

  if(r == 1){
    stop("'x' and 'y' are perfectly correlated",
    call. = FALSE)
  }

  # Define res using the ci.pearson.r function

  res = aha::ci.pearson.r(r = r,
                           n = length(x),
                           null = null,
                           alternative = alternative,
                           conf.level = conf.level,
                           silent = silent)
  return(res)
}
