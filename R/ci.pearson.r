#' @title Confidence interval, margin-of-error, t value, and p value for the
#' population Pearson's correlation coefficient
#'
#' @description The `ci.pearson.r` function computes a confidence interval (CI)
#' and confidence interval half-width (i.e., margin-of-error; hereby: _MoE_) for
#' the population Pearson's correlation coefficient (_r_) based on summary
#' reports of data. If the _MoE_ is greater than half the size of _r_ the
#' function will alert you that this _r_ may have been estimated with low
#' precision.
#'
#' For completeness, the function also computes the exact _t_ value and _p_
#' value for a given combination of (_r_) and sample size (_N_). This is useful
#' if the summary report has not included these values.
#'
#' _Note_: this function assumes the sample data are outlier free and have a
#' bivariate normal distribution. Ensure that these assumptions match those of
#' the reporting publication.
#'
#' @param r Sample Pearson's correlation coefficient value.
#' @param n Total sample size.
#' @param alternative Character string specifying the alternative hypothesis,
#' must be one of "two.sided" (default), "greater" or "less".
#' @param silent A logical indicating whether you want the function to return
#' print output or suppress print output.
#'
#' @keywords Pearson's r, correlation coefficient, confidence intervals
#'
#' @export
#' @examples
#' # Examine a reported Pearson correlation of 0.32 in a sample of 26 bivariate
#' # observations (two-sided test)
#' ci.pearson.r(r = 0.32, n = 26)
#'
#' # Examine a reported Pearson correlation of 0.32 in a sample of 26 bivariate
#' # observations (hypothesis: the true correlation is greater than 0)
#' ci.pearson.r(r = 0.49, n = 17, alternative = "greater")
#'
#' # Examine a reported Pearson correlation of 0.32 in a sample of 26 bivariate
#' # observations (hypothesis: the true correlation is less than 0)
#' ci.pearson.r(r = 0.49, n = 17, alternative = "less")
#'
#' # Examine a reported Pearson correlation of 0.32 in a sample of 26 bivariate
#' # observations (two-sided test) but suppress the print output
#' ci.pearson.r(r = 0.49, n = 17, silent = TRUE)

ci.pearson.r <- function(r,
                         n,
                         alternative = "two.sided",
                         silent = FALSE){

  if(missing(r) || !is.numeric(r)){
    stop("Please provide a numeric value for 'r'",
         call. = FALSE)
  }

  if(r >= 1 || r <= -1){
    stop("'r' must be a single number between -1 and 1",
         call. = FALSE)
  }

  if(alternative != "two.sided" && alternative != "greater" && alternative != "less"){
    stop("'alternative' may be specified as one of: 'two.sided', 'greater', or 'less'",
         call. = FALSE)
  }

  if(!is.logical(silent)){
    stop("'silent' can be TRUE (T) or FALSE (F)'",
         call. = FALSE)
  }

  if(n < 4){
    stop("'n' must be greater than 3",
         call. = FALSE)
  }

  # Define a function to suppress the print output of CRAN package functions
  # that do not allow you to turn off the output

  quiet <- function(x){

    sink(tempfile())
    on.exit(sink())
    invisible(force(x))
  }

  # Define the Type I error rate for the lower and upper confidence limits

  alpha.lower = switch(alternative,
                       "two.sided" = 0.025,
                       "greater" = 0.05,
                       "less" = 0)

  alpha.upper = switch(alternative,
                       "two.sided" = 0.025,
                       "greater" = 0,
                       "less" = 0.05)

  # Define degrees of freedom

  df = n-2

  # Compute the t value for r and n

  tval = psych::r2t(rho = r, n = n)

  # Compute the p value for the test performed

  pval = switch(alternative,
                "two.sided" = 2*min(pt(tval, df), pt(tval, df, lower.tail=FALSE)),
                "greater" = pt(tval,df = df, lower.tail = FALSE),
                "less" = pt(tval,df = df)
  )

  # Compute the confidence limits based on the test performed
  # The quiet function will suppress the output of ci.cc

  es.ci = quiet(MBESS::ci.cc(r = r,
                             n = n,
                             alpha.lower = alpha.lower,
                             alpha.upper = alpha.upper,
                             conf.level = NULL))

  # The confidence limits are stored in vector position 1 and 3

  es.ci.LL = as.numeric(es.ci[1])
  es.ci.UL = as.numeric(es.ci[3])

  # Compute the margin of error as the half-width of a two-sided confidence
  # interval. MoE cannot be computed for 1-sided tests and "NA" is returned.

  moe = switch(alternative,
               "two.sided" = (es.ci.UL-es.ci.LL)/2,
               "greater" = "NA",
               "less" = "NA")


  # Define the alternative hypothesis for the test performed

  hyp = switch(alternative,
               "two.sided" = "alternative hypothesis: true correlation is not equal to 0",
               "greater" = "alternative hypothesis: true correlation is greater than 0",
               "less" = "alternative hypothesis: true correlation is less than 0")

  # Define a clean output of the results

  res = sprintf("r(%.0f) = %.2f, 95%% CI [%.2f, %.2f], t = %.2f, p = %.3f",
                df,
                r,
                es.ci.LL,
                es.ci.UL,
                tval,
                pval)

  # If the user has not specified to suppress print output, print the following:

  if(silent == FALSE){

    cat(hyp,"\n")

    cat(res,"\n")

    # Only result a statement on MoE if a two-sided test is performed

    if(is.numeric(moe) && moe > r/2){
      cat("Note: This r may have been estimated with low precision\n\n", sep=" ")
    } else {cat("\n")}
    }

  return(list(r = r,
              n = n,
              ci = c(es.ci.LL, es.ci.UL),
              moe = moe,
              df = df,
              tval = tval,
              pval = pval))
}

