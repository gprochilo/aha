#' @title Confidence interval, margin-of-error, t value, and p value for the
#' population Pearson's correlation coefficient based on summary data.
#'
#' @description The ci.pearson.r function computes a confidence interval (CI)
#' and confidence interval half-width (i.e., margin-of-error; hereby: moe) for
#' the population Pearson's correlation coefficient (r) based on summary
#' reports of data. If moe is greater than half the size of r the function will
#' alert you that r may have low precision.
#'
#' For completeness, the function also computes the exact t value and p
#' value for a given combination of r and sample size (N). This is useful
#' if the summary report has not included these values. The user may also
#' specify a smallest effect size of interest as the null value, if desired,
#' and compute these statistics based on the alternate hypothesis that the true
#' correlation is not equal to the null value (this defaults to a null of 0 if
#' no specified). If a nonzero null is used, a z test is performed and a z
#' statistic is reported instead of t.
#'
#' Important note: this function assumes the sample data are outlier free and
#' have a bivariate normal distribution. Ensure that these assumptions match
#' those of the reporting publication.
#'
#' Important note: also be aware that there will be some loss of precision when
#' using summary data as input. For example, if you input r values that have been
#' rounded to two decimal places, the computed confidence limit, t, and p values
#' may be slightly different from the true values. This difference, however, will
#' be relatively minor.
#'
#' @param r Sample Pearson's correlation coefficient value.
#' @param n Total sample size.
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
#' # Examine a reported Pearson correlation of 0.32 in a sample of 26 bivariate
#' # observations (two-sided test)
#' ci.pearson.r(r = 0.32, n = 26)
#'
#' # Examine a reported Pearson correlation of 0.32 in a sample of 26 bivariate
#' # observations (hypothesis: the true correlation is greater than 0)
#' ci.pearson.r(r = 0.32, n = 26, alternative = "greater")
#'
#' # Examine a reported Pearson correlation of 0.32 in a sample of 26 bivariate
#' # observations (hypothesis: the true correlation is less than 0)
#' ci.pearson.r(r = 0.32, n = 26, alternative = "less")
#'
#' # Examine a reported Pearson correlation of 0.32 in a sample of 26 bivariate
#' # observations (two-sided test) but suppress the print output
#' ci.pearson.r(r = 0.32, n = 26, silent = TRUE)
#'
#' # Examine a reported Pearson correlation of 0.32 in a sample of 26 bivariate
#' observations testing the null hypothesis that the true correlation is not
#' equal to 0.5
#' ci.pearson.r(r = 0.32, n = 26, null = 0.5)
#'
#' # Examine a reported Pearson correlation of 0.32 in a sample of 26 bivariate
#' observations and report an 80% confidence interval
#' ci.pearson.r(r = 0.32, n = 26, conf.level = 0.8)

ci.pearson.r <- function(r,
                         n,
                         null = 0,
                         alternative = "two.sided",
                         conf.level = 0.95,
                         silent = FALSE){

  # Define the following stop functions

  if(missing(r) || !is.numeric(r)  || length(r) > 1){
    stop("Please provide a single numeric value for 'r'",
         call. = FALSE)
  }

  if(r >= 1 || r <= -1  || length(r) > 1){
    stop("'r' must be a single number between -1 and 1",
         call. = FALSE)
  }

  if(!is.numeric(null)  || length(null) > 1){
    stop("Please provide a single numeric value for 'null'",
         call. = FALSE)
  }

  if(null >= 1 || null <= -1  || length(null) > 1){
    stop("'null' must be a single number between -1 and 1",
         call. = FALSE)
  }

  if(!is.numeric(conf.level)  || length(conf.level) > 1){
    stop("Please provide a single numeric value for 'conf.level'",
         call. = FALSE)
  }

  if(conf.level >= 1 || conf.level <= 0 || length(conf.level) > 1){
    stop("'conf.level' must be a single number between 0 and 1",
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

  if(missing(n) || !is.numeric(n)  || length(n) > 1 || n < 4){
    stop("Please provide a single numeric value for 'n' that is greater than 3",
         call. = FALSE)
  }

  # Define the Type I error rate for the lower and upper confidence limits

  alpha.lower = switch(alternative,
                       "two.sided" = (1-conf.level)/2,
                       "greater" = (1-conf.level),
                       "less" = 0)

  alpha.upper = switch(alternative,
                       "two.sided" = (1-conf.level)/2,
                       "greater" = 0,
                       "less" = (1-conf.level))


  # Compute the confidence limits based on the test performed
  # The quiet function will suppress the output of ci.cc

  es.ci = MBESS::ci.cc(r = r,
                       n = n,
                       alpha.lower = alpha.lower,
                       alpha.upper = alpha.upper,
                       conf.level = NULL)

  # The confidence limits are stored in vector position 1 and 3

  es.ci.LL = as.numeric(es.ci[1])
  es.ci.UL = as.numeric(es.ci[3])

  # Apply Fishers r to z transformation for both r and the null to compute the

  # If the null is non-zero we use a z test

  if(null != 0){

    rprime = 0.5*log(abs((1+r)/(1-r)))
    nullprime = 0.5*log(abs((1+null)/(1-null)))
    se = sqrt(1/(n-3))
    crit.z = qnorm(.025, lower.tail = FALSE)
    z = (rprime-nullprime)/se

    pval = switch(alternative,
                  "two.sided" = 2*min(pnorm(z), pnorm(z, lower.tail=FALSE)),
                  "greater" = pnorm(z, lower.tail = FALSE),
                  "less" = pnorm(z))

    # Define statistic = z for return function
    # type and pr are used in sprintf functions to clarify the output for the user

    statistic = z
    type = "z"
    pr = "z"

  }

  # If the null is zero we use a t test

  if(null == 0){

    df = n-2
    t =  r/(sqrt((1-r^2)/(n-2)))

    pval = switch(alternative,
                  "two.sided" = 2*min(pt(q = t, df = df), pt(q = t, df = df, lower.tail = FALSE)),
                  "greater" = pt(q = t, df = df, lower.tail = FALSE),
                  "less" = pt(q = t, df = df))

    # Define statistic = t for return function
    # type and pr are used in sprintf functions to clarify the output for the user

    statistic = t
    type = "t"
    pr = paste0("t(",df,")")

  }

  # Define a message to tell the user what test they have performed

  hyp = switch(alternative,
               "two.sided" = paste("alternative hypothesis: true correlation is not equal to",null),
               "greater" = paste("alternative hypothesis: true correlation is greater than", null),
               "less" = paste("alternative hypothesis: true correlation is less than", null)
  )

  # Compute the margin of error as the half-width of a two-sided confidence
  # interval. MoE is not computed for 1-sided tests and "NA" is returned.

  moe = switch(alternative,
               "two.sided" = (es.ci.UL-es.ci.LL)/2,
               "greater" = "NA",
               "less" = "NA")

  # Define a temporary variable for moe so it can be printed as a character
  # using sprintf. This is done because moe can either be a character or numeric

  temp.moe = switch(alternative,
                    "two.sided" = round(moe,2),
                    "greater" = moe,
                    "less" = moe)

  # Define a user-friendly output of the result in a manner that could be
  # copied straight into a report

  res = sprintf("r = %.2f, %.0f%% CI [%.2f, %.2f], MoE = %s, %s = %.2f, p = %.3f",
                r,
                conf.level*100,
                es.ci.LL,
                es.ci.UL,
                temp.moe,
                pr,
                statistic,
                pval)

  # If the user does not want to silence the printed output, print the following:

  if(silent == FALSE){

    cat(hyp,"\n")
    cat(res,"\n")
    cat("note: statistic is a", type, "value","\n")

    # Only result a statement on MoE if a two-sided test is performed

    if(is.numeric(moe) && moe > r/2){
      cat("note: r may have low precision\n\n", sep=" ")
    } else {cat("\n")}

    }

  # Return the following values

  return(list(r = r,
              n = n,
              null = null,
              statistic = statistic,
              pval = pval,
              ci = c(es.ci.LL, es.ci.UL),
              moe = moe))
}
