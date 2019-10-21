#' @title Confidence interval, margin-of-error, *t* value, and *p* value for the
#' population Pearson's correlation coefficient (*r*) based on summary data.
#'
#' @description The r_ci function computes a confidence interval (CI) and
#' confidence interval half-width (i.e., margin-of-error; hereby: *MoE*) for
#' the population Pearson's correlation coefficient (*r*) based on summary
#' reports of data. If *MoE* is greater than half the size of *r* the function
#' will alert the user that *r* may have low precision.
#'
#' For completeness, the function also computes the exact *t* value and *p*
#' value for a given combination of *r* and sample size (*N*). This is useful
#' if the summary report has not included these values, or has only included
#' approximate *p* values (e.g., *p* < .05).
#'
#' *Important note*: this function assumes the sample data are outlier free and
#' have a bivariate normal distribution. Ensure that these assumptions match
#' those of the reporting publication. There will also be a small loss of precision
#' when using summary data to compute confidence intervals and test statistics.
#' For example, if you input an *r* value that has been rounded to two decimal
#' places, the *t* and *p* values may be slightly different from the true values.
#' These differences, however, will be relatively minor.
#'
#'
#' @param r Sample Pearson's correlation coefficient value.
#' @param n Sample size
#' @param alternative Character string specifying the alternative hypothesis,
#' must be one of "two_sided" (default), "greater" or "less".
#' @param conf_level Confidence level of the interval.
#' @param silent A logical indicating whether to suppress message output: FALSE (default) or TRUE.
#'
#' @keywords Pearson's r, correlation coefficient, confidence intervals
#'
#' @export
#' @examples
#' # Compute statistics for a Pearson correlation of 0.32 in a sample of 26 bivariate
#' # observations (two-sided test)
#'  r_ci(r = 0.32, n = 26)
#'
#' # Compute statistics for a Pearson correlation of 0.32 in a sample of 26 bivariate
#' # observations using the hypothesis that the true correlation is greater than 0:
#' r_ci(r = 0.32, n = 26, alternative = "greater")
#'
#' # Compute statistics for a Pearson correlation of 0.32 in a sample of 26 bivariate
#' # observations using the hypothesis that the true correlation is less than 0:
#' r_ci(r = 0.32, n = 26, alternative = "less")

r_ci <- function(r,
                 n,
                 alternative = "two_sided",
                 conf_level = 0.95,
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

  if(!is.numeric(conf_level)  || length(conf_level) > 1){
    stop("Please provide a single numeric value for 'conf_level'",
         call. = FALSE)
  }

  if(conf_level >= 1 || conf_level <= 0 || length(conf_level) > 1){
    stop("'conf_level' must be a single number between 0 and 1",
         call. = FALSE)
  }

  if(alternative != "two_sided" && alternative != "greater" && alternative != "less"){
    stop("'alternative' may be specified as one of: 'two_sided', 'greater', or 'less'",
         call. = FALSE)
  }

  if(missing(n) || !is.numeric(n)  || length(n) > 1 || n < 4){
    stop("Please provide a single numeric value for 'n' that is greater than 3",
         call. = FALSE)
  }

  # Define the Type I error rate for the lower and upper confidence limits

  alpha_lower = switch(alternative,
                       "two_sided" = (1 - conf_level) / 2,
                       "greater" = (1 - conf_level),
                       "less" = 0)

  alpha_upper = switch(alternative,
                       "two_sided" = (1 - conf_level) / 2,
                       "greater" = 0,
                       "less" = (1 - conf_level))


  # Compute the confidence limits based on the test performed
  # The quiet function will suppress the output of ci.cc

  ci = MBESS::ci.cc(r = r,
                    n = n,
                    alpha.lower = alpha_lower,
                    alpha.upper = alpha_upper,
                    conf.level = NULL)

  # The confidence limits are stored in list position 1 and 3

  ci_LL = as.numeric(ci[1])
  ci_UL = as.numeric(ci[3])

  # Compute t and p values

  df = n - 2
  t_value =  r / (sqrt((1 - r^2)/(n - 2)))
  p_value = switch(alternative,
                   "two_sided" = 2 * min(pt(q = t_value, df = df),
                                         pt(q = t_value, df = df, lower.tail = FALSE)),
                   "greater" = pt(q = t_value, df = df, lower.tail = FALSE),
                   "less" = pt(q = t_value, df = df))

  # Define a message to tell the user what test they have performed

  hyp = switch(alternative,
               "two_sided" = paste("alternative hypothesis: true correlation is not equal to 0"),
               "greater" = paste("alternative hypothesis: true correlation is greater than 0"),
               "less" = paste("alternative hypothesis: true correlation is less than 0")
  )

  # Compute the margin of error as the half-width of a two-sided confidence
  # interval. MoE is not computed for 1-sided tests and "NA" is returned.

  moe = switch(alternative,
               "two_sided" = (ci_UL - ci_LL) / 2,
               "greater" = "NA",
               "less" = "NA")

  # Define a user-friendly output of the result to return as a message

  res = sprintf("r = %s, %.0f%% CI [%.2f, %.2f], MoE = %s, t(%.0f) = %.2f, p = %s",
                substring(format(r, nsmall = 2), 2),
                conf_level * 100,
                ci_LL,
                ci_UL,
                ifelse(is.character(moe), moe, round(moe, 2)),
                df,
                t_value,
                ifelse(p_value < .001, "<.001", substring(format(round(p_value, 3), nsmall = 3), 2)))

  # Return the hypothesis used and user-friendly result as messages if silent = FALSE

  if(silent == FALSE){

  message(hyp)
  message(res)

  if(is.numeric(moe) & moe > r / 2){
    message("Note: r may have low precision \n")} else{message()}
  }

  # Return the following values
  return(list(r = r,
              ci = c(ci_LL, ci_UL),
              n = n,
              t_value = t_value,
              p_value = p_value,
              moe = moe))
}
