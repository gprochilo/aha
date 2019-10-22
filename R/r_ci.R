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
#' @param null_value The null hypothesis value used to evaluate the sample Pearson's
#' correlation coefficient (default is 0).
#' @param silent A logical indicating whether to suppress message output: FALSE (default) or TRUE.
#' @param print_plot A logical indicating whether to print plot of results: FALSE (default) or TRUE.
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
                 null_value = 0,
                 silent = FALSE,
                 print_plot = TRUE){

  # Define the following stop functions

  if(missing(r) || !is.numeric(r)  || length(r) > 1){
    stop("Please provide a single numeric value for 'r'",
         call. = FALSE)
  }

  if(r >= 1 || r <= -1  || length(r) > 1){
    stop("'r' must be a single number between -1 and 1",
         call. = FALSE)
  }

  if(null_value >= 1 || null_value <= -1  || length(null_value) > 1){
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

  # Define internal function (required for plotting function)

  r_internal <- function(r,
                         n,
                         alternative,
                         conf_level,
                         null_value,
                         silent,
                         print_plot){

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

  # Compute test statistic and p values
  # If null_value = 0 we use a t test:

  if(null_value == 0){

  deg_f = n - 2
  t_value =  r / (sqrt((1 - r^2)/(n - 2)))
  p_value = switch(alternative,
                   "two_sided" = 2 * min(pt(q = t_value, df = deg_f),
                                         pt(q = t_value, df = deg_f, lower.tail = FALSE)),
                   "greater" = pt(q = t_value, df = deg_f, lower.tail = FALSE),
                   "less" = pt(q = t_value, df = deg_f))

  test_message = "test statistic:    t value"
  symbol = "t"
  test_statistic = t_value

  }

  # If null_value != 0 we use a z test:

# Reference for SAS code:
# Weaver, B., & Wuensch, K. L. (2013). SPSS and SAS programs for comparing
# Pearson correlations and OLS regression coefficients. Behavior Research
# Methods, 45(3), 880-895. doi: 10.3758/s13428-012-0289-7

  if(null_value != 0){

    r_prime = 0.5 * log(abs((1 + r)/(1 - r)))
    null_prime = 0.5 * log(abs((1 + null_value) / (1 - null_value)))
    se = sqrt(1 / (n - 3))
    critical_z = qnorm(.025, lower.tail = FALSE)
    z_value = (r_prime - null_prime) / se

    p_value = switch(alternative,
                  "two_sided" = 2 * min(pnorm(z_value), pnorm(z_value, lower.tail = FALSE)),
                  "greater" = pnorm(z_value, lower.tail = FALSE),
                  "less" = pnorm(z_value))

    test_message = "test statistic:    z value"
    symbol = "z"
    deg_f = NULL
    test_statistic = z_value

  }

  # Define a message to tell the user what test they have performed

  hyp = switch(alternative,
               "two_sided" = paste("hypothesis:        true correlation is not equal to", null_value),
               "greater" = paste("hypothesis:        true correlation is greater than", null_value),
               "less" = paste("hypothesis:        true correlation is less than", null_value)
  )

  # Compute the margin of error as the half-width of a two-sided confidence
  # interval. MoE is not computed for 1-sided tests and "NA" is returned.

  moe = switch(alternative,
               "two_sided" = (ci_UL - ci_LL) / 2,
               "greater" = "NA",
               "less" = "NA")

  # Define a user-friendly output of the result to return as a message

  res = sprintf("r = %s, %.0f%% CI [%.2f, %.2f], MoE = %s, %s%s = %.2f, p = %s",
                substring(format(r, nsmall = 2), 2),
                conf_level * 100,
                ci_LL,
                ci_UL,
                ifelse(is.character(moe), moe, round(moe, 2)),
                symbol,
                ifelse(is.null(deg_f), "", paste0("(", deg_f, ")")),
                test_statistic,
                ifelse(p_value == 1, "1",
                ifelse(p_value < .001, "<.001", substring(format(round(p_value, 3), nsmall = 3), 2))))

  # Save these variables

  return(list(r = r,
              ci = c(ci_LL, ci_UL),
              n = n,
              statistic = test_statistic,
              p_value = p_value,
              moe = moe,
              res = res,
              hyp = hyp,
              test_message = test_message))

  }

  # Run r_internal function

  r_internal(r,
             n,
             alternative,
             conf_level,
             null_value,
             silent) -> result

  # Plot data

  # Compute p values for all null hypotheses of user specified sample size n
  purrr::map_df(seq(-0.99, 0.99, 0.01),
                ~data.frame("null" = ., "p" =
                              r_internal(r = r,
                                         n = n,
                                         alternative = alternative,
                                         conf_level = conf_level,
                                         null_value = .,
                                         silent = TRUE,
                                         print_plot = FALSE)$p_value)) -> ps

  # Plot the data
  ps %>%
    ggplot2::ggplot(aes(x = null, y = p)) +
    ggplot2::geom_point() +
    ggplot2::geom_line() +
    ggplot2::geom_vline(xintercept = c(result$ci[1], result$ci[2]), linetype = "dashed", color = "blue") +
    ggplot2::geom_vline(xintercept = null_value, size = 1) +
    ggplot2::geom_hline(yintercept = 1 - conf_level, color = "red", linetype = "dashed") +
    ggplot2::geom_ribbon(data = ps %>% filter(null >= result$ci[1] & null <= result$ci[2]),
                                              aes(ymin = 0, ymax = 1),
                         alpha = 0.05, fill = "blue") +
    ggplot2::scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
    ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
    coord_cartesian(ylim = c(0, 1), expand = FALSE, clip = "off") +
    ggplot2::labs(x = "Null Hypothesis Value", y = expression(paste(italic("p"), " value")),
                  caption = paste(" - The black solid line represents the null value for the summary data hypothesis test",  "\n",
                                  "- The blue dashed lines represent the", conf_level * 100, "% CI: r = ", result$r, "[", round(result$ci[1], 2), ",", round(result$ci[2], 2), "]", "\n",
                                  "- The red dashed line represents the criterion for significance:", (1-conf_level), "\n",
                                  "- The", conf_level * 100, "% CI is comprised of all null values that yield p >", 1 - conf_level, "\n",
                                  "- All values in the shaded region are compatible with r = ", result$r)) +
    theme_light(base_size = 15) +
    theme(plot.caption = element_text(hjust = 0)) +
    ggtitle(expression(paste(italic("P"), " Value Function"))) -> r_plot



  if(print_plot == TRUE){
    print(r_plot)
  }

  # Return the hypothesis used and user-friendly result as messages if silent = FALSE

  if(silent == FALSE){

    message(result$res, "\n")
    message(result$hyp)
    message(result$test_message)

    if(is.numeric(result$moe) & result$moe > r / 2){
      message("note:              r may have low precision \n")} else{message()}
  }

  # Return results

  return(result[1:6])

}
