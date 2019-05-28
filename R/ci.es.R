#' @title Compute Confidence Interval and Margin-of-Error for Cohen's dz,
#' Cohen's ds, or Pearson's r
#'
#' @description This function computes a confidence interval (CI) and confidence
#' interval half-width (i.e., margin-of-error; hereby: MoE) for Cohen's dz,
#' Cohen's ds, or Pearson's r. The CI for Cohen's dz and Cohen's ds is computed
#' using an exact method based on the noncentral t distribution. The CI for
#' Pearson's r is computed using Fisher's transformation.
#'
#' Note: Cohen's dz is computed on paired sample data as the mean difference
#' divided by the standard deviation of the difference. It also returns t and p
#' values for each combination of effect size and sample size.
#'
#' Note: Cohen's ds is computed on two sample data as the mean difference
#' divided by the pooled standard deviation of sample 1 and sample 2.
#'
#' @param es Effect size in Cohen's dz, Cohen's ds, or Pearson's r units.
#' @param N Total sample size for Cohen's dz or Pearson's r.
#' @param n1 Sample size for sample 1 (Cohen's ds).
#' @param n2 Sample size for sample 2 (Cohen's ds).
#' @param effect Specify one of "dz", "ds", or "r".
#' @param conf Specify the confidence interval width. Defaults to 0.95.
#' @param tails Specify whether to perform a 1- or 2-tailed test. Defaults to 2.
#' @param pr Specify whether to include printed output. Defaults to TRUE.
#'
#' @keywords Cohen's d, confidence intervals, Pearson's r, correlation coefficient
#'
#' @export
#' @examples
#' # Compute a 2-tailed 95% CI on Cohen's dz = 0.6 with 20 paired samples
#' ci.es(es = 0.6, N = 20, effect = "dz", conf = 0.95, tails = 2)
#'
#' # Compute a 1-tailed 95% CI on Cohen's dz = 0.6 with 20 paired samples
#' ci.es(es = 0.6, N = 20, effect = "dz", conf = 0.95, tails = 1)
#'
#' # Compute a 2-tailed 95% CI on Cohen's ds = 0.6 with 10 observations in each
#' # sample
#' ci.es(es = 0.6, n1 = 10, n2 = 10, effect = "ds", conf = 0.95, tails = 2)
#'
#' # Compute a 2-tailed 95% CI on Pearson's r = 0.6 with 20 bivariate samples
#' ci.es(es = 0.6, N = 20, effect = "r", conf = 0.95, tails = 2)
#'
#' # Compute a 2-tailed 95% CI on Pearson's r = 0.6 with 20 bivariate samples,
#' # but suppress any print statements.
#' ci.es(es = 0.6, N = 20, effect = "r", conf = 0.95, tails = 2, pr = FALSE)

ci.es <- function(es,
                  N = NULL,
                  n1 = NULL,
                  n2 = NULL,
                  effect = c("dz","ds","r"),
                  conf = 0.95,
                  tails = 2,
                  pr = TRUE){

  if(effect == "r"){if(es <= -1 | es >= 1){
    return(cat("Error: please specify an r greater than -1 and less than 1."))
  }}

  if(class(es) != "numeric"){
    return(cat("Error: please enter a numerical value for es."))
  }

  if(conf >= 1 || conf <= 0){
    return(cat("Error: conf must be greater than 0 and less than 1."))
  }

  if(tails != 1 && tails != 2){
    return(cat("Error: tails must equal 1 or 2"))}

  if(effect != "dz" && effect != "ds" && effect != "r"){
    return(cat("Error: effect must be specified as either 'dz', 'ds', or 'r'."))}

  if(effect == "ds"){if(length(N) == 1){
    return(cat("Error: if effect = 'ds' you must specify sample size per group for 'n1' and 'n2' and leave 'N' blank."))}
  }

  if(effect == "ds"){if(is.null(n1) | is.null(n2)){
    return(cat("Error: if effect = 'ds' you must specify sample size per group for 'n1' and 'n2'."))}
  }

  if(effect == "dz" || effect == "r"){if(is.null(N)){
    return(cat("Error: if effect = 'dz' or 'r' you must specify the total sample size 'N'."))}
  }

  if(effect == "dz" || effect == "r"){if(length(n1) == 1 | length(n2) == 1){
    return(cat("Error: if effect = 'dz' or 'r' you must specify the total sample size 'N' and leave 'n1' and 'n2' blank."))}
  }

  if(!is.null(N)){if(N <= 3){
    return(cat("Error: please specify a sample size greater than 3"))
  }}

  if(!is.null(n1) || !is.null(n2)){if(n1 <= 3 || n2 <= 3){
    return(cat("Error: each sample must have more than 3 observations."))
  }}

  quiet <- function(x){

    sink(tempfile())
    on.exit(sink())
    invisible(force(x))
  }

  if(effect == "dz"){
    eff <- function(i){
      df = i-1
      tval = sqrt(i)*es
      if(tails==2){pval = 2*pt(-abs(tval),df = df)}
      if(tails==1){pval = 1*pt(-abs(tval),df = df)}
      if(tails==2){conf = conf}
      if(tails==1){conf = (2*conf)-1}
      es.ci = quiet(MBESS::ci.sm(sm = es, N = i, conf.level = conf))
      es.ci.LL = es.ci$Lower.Conf.Limit.Standardized.Mean
      es.ci.UL = es.ci$Upper.Conf.Limit.Standardized.Mean
      moe = (es.ci.UL-es.ci.LL)/2
      return(list(es = es,
                  ci = c(es.ci.LL, es.ci.UL),
                  moe = moe,
                  df = df,
                  tval = tval,
                  pval = pval))
    }
  }

  if(effect == "ds"){
    eff <- function(i,j){
      df = (i+j)-2
      tval = (sqrt(df)*es)/2
      if(tails==2){pval = 2*pt(-abs(tval),df = df)}
      if(tails==1){pval = 1*pt(-abs(tval),df = df)}
      if(tails==2){conf = conf}
      if(tails==1){conf = (2*conf)-1}
      es.ci = quiet(MBESS::ci.smd(smd = es, n.1 = i, n.2 = j, conf.level = conf))
      es.ci.LL = es.ci$Lower.Conf.Limit.smd
      es.ci.UL = es.ci$Upper.Conf.Limit.smd
      moe = (es.ci.UL-es.ci.LL)/2
      return(list(es = es,
                  ci = c(es.ci.LL, es.ci.UL),
                  moe = moe,
                  df = df,
                  tval = tval,
                  pval = pval))
    }
  }

  if(effect == "r"){
    eff <- function(i){
      df = i-2
      tval = psych::r2t(rho = es, n = i)
      if(tails==2){pval = 2*pt(-abs(tval),df = df)}
      if(tails==1){pval = 1*pt(-abs(tval),df = df)}
      if(tails==2){conf = conf}
      if(tails==1){conf = (2*conf)-1}
      es.ci = psych::r.con(rho = es, n = i, p = conf)
      es.ci.LL = es.ci[1]
      es.ci.UL = es.ci[2]
      moe = (es.ci.UL-es.ci.LL)/2
      return(list(es = es,
                  ci = c(es.ci.LL, es.ci.UL),
                  moe = moe,
                  df = df,
                  tval = tval,
                  pval = pval))
    }
  }

  if(effect == "dz" | effect == "r"){res = eff(N)}
  if(effect == "ds"){res = eff(n1,n2)}


  conf.label = conf*100
  report = sprintf("%s-tailed test: %s = %.2f, %s%%CI [%.2f, %.2f], MoE = %.2f, t(%s) = %.2f, p = %.3f",
                   tails,
                   effect,
                   es,
                   conf.label,
                   res$ci[1],
                   res$ci[2],
                   res$moe,
                   res$df,
                   res$tval,
                   res$pval)
  if(tails == 1){
  report = sprintf("%s-tailed test: %s = %.2f, %s%%CI [%.2f, %.2f], MoE = NA, t(%s) = %.2f, p = %.3f",
                   tails,
                   effect,
                   es,
                   conf.label,
                   res$ci[1],
                   res$ci[2],
                   res$df,
                   res$tval,
                   res$pval)

  res$moe = "NA"}

  if(pr == TRUE){cat(report,"\n")}
  if(pr == TRUE){
    if(res$moe > es/2){cat("Note: This", effect, "may have low precision\n\n", sep=" ")}
    else{cat("\n")}
  }

  return(res)
}
