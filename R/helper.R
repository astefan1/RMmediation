#' Summary of posterior distribution
#' @description
#' Extracts posterior mean, SD, and 95% central credible interval from posterior samples
#' @param draws Array of posterior draws
#' @param digits Number of reported digits
#' @importFrom stats quantile

.draws_summary <- function(draws, digits){
  post_means <- t(apply(draws, 3, function(x) round(mean(x), digits) ))
  post_CI <- apply(draws, 3, function(x) round(stats::quantile(x, probs = c(0.025, 0.975)), digits))

  res <- cbind(dimnames(draws)$variable, t(post_means), t(post_CI))
  colnames(res) <- c("Parameter", "Estimate", "95% CI lower", "95% CI upper")
  return(res)
}
