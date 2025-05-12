#' Report results of the repeated-measures mediation model
#' @param fit BRMS model object, result of rmmediation function call
#' @param digits Number of reported digits
#' @importFrom brms as_draws_array
#' @importFrom knitr kable

report_rmmediation <- function(fit, digits = 2) {

  draws <- as_draws_array(fit)

  # Fixed effect of a, b, cprime
  fixedeffects <- draws[, , c("b_SMX", "b_SYM", "b_SYX")]

  # Correlation of random effect of a and b
  corRanEffAB <- draws[,,"cor_ID__SMX__SYM"]

  # Random effects of a and b path
  randomeffects <- draws[,,c("sd_ID__SMX", "sd_ID__SYM")]

  # Covariance between random effects
  covRanef <- corRanEffAB * randomeffects[,,"sd_ID__SMX"] * randomeffects[,,"sd_ID__SYM"]

  # Indirect effects (product of a and b plus covariance)
  indirect <- fixedeffects[,,"b_SMX"] * fixedeffects[,,"b_SYM"] + covRanef

  # Total effect (indirect + cprime)
  total <- indirect + fixedeffects[,,"b_SYX"]

  # Variance and SD of indirect effects (see Bauer et al., 2006, eq. 6)
  # b^2*Var(a)+a^2*Var(b)+Var(a)*Var(b)+2*a*b*Cov(a,b)+Cov(a,b)^2

  VarIndirect <- fixedeffects[,,"b_SYM"]^2*randomeffects[,,"sd_ID__SMX"]+
    fixedeffects[,,"b_SMX"]^2*randomeffects[,,"sd_ID__SYM"] +
    randomeffects[,,"sd_ID__SMX"]^2*randomeffects[,,"sd_ID__SYM"]^2 +
    2*fixedeffects[,,"b_SMX"]*fixedeffects[,,"b_SYM"]*covRanef +
    covRanef^2

  SDindirect <- sqrt(VarIndirect)

  # Summarize fixed path coefficients
  fixedcoefs <- rbind(.draws_summary(fixedeffects, digits),
                      .draws_summary(indirect, digits),
                      .draws_summary(total, digits))
  fixedcoefs[, "Parameter"] <- c("a", "b", "c'", "indirect", "total")

  randomcoefs <- data.frame("Parameter" = c("SD(a)", "SD(b)", "cor(a,b)", "SD(indirect)"),
                            "Estimate" = c(round(mean(randomeffects[,,"sd_ID__SMX"]), digits),
                                           round(mean(randomeffects[,,"sd_ID__SYM"]), digits),
                                           round(mean(corRanEffAB), digits),
                                           round(mean(SDindirect), digits)))

  # Compute percentage mediated
  perc_mediated <- round(indirect/total*100, digits)

  return(cat(knitr::kable(fixedcoefs, row.names = FALSE, caption = "Fixed Effects"), "\n", knitr::kable(randomcoefs, caption = "Random Effects"), "\n", paste0("Percentage mediated: ", perc_mediated)))


}
