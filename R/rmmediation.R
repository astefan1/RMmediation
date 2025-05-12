#' Fit Bayesian repeated-measures mediation model
#' @param data Data frame in the Bauer et al. (2006) format. Column names need to contain X, Z, SM, SY, ID.
#' @param ... Arguments submitted to brm
#' @importFrom brms bf
#' @importFrom brms brm
#' @importFrom parallel detectCores
#' @export

rmmediation <- function(data, ...){

  checkBauerFormat(data)

  # Reduce data to important variables
  data <- data[, c("ID", "X", "M", "Z", "SM", "SY")]

  # Compute interaction effects for regression model
  data$SMX <- data$X * data$SM
  data$SYX <- data$X * data$SY
  data$SYM <- data$M * data$SY

  # Define the model
  model_formula <- brms::bf(Z ~ 0 + SM + SMX + SY + SYM + SYX +
                              (0 + SM + SMX + SY + SYM + SYX | ID),
                            sigma ~ 0+ SM + SY)

  # Fit the model

  # Take arguments from function call
  args <- list(...)

  # These brms arguments will always be used (... would be overwritten)
  args[["formula"]] <- model_formula
  args[["data"]] <- data
  args[["family"]] <- "gaussian"

  # These arguments are only supplied if there are no arguments in ...
  if(is.null(args[["chains"]])) args[["chains"]] <- 4
  if(is.null(args[["iter"]])) args[["iter"]] <- 5000
  if(is.null(args[["warmup"]])) args[["warmup"]] <- 2000
  if(is.null(args[["cores"]])) args[["cores"]] <- min(4, detectCores(logical = FALSE))

  # BRMS model fitting function call
  fit <- do.call(brms::brm, args = args)

  # Return

  return(fit)

}
