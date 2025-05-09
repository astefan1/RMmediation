#' Check whether a dataset fulfills the requirements of the Bauer (2006) format
#' @param data Dataframe object

checkBauerFormat <- function(data) {

  if(!all(c("X", "Z", "M", "SM", "SY", "ID") %in% colnames(data))){
    stop("The columns names do not follow Bauer(2006). Consider running restructure_data().")
  }

  stopifnot("X must be numeric" = is.numeric(data$X))
  stopifnot("Z must be numeric" = is.numeric(data$Z))

  stopifnot("SM must be a vector of 0s and 1s" = unique(data$SM) == c(0,1) | unique(data$SM) == c(1,0))
  stopifnot("SY must be a vector of 0s and 1s" = unique(data$SY) == c(0,1) | unique(data$SY) == c(1,0))

}
