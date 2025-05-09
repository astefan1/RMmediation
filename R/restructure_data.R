#' Restructure data to Bauer (2006) format
#' @param X Independent variable (must be numeric)
#' @param M Mediator variable (must be numeric)
#' @param Y Dependent variable (must be numeric)
#' @param ID Subject ID variable
#' @importFrom reshape2 melt

restructure_data <- function(X, M, Y, ID) {

  # Is the data in long format?
  stopifnot(length(unique(ID)) < length(ID))

  # Convert X, M, Y to numeric (in case they aren't)
  X <- as.numeric(X)
  M <- as.numeric(M)
  Y <- as.numeric(Y)

  # Combine variables to dataset
  dat <- data.frame(X=X, M=M, Y=Y, ID=ID)

  # Convert to Bauer's format
  datBauer <- reshape2::melt(dat,
                             id.vars = c("ID", "X", "M"),
                             measure.vars = c("M", "Y"),
                             variable.name = "S",
                             value.name = "Z")
  datBauer$SM <- as.numeric(datBauer$S == "M")
  datBauer$SY <- as.numeric(datBauer$S == "Y")

  # Skip S column
  datBauer <- datBauer[, colnames(datBauer) != "S"]

  # Return
  return(datBauer)

}
