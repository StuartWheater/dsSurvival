#'
#' @title coxphDS3 computes the three aggretated statistics needed for an iteration
#' @description This function computes the three aggretated statistics needed for an iteration
#' @param df the name of the data frame that contains the variables to be used 
#' in the Cox PH model.
#' @param expl_vars list of the explanatory variables (covariates) that are
#' included as columns in \code{df}
#' @param time_col name of the column in \code{df} that contains the event/censor times
#' @param censor_col name of the column in \code{df} that explains whether an event 
#' occurred or the patient was censored
#' @param beta.tr vector of beta coefficients. The length of this vector is equal to 
#' the length of \code{expl_vars}
#' @param unique_event_times vector of "pooled" unique event times
#' @return a list with the aggretated statistics
#' @author Demetris Avraam
#' @export
#'
coxphDS3 <- function(df, expl_vars, time_col, censor_col, beta.tr, unique_event_times) {
  
  df <- eval(parse(text=df), envir = parent.frame())
  
  expl_vars <- unlist(strsplit(expl_vars, split=","))
  
  unique_event_times <- as.numeric(unlist(strsplit(unique_event_times, split=",")))
  
  beta <- as.numeric(unlist(strsplit(beta.tr, split=",")))
  
  
  # Split the dataframe column wise into covariate, censor and time columns
  preprocess.data <- function(df, expl_vars, censor_col, time_col) {
    
    # Sort the dataframe/matrix by time
    df[, time_col] <- as.numeric(df[, time_col])
    sort_idx <- order(df[, time_col])
    df <- df[sort_idx, ]
    
    # Split dataframe into explanatory variables, time and censor columns
    Z <- df[, expl_vars]
    time <- df[, time_col]
    censor <- df[, censor_col]
    
    if (dim(as.matrix(Z))[2] == 1) {
      Z <- as.matrix(Z)
    }
    
    # Return the list
    return(list(Z=Z, time=time, censor=censor))
  }  
  
  data <- preprocess.data(df, expl_vars, censor_col, time_col)
  
  D <- length(unique_event_times)
  m <- length(expl_vars)
  
  # initialize matrices for the aggregates we're about to compute
  agg1 <- base::array(dim=c(D), 0)
  agg2 <- base::array(dim=c(D, m), 0)
  dimnames(agg2) <- list(NULL, expl_vars)
  
  agg3 <- base::array(dim=c(D, m, m), 0)
  dimnames(agg3) <- list(NULL, expl_vars, expl_vars)
  
  for (i in 1:D) {
 
    # Compute the risk set at time t. this includes all patients that have a
    # survival time greater than or equal to the current time
    R_i <- as.matrix(data$Z[data$time >= unique_event_times[i], ])

    if (nrow(R_i) == 0) {
      break
    }

    # aggregate 1: SUM_risk[exp(beta * z)]
    ebz <- exp(R_i %*% beta)
    agg1[i] <- sum(ebz)

    # aggregate 2: SUM_risk[z_r *exp(beta * z)]
    # Use apply to multiply each column (element-wise) in R_i with ebz
    z_ebz <- apply(R_i, 2, '*', ebz)

    # Undo the simplification that `apply` does in case of a single row in R_i
    if (nrow(R_i) == 1) {
      z_ebz <- t(z_ebz)
    }

    agg2[i, ] <- colSums(z_ebz)

    # aggregate 3: SUM_risk[z_r * z_q * exp(beta * z)]
    summed <- matrix(0, nrow=m, ncol=m)
    for (j in 1:nrow(R_i)) {
      # z_ebz[j, ]: numeric vector
      # the outer product creates a matrix:
      # | z1*z1 | z1*z2 | ... | z1*zm |
      # | z2*z1 | z2*z2 | ... | z2*zm |
      # | ...   | ...   | ... | ...   |
      # | zm*z1 | zm*z2 | ... | zm*zm |
      summed <- summed + z_ebz[j, ] %*% t(R_i[j, ])
    }

    agg3[i, , ] <- summed
  }
  
  return(list(agg1 = agg1, agg2 = agg2, agg3 = agg3))
  
}