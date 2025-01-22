#'
#' @title coxphDS2 computes the aggregate statistic
#' @description This function computes the aggregate statistic that is the
#' sum over all distinct times i (sum the covariates of cases in the set of cases 
#' with events at time i).
#' @param df the name of the data frame that contains the variables to be used 
#' in the Cox PH model.
#' @param expl_vars list of the explanatory variables (covariates) that are
#' included as columns in \code{df}
#' @param time_col name of the column in \code{df} that contains the event/censor times
#' @param censor_col name of the column in \code{df} that explains whether an event 
#' occurred or the patient was censored
#' @return a numeric vector with sums and named index with covariates
#' @author Demetris Avraam
#' @export
#'
coxphDS2 <- function(df, expl_vars, time_col, censor_col){
  
  df <- eval(parse(text=df), envir = parent.frame())
  
  expl_vars <- unlist(strsplit(expl_vars, split=","))
  
  # Split the dataframe column wise into covariate, censor and time columns.
  preprocess.data <- function(df, expl_vars, censor_col, time_col) {
    
    # Sort the dataframe/matrix by time
    df[, time_col] = as.numeric(df[, time_col])
    sort_idx <- order(df[, time_col])
    df <- df[sort_idx, ]
    
    # Split the dataframe into explanatory variables, time and censor columns
    Z <- df[, expl_vars]
    time <- df[, time_col]
    censor <- df[, censor_col]
    
    if (dim(as.matrix(Z))[2] == 1) {
      Z = as.matrix(Z)
    }
    
    # Return the list
    return(list(Z=Z, time=time, censor=censor))
  }
  
  data <- preprocess.data(df, expl_vars, censor_col, time_col)
  
  # Set the condition to enable univariate Cox
  if (dim(data$Z)[2] > 1) {
    cases_with_events <- data$Z[data$censor == 1, ]
  } else {
    cases_with_events <- as.matrix(data$Z[data$censor == 1])
  }
  
  # Since an item can only be in a single set of events, we're essentially
  # summing over all cases with events.
  summed_zs <- colSums(cases_with_events)
  
  return(summed_zs)
}

