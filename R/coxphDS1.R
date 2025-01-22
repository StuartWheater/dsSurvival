#'
#' @title coxphDS1 returns a dataframe of unique event times
#' @description This function returns a dataframe of unique event times
#' @param df the name of the data frame that contains the variables to be used 
#' in the Cox PH model.
#' @param time_col name of the column in \code{df} that contains the event/censor times
#' @param censor_col name of the column in \code{df} that explains whether an event 
#' occurred or the patient was censored
#' @return a dataframe with columns time and Freq.
#' @author Demetris Avraam
#' @export
#'
coxphDS1 <- function(df, time_col, censor_col){
  
  # Capture the nfilter for small cell counts
  thr <- dsBase::listDisclosureSettingsDS()
  nfilter.tab <- as.numeric(thr$nfilter.tab)  
  
  df <- eval(parse(text=df), envir = parent.frame())
  
  time <- df[df[, censor_col]==1, time_col]
  time <- sort(time)
  
  df_time <- as.data.frame(table(time), stringsAsFactors=F)
  df_time <- apply(df_time, 2, as.numeric)
  
  # check if any cells (time-intervals) have less than the disclosure threshod values 
  small_cells <- length(which(df_time[,'Freq'] < nfilter.tab))
  
  if (small_cells == 0){
    return(df_time)
  }else{
    return.message <- paste0("Failed: at least one time has less than ",nfilter.tab, "events")  
    stop(return.message, call. = FALSE)
  }  
  
}