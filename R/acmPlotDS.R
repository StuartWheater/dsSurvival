#' @title Server-side function to return prediction data for ACM plot
#' @description This function returns the prediction data for ACM (All-Cause Mortality) analysis
#' @details This function is called by ds.acmPlot and returns the prediction data
#' from the server side.
#' 
#' @param pred_obj name of the prediction object created by predictDS
#' @return a data frame containing prediction data
#' @author Xavier Escribà Montagut, 2025
#' @export
acmPlotDS <- function(pred_obj = NULL) {
  pred_data <- eval(parse(text=pred_obj), envir = parent.frame())
  if (!exists(pred_data)) {
    stop("The specified prediction object does not exist", call.=FALSE)
  }

  if(privacy_level != 'permissive' || privacy_level != 'avocado'){
    stop("This function requires the server to be in permissive mode", call.=FALSE)
  }

  if (!inherits(pred_data, "Predict")) {
    stop("The provided object is not of class 'Predict'", call.=FALSE)
  } else {
    return(pred_data)
  }
}