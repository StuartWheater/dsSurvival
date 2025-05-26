#' @title Sets the server-side datadist object as the global option
#' @description Sets a previously created datadist object as the global datadist option
#' @details Server-side assign function \code{useDatadistDS} called by client-side function \code{ds.useDatadist}.
#' This function sets a datadist object created with datadistDS as the global option for rms plotting functions.
#' @param datadist character string specifying the name of a datadist object
#' @return TRUE if successful
#' @author Xavier Escribà Montagut, 2025
#' @export
useDatadistDS <- function(datadist = NULL) {
  if (is.null(datadist)) {
    stop("Please provide the name of a datadist object", call. = FALSE)
  }

  dd <- eval(parse(text = datadist), envir = parent.frame())

  if (!inherits(dd, "datadist")) {
    stop("The provided object is not a datadist object", call. = FALSE)
  }

  options(datadist = dd)

  return(TRUE)
}
