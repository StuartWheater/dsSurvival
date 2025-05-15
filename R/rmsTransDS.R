#' @title Creates restricted cubic splines and other transformations using rms package at the serverside environment
#' @description Returns a transformed variable using rms::rms.trans from the server side environment.
#' @details Serverside assign function \code{rmsTransDS} for creating restricted cubic splines and other transformations.
#' This function primarily focuses on restricted cubic splines (rcs) by default but supports other transformations.
#' Returns transformed variables for use in survival analysis from the server side environment.
#' @param x name of the variable to be transformed. Should be a character string.
#' @param transformation type of transformation to apply. Default is "rcs" for restricted cubic splines.
#'        Other options include "asis", "pol", "lsp", "catg", "scored", "strat", "gTrans".
#' @param parms parameters specific to the transformation type:
#'        \itemize{
#'          \item "rcs" - numeric vector specifying knot locations
#'          \item "asis" - NULL (no parameters needed)
#'          \item "pol" - integer specifying polynomial degree
#'          \item "lsp" - numeric vector specifying knot locations for linear spline
#'          \item "catg" - numeric vector specifying cut points for categories
#'          \item "scored" - numeric vector specifying scores for ordinal levels
#'          \item "strat" - numeric vector specifying cut points for stratification
#'          \item "gTrans" - function to transform variable using custom function
#'        }
#' @return a transformed variable using rms::rms.trans from the server side environment.
#' @author Xavier Escribà Montagut, 2025
#' @import rms
#' @export
rmsTransDS <- function(x = NULL,
                      transformation = "rcs",
                      parms = NULL) {
    
    #########################################################################
    # DataSHIELD MODULE: CAPTURE THE nfilter SETTINGS                        #
    thr <- dsBase::listDisclosureSettingsDS()                               #
    nfilter.tab <- as.numeric(thr$nfilter.tab)                             #
    #########################################################################
    
    # Check if x is provided
    if (is.null(x)) {
        stop("Variable name (x) must be provided.", call.=FALSE)
    }
    
    # Check type for x parameter
    class_x <- dsBase::classDS(x=x)
    if (!('numeric' %in% class_x) & !('integer' %in% class_x)) {
        stop('Variable to be transformed (x) must be numeric or integer.', call.=FALSE)
    }
    
    # Evaluate x in the parent frame
    x_param <- eval(parse(text=x), envir = parent.frame())
    
    # Check minimum sample size using nfilter.tab
    if (length(stats::na.omit(x_param)) < nfilter.tab) {
        stop(paste0("Insufficient data points for transformation. Minimum required: ", 
                   nfilter.tab), call.=FALSE)
    }

    if (is.character(parms)) {
        parms <- eval(parse(text=parms), envir = parent.frame())
    }
    
    # Handle transformations
    if (transformation == "rcs") {
        transformed <- rms::rcs(x_param, parms)
    } else if (transformation == "asis") {
        transformed <- rms::asis(x_param, parms)
    } else if (transformation == "pol") {
        transformed <- rms::pol(x_param, parms)
    } else if (transformation == "lsp") {
        transformed <- rms::lsp(x_param, parms)
    } else if (transformation == "catg") {
        transformed <- rms::catg(x_param, parms)
    } else if (transformation == "scored") {
        transformed <- rms::scored(x_param, parms)
    } else if (transformation == "strat") {
        transformed <- rms::strat(x_param, parms)
    } else if (transformation == "gTrans") {
        transformed <- rms::gTrans(x_param, parms)
    } else {
        stop(paste("Invalid transformation. Must be one of:", 
                  paste(c("rcs", "asis", "pol", "lsp", "catg", "scored", "strat", "gTrans"),
                        collapse=", ")), call.=FALSE)
    }
    
    return(transformed)
}
#ASSIGN FUNCTION
# rmsTransDS 
