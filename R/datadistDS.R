#' @title Creates distribution summaries for rms plotting functions at the server-side environment
#' @description Computes statistical summaries of variables for use in plotting regression model fits
#' @details Server-side assign function {datadistDS} called by client-side function {ds.datadist}.
#' Returns a datadist object containing distribution summaries for use in rms plotting functions.
#' This function ensures privacy by only returning summarized statistics and checking for minimum
#' counts in categorical variables.
#' @param data character string specifying the name of a data frame
#' @param adjust_to named list where names are variable names and values are either "min", "max", or "mean"
#'        to set the "Adjust to" values for specific variables
#' @return an rms::datadist object containing distribution summaries
#' @author [Your Name], 2024
#' @export
datadistDS <- function(data = NULL, adjust_to = NULL) {
    
    #########################################################################
    # DataSHIELD MODULE: CAPTURE THE nfilter SETTINGS                        #
    thr <- dsBase::listDisclosureSettingsDS()                               #
    nfilter.tab <- as.numeric(thr$nfilter.tab)                             #
    #########################################################################
    
    # Check if required packages are available
    if (!requireNamespace("rms", quietly = TRUE)) {
        stop("Package 'rms' needed for this function to work. Please install it on the server.", call. = FALSE)
    }
    
    # Get the data frame from the parent environment
    df <- eval(parse(text = data), envir = parent.frame())
    
    # Check if it's actually a data frame
    if (!is.data.frame(df)) {
        stop("The provided object is not a data frame", call. = FALSE)
    }
    
    # Check categorical variables for disclosure risk
    for (col in names(df)) {
        if (is.factor(df[[col]]) || is.character(df[[col]])) {
            tab <- table(df[[col]])
            if (any(tab < nfilter.tab)) {
                stop(paste("Disclosure risk: category with count less than nfilter.tab in variable", 
                          col), call. = FALSE)
            }
        }
    }
    
    # Create the datadist object
    dd <- rms::datadist(df)
    
    # Adjust the "Adjust to" values if specified
    if (!is.null(adjust_to)) {
        for (var in names(adjust_to)) {
            if (!var %in% names(df)) {
                stop(paste("Variable", var, "not found in the data frame"), call. = FALSE)
            }
            
            if (adjust_to[[var]] == "min") {
                dd$limits["Adjust to", var] <- min(df[[var]], na.rm = TRUE)
            } else if (adjust_to[[var]] == "max") {
                dd$limits["Adjust to", var] <- max(df[[var]], na.rm = TRUE)
            } else if (adjust_to[[var]] == "mean") {
                dd$limits["Adjust to", var] <- mean(df[[var]], na.rm = TRUE)
            }
        }
    }
    
    return(dd)
} 