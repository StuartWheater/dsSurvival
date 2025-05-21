#' @title Server-side function to create data for a Fine-Gray model
#' @description This is the server-side function called by ds.finegray to create a dataset for fitting a Fine-Gray model
#' @details Server-side assign function \code{finegrayDS} called by client-side function \code{ds.finegray}.
#' This function is a wrapper for the survival::finegray function, which creates a dataset for fitting a Fine-Gray model
#' in competing risks analysis. The Fine-Gray model is fit by first creating a special data set,
#' and then fitting a weighted Cox model to the result.
#' 
#' @param formula character string or an object of class formula specifying the model
#' @param data character string specifying the name of a data frame
#' @param .weights character string specifying the name of a weights variable (optional)
#' @param na.action character string specifying how to handle missing values
#' @param etype the event type for which a data set will be generated (character or numeric)
#' @param prefix prefix for the new variables that will be added to the dataset
#' @param count character string for an optional count variable
#' @param id character string specifying an ID variable (optional)
#' @param timefix logical indicating whether to apply correction for tied survival times
#' @return The modified data frame with Fine-Gray data preparation
#' @author Xavier Escribà Montagut, 2025
#' @export
finegrayDS <- function(formula = NULL,
                      data = NULL,
                      .weights = NULL,
                      na.action = "na.pass",
                      etype = NULL,
                      prefix = "fg",
                      count = NULL,
                      id = NULL,
                      timefix = TRUE) {
  if (is.null(formula) || is.null(data)) {
    stop("Both 'formula' and 'data' parameters must be provided", call. = FALSE)
  }

  formula <- Reduce(paste, deparse(formula))

  formula <- gsub("sssss", "survival::Surv(", formula, fixed = TRUE)
  formula <- gsub("ggggg", "rms::rcs(", formula, fixed = TRUE)
  formula <- gsub("aaaaa", "rms::asis(", formula, fixed = TRUE)
  formula <- gsub("mmmmm", "rms::matrx(", formula, fixed = TRUE)
  formula <- gsub("ooooo", "rms::pol(", formula, fixed = TRUE)
  formula <- gsub("hhhhh", "rms::lsp(", formula, fixed = TRUE)
  formula <- gsub("ccccc", "rms::catg(", formula, fixed = TRUE)
  formula <- gsub("ddddd", "rms::scored(", formula, fixed = TRUE)
  formula <- gsub("nnnnn", "rms::strat(", formula, fixed = TRUE)
  formula <- gsub("ttttt", "rms::gTrans(", formula, fixed = TRUE)
  formula <- gsub("lll", "=", formula, fixed = TRUE)
  formula <- gsub("xxx", "|", formula, fixed = TRUE)
  formula <- gsub("yyy", "(", formula, fixed = TRUE)
  formula <- gsub("zzz", ")", formula, fixed = TRUE)
  formula <- gsub("ppp", "/", formula, fixed = TRUE)
  formula <- gsub("qqq", ":", formula, fixed = TRUE)
  formula <- gsub("rrr", ",", formula, fixed = TRUE)
  formula <- gsub("jjj", ".", formula, fixed = TRUE)
  formula <- stats::as.formula(formula)

  dataDF <- eval(parse(text = data), envir = parent.frame())

  if (!is.data.frame(dataDF)) {
    stop("The 'data' parameter must refer to a data.frame", call. = FALSE)
  }

  call_args <- list(formula = formula, data = dataDF)

  if (!is.null(.weights)) {
    weights_var <- eval(parse(text = .weights), envir = parent.frame())
    call_args$weights <- weights_var
  }

  if (!is.null(na.action)) {
    if (na.action == "na.pass") {
      call_args$na.action <- stats::na.pass
    } else if (na.action == "na.omit") {
      call_args$na.action <- stats::na.omit
    } else if (na.action == "na.exclude") {
      call_args$na.action <- stats::na.exclude
    } else if (na.action == "na.fail") {
      call_args$na.action <- stats::na.fail
    } else {
      call_args$na.action <- stats::na.pass  # default
    }
  }

  if (!is.null(etype)) {
    call_args$etype <- etype
  }

  call_args$prefix <- prefix

  if (!is.null(count) && count != "") {
    call_args$count <- eval(parse(text = count), envir = parent.frame())
  }

  if (!is.null(id)) {
    id_var <- eval(parse(text = id), envir = parent.frame())
    call_args$id <- id_var
  }

  call_args$timefix <- timefix
  result <- do.call(survival::finegray, call_args)
  return(result)
}
