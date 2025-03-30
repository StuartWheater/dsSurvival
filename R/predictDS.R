#' @title Server-side function to generate predictions from rms models
#' @description This is the server-side implementation of the prediction functionality for rms models
#' @details This function is called by ds.Predict and implements the actual prediction calculation
#' using rms::Predict on the server side. Includes disclosure controls to ensure privacy protection.
#' 
#' @param fit name of the fitted model object
#' @param pred_vars vector of predictor names to vary
#' @param pred_values list of values for each predictor
#' @param fun optional transformation function
#' @param type type of predictions
#' @param conf.int confidence level (0-1)
#' @param conf.type type of confidence interval
#' @param ref.zero logical; if TRUE, predictions are centered at a reference value
#' @return a prediction object from rms::Predict
#' @author Xavier Escribà Montagut, 2025
#' @export
predictDS <- function(fit = NULL,
                     pred_vars = NULL,
                     pred_values = NULL,
                     fun = NULL,
                     type = NULL,
                     conf.int = 0.95,
                     conf.type = c("mean", "individual"),
                     ref.zero = FALSE) {

  if (!exists(fit)) {
      stop("The specified model object does not exist", call.=FALSE)
  }

  model_obj <- eval(parse(text=fit), envir = parent.frame())

  if (!inherits(model_obj, c("rms", "cph", "lrm", "ols", "psm"))) {
      stop("The specified model is not an rms model", call.=FALSE)
  }

  pred_spec <- list()
  for (i in seq_along(pred_vars)) {
      pred_spec[[pred_vars[i]]] <- pred_values[[i]]
  }

  pred_args <- c(
      list(
          object = model_obj,
          conf.int = conf.int,
          conf.type = conf.type[1],
          ref.zero = ref.zero
      ),
      pred_spec
  )

  if (!is.null(fun)) {
      # Validate transformation function
      if (!(fun %in% c("exp", "log", "log2", "log10"))) {
          stop("Disclosure risk: only standard transformations are allowed", call.=FALSE)
      }
      pred_args$fun <- match.fun(fun)
  }

  if (!is.null(type)) {
      pred_args$type <- type
  }

  result <- do.call(rms::Predict, pred_args)
  return(result)
} 