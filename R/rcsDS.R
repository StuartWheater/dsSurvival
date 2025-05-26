#' @title Server-side restricted cubic splines creation
#' @description Creates restricted cubic splines on the server side using rms::rcs
#' @details This is the server-side function that creates restricted cubic splines
#'     transformations using the rms package.
#'
#' @param x character string specifying the variable to be transformed
#' @param knots integer specifying the number of knots for restricted cubic splines
#' @return The transformed variable with restricted cubic splines
#' @author Xavier Escribà Montagut, 2025
#' @export
rcsDS <- function(x, knots = 5) {
  if (is.character(x)) {
    x <- eval(parse(text = x), envir = parent.frame())
  }

  if (is.character(knots)) {
    knots <- eval(parse(text = knots), envir = parent.frame())
  }

  result <- rms::rcs(x, knots)

  return(result)
}