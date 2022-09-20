#' Create a values object.
#'
#' Create a values object for use in other functions
#'
#' @param ... value objects. Created by measure.value() or attribute value().
#' @examples
#' data(example.data)
#' config <- create.config(example.data, title = "sweden_at_eurovision_no_missing")
#' config <- add.measure.column(config, column.name = "Measure",
#'    values = values(measure.value(label = "Final Rank"),
#'    measure.value(label = "Final Points"),
#'    measure.value(label = "People on Stage")))
#' @export
values <- function(...) {
  values <- list(...)
  l <- list()
  for (value in values) {
    if (!inherits(value,"value")) {
      stop("Warning: values does not meet requirement. Try using attribute.value() or measure.value().")
    } else {
      l <- append(l, list(unclass(value)))
    }
  }
  class(l) <- "values"
  return(l)
}

#' Create a unit.values object.
#'
#' Create a unit.values object for use in other functions
#'
#' @param ... unit objects. Created by unit().
#' @examples
#' data(example.data)
#' config <- create.config(example.data, title = "sweden_at_eurovision_no_missing")
#' config <- add.unit.column(config, column.name = "Value",
#'    unit.values = unit.values(unit(label = "Unitless"),
#'    unit(label = "Number")))
#' @export
unit.values <- function(...) {
  values <- list(...)
  l <- list()
  for (value in values) {
    if (!inherits(value,"unit")) {
      stop("Warning: values does not meet requirement. Try using unit().")
    } else {
      l <- append(l, list(unclass(value)))
    }
  }
  class(l) <- "unit.values"
  return(l)
}
