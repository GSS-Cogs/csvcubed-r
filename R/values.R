#' Create a values object.
#'
#' Create a values object for use in other functions
#'
#' @param ... value objects. Created by measure.value() or attribute.value().
#' @examples
#' data(example.data)
#' config <- create.config(example.data, title = "sweden_at_eurovision_no_missing")
#' config <- add.measure.column(config,
#'   column.name = "Measure",
#'   values = values(
#'     measure.value(label = "Final Rank"),
#'     measure.value(label = "Final Points"),
#'     measure.value(label = "People on Stage")
#'   )
#' )
#' @export
values <- function(...) {
  values <- list(...)
  l <- list()
  for (value in values) {
    if (inherits(value, "value")) {
      l <- append(l, list(unclass(value)))
    } else if (is.single.string(value) && !is.empty.string(value)) {
      l <- append(l, list(unclass(attribute.value(label = value))))
    } else {
      stop("Warning:values do not meet requirement. Values must be non-empty strings or created by measure.value() or attribute.value()")
    }
  }
  class(l) <- "values"
  return(l)
}

#' Create a unit.values object.
#'
#' Create a unit.values object for use in other functions
#'
#' @param ... unit objects created by unit.value() or strings.
#' @examples
#' data(example.data)
#' config <- create.config(example.data, title = "sweden_at_eurovision_no_missing")
#' config <- add.unit.column(config,
#'   column.name = "Value",
#'   unit.values = unit.values(
#'     unit.value(label = "Unitless"),
#'     unit.value(label = "Number")
#'   )
#' )
#' @export
unit.values <- function(...) {
  values <- list(...)
  l <- list()
  for (value in values) {
    if (inherits(value, "unit")) {
      l <- append(l, list(unclass(value)))
    } else if (is.single.string(value) && !is.empty.string(value)) {
      l <- append(l, list(unclass(unit.value(label = value))))
    } else {
      stop("Warning:Unit values do not meet requirement. Values must be non-empty strings or created by unit.value().")
    }
  }
  class(l) <- "unit.values"
  return(l)
}
