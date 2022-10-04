#' Add a unit column.
#'
#' Add a unit column to the configuration.
#'
#' @param config A configuration object.The configuration object to add the column to.
#' @param column.name String. A column name in the Data Frame.
#' @param unit.values The Units used in this dataset. Can be:
#'   A boolean. Whether to automatically generate new units from the unique values in this column.
#'   A "unit.values" object. A list of units permitted in this column. Use unit.values() and unit.value() to build such an object.
#' @examples
#' data(example.data)
#' config <- create.config(example.data, title = "sweden_at_eurovision_no_missing")
#' config <- add.unit.column(config,
#'   column.name = "Unit",
#'   unit.values = unit.values("Numberless", "Unitless")
#' )
#' @export
add.unit.column <- function(config,
                            column.name,
                            unit.values = "") {
  config <- add.column.configuration(config, type = "units", column.name = column.name, unit.values = unit.values)
  return(config)
}

#' Create a unit object.
#'
#' Create a unit object for add.unit.column() or add.observation.column()
#'
#' @param label A string. Label describing the Unit.
#' @param description A string. A explanation of what the Unit represents.
#' @param from.existing A string. URI of an existing Unit to reuse. Can use built-in list Units to auto-fill.
#' @param definition.uri A string. Source URI for the definition of the Unit
#' @param scaling.factor A number. Scaling factor of the observation to base unit.
#' @param quantity.kind A string. The QUDT quantity kind this unit is used to measure. Can use built-in list QuantityKinds to auto-fill.
#' @param si.scaling.factor A number. Scaling factor to convert from this unit to the SI base unit defined in the unit's quantity_kind.
#' @examples
#' unit.value(
#'   label = "meter", description = "meter",
#'   from.existing = Units$M, si.scaling.factor = 1,
#'   quantity.kind = QuantityKinds$Length
#' )
#' @export
unit.value <- function(label,
                       description = "",
                       from.existing = "",
                       definition.uri = "",
                       scaling.factor = "",
                       quantity.kind = "",
                       si.scaling.factor = "") {
  if (!is.single.string(label)) {
    stop("Warning: label must be single nonempty string!")
  }
  if (!is.single.string(description)) {
    stop("Warning: description must be single nonempty string!")
  }
  if (!is.valid.uri(from.existing) && !is.empty.string(from.existing)) {
    stop("Warning: from.existing must be a uri.")
  }
  if (!is.valid.uri(definition.uri) && !is.empty.string(definition.uri)) {
    stop("Warning: definition.uri must be a uri.")
  }
  if (!is.single.number(scaling.factor) && !is.empty.string(scaling.factor)) {
    stop("Warning: scaling.factor must be a single number.")
  }
  if (!is.valid.uri(quantity.kind) && !is.empty.string(quantity.kind)) {
    stop("Warning: quantity.kind must be a uri.")
  }
  if (!is.single.number(si.scaling.factor) && !is.empty.string(si.scaling.factor)) {
    stop("Warning: si.scaling.factor must be a single number.")
  }
  unit <- list()
  unit.class <- "unit"
  class(unit) <- unit.class
  unit$label <- label
  unit$description <- if (!is.empty.string(description)) description
  unit$from_existing <- if (!is.empty.string(from.existing)) from.existing
  unit$definition_uri <- if (!is.empty.string(definition.uri)) definition.uri
  unit$scaling_factor <- if (!is.empty.string(scaling.factor)) scaling.factor
  unit$quantity_kind <- if (!is.empty.string(quantity.kind)) quantity.kind
  unit$si_scaling_factor <- if (!is.empty.string(si.scaling.factor)) si.scaling.factor
  return(unit)
}
