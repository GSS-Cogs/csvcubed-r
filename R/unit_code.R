#' Add a unit column.
#'
#' Add a unit column to the configuration.
#'
#' @param config A configuration object.The configuration object to add the column to.
#' @param column.name String. A column name in the Data Frame.
#' @param unit.values The Units used in this dataset.Can be:
#'   A boolean. Whether to automatically generate new units from the unique values in this column.
#'   A "unit.values" object. A list of units permitted in this column. Use unit.values() and unit() to build such an object.
#' @examples
#' data(example.data)
#' config <- create.config(example.data, title = "sweden_at_eurovision_no_missing")
#'    unit.values = unit.values(unit(label = "Unitless"),
#'    unit(label = "Number"))
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
#' @param from.existing A string. URI of an existing Unit to reuse. Can use list unit.value.from.existing to auto-fill.
#' @param definition.uri A string. Source URI for the definition of the Unit
#' @param scaling.factor A number. Scaling factor of the observation to base unit.
#' @param quantity.kind A string. The QUDT quantity kind this unit is used to measure.
#' @param si.scaling.factor A number. Scaling factor to convert from this unit to the SI base unit defined in the unit's quantity_kind.
#' @examples
#' unit(label = "meter", description = "meter",
#'    from.existing = unit.value.from.existing$M, si.scaling.factor = 1,
#'    quantity.kind = quantity.kind$Length)
#' @export
unit <- function(label,
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
  if (!is.valid.uri(from.existing) & from.existing != "") {
    stop("Warning: from.existing must be a uri.")
  }
  if (!is.valid.uri(definition.uri) & definition.uri != "") {
    stop("Warning: definition.uri must be a uri.")
  }
  if (!is.single.number(scaling.factor) & scaling.factor != "") {
    stop("Warning: scaling.factor must be a single number.")
  }
  if (!is.valid.uri(quantity.kind) & quantity.kind != "") {
    stop("Warning: quantity.kind must be a uri.")
  }
  if (!is.single.number(si.scaling.factor) & si.scaling.factor != "") {
    stop("Warning: si.scaling.factor must be a single number.")
  }
  unit <- list()

  unit.class <- "unit"

  class(unit) <- unit.class
  unit$label <- label
  unit$description <- if (description != "") description
  unit$from_existing <- if (from.existing != "") from.existing
  unit$definition_uri <- if (definition.uri != "") definition.uri
  unit$scaling_factor <- if (scaling.factor != "") scaling.factor
  unit$quantity_kind <- if (quantity.kind != "") quantity.kind
  unit$si_scaling_factor <- if (si.scaling.factor != "") si.scaling.factor
  return(unit)
}
