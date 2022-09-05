add.unit.configuration <- function(config,
                                   column.name,
                                   unit = "") {
  config <- add.column.configuration(config, type = "unit", column.name = column.name, unit = unit)
  return(config)
}

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
  if (!is_valid_uri(from.existing) & from.existing != "") {
    stop("Warning: from.existing must be a uri.")
  }
  if (!is_valid_uri(definition.uri) & definition.uri != "") {
    stop("Warning: definition.uri must be a uri.")
  }
  if (!is.single.number(scaling.factor) & scaling.factor != "") {
    stop("Warning: scaling.factor must be a single number.")
  }
  if (!is_valid_uri(quantity.kind) & quantity.kind != "") {
    stop("Warning: quantity.kind must be a uri.")
  }
  if (!is.single.number(si.scaling.factor) & si.scaling.factor != "") {
    stop("Warning: si.scaling.factor must be a single number.")
  }
  unit <- list()
  class(unit) <- "unit"
  unit$label <- label
  unit$description <- if (description != "") description
  unit$from_existing <- if (from.existing != "") from.existing
  unit$definition_uri <- if (definition.uri != "") definition.uri
  unit$scaling_factor <- if (scaling.factor != "") scaling.factor
  unit$quantity_kind <- if (quantity.kind != "") quantity.kind
  unit$si_scaling_factor <- if (si.scaling.factor != "") si.scaling.factor
  return(unit)
}
