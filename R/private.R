#' private function
#' @noRd
is.single.string <- function(input) {
  is.character(input) && length(input) == 1
}

#' private function
#' @noRd
is.empty.string <- function(input) {
  is.single.string(input) && (input == "")
}

#' private function
#' @noRd
is.single.number <- function(input) {
  is.numeric(input) && length(input) == 1
}

#' private function
#' @noRd
is.valid.uri <- function(string) {
  # https://www.rfc-editor.org/rfc/pdfrfc/rfc2396.txt.pdf
  return(
    length(
      grep(
        "^(([^:/?#]+):)(//([^/?#]*))?([^?#]*)(\\?([^#]*))?(#(.*))?",
        string
      )
    ) > 0
  )
}

#' private function
#' @noRd
assign.column.attribute.uri <- function(config, column.name, attribute, uri) {
  if (is.valid.uri(uri)) {
    config$columns[[column.name]][[attribute]] <- uri
  } else if (is.empty.string(uri)) {
    return(config)
  } else {
    stop(paste("Warning: ", attribute, " must a uri.", sep = ""))
  }
  return(config)
}

#' private function
#' @noRd
assign.column.attribute.string <- function(config, column.name, attribute, string) {
  if (is.single.string(string)) {
    if (!is.empty.string(string)) {
      config$columns[[column.name]][[attribute]] <- string
    }
  } else {
    stop(paste("Warning: ", attribute, " must a single nonempty string.", sep = ""))
  }
  return(config)
}

#' private function
#' @noRd
assign.column.attribute.boolean <- function(config, column.name, attribute, boolean) {
  if (rapportools::is.boolean(boolean)) {
    config$columns[[column.name]][[attribute]] <- boolean
  } else if (is.empty.string(boolean)) {
    return(config)
  } else {
    stop("Warning: required must be a boolean.")
  }
  return(config)
}

#' private function
#' @noRd
assign.column.attribute.code.list <- function(config, column.name, code.list) {
  if (rapportools::is.boolean(code.list)) {
    config$columns[[column.name]][["code_list"]] <- code.list
  } else if (is.valid.uri(code.list)) {
    config$columns[[column.name]][["code_list"]] <- code.list
  } else if (is.empty.string(code.list)) {
    return(config)
  } else {
    stop("Warning: code.list must be a boolean or a uri.")
  }
  return(config)
}

#' private function
#' @noRd
assign.themes <- function(config, themes) {
  if (is.character(themes) && !is.empty.string(themes)) {
    config$themes <- themes
  } else if (!is.empty.string(themes)) {
    stop("Warning: 'themes' must be a single string or a vector of strings.")
  }
  return(config)
}

#' private function
#' @noRd
assign.keywords <- function(config, keywords) {
  if (is.character(keywords) && !is.empty.string(keywords)) {
    config$keywords <- keywords
  } else if (!is.empty.string(keywords)) {
    stop("Warning: 'keywords' must be a single string or a vector of strings.")
  }
  return(config)
}

#' private function
#' @noRd
assign.column.attribute.values <- function(config, column.name, attribute, vs) {
  if (rapportools::is.boolean(vs)) {
    config$columns[[column.name]][[attribute]] <- vs
  } else if (inherits(vs, "values")) {
    config$columns[[column.name]][[attribute]] <- unclass(vs)
  } else if (!is.empty.string(vs)) {
    stop("Warning: values does not meet requirement. Try using values().")
  }
  return(config)
}

#' private function
#' @noRd
assign.column.attribute.unit.values <- function(config, column.name, attribute, unit.values) {
  if (rapportools::is.boolean(unit.values)) {
    config$columns[[column.name]][[attribute]] <- unit.values
  } else if (inherits(unit.values, "unit.values")) {
    config$columns[[column.name]][[attribute]] <- unclass(unit.values)
  } else if (is.character(unit.values) && !is.empty.string(unit.values)) {
    l <- list()
    for (unit in unit.values) {
      if (!is.empty.string(unit)) {
        l <- append(l, list(unclass(attribute.value(label = unit))))
      }
    }
    class(l) <- "unit.values"
    config <- assign.column.attribute.unit.values(config, column.name, attribute, unit.values = l)
  } else if (!is.empty.string(unit.values)) {
    stop("Warning: values does not meet requirement. Try using unit.values().")
  }
  return(config)
}

#' private function
#' @noRd
assign.column.attribute.measure <- function(config, column.name, attribute, measure) {
  if (inherits(measure, "value")) {
    config$columns[[column.name]][[attribute]] <- unclass(measure)
  } else if (is.single.string(measure) && !is.empty.string(measure)) {
    config <- assign.column.attribute.measure(config, column.name, attribute, measure = measure.value(label=measure))
  } else if (!is.empty.string(measure)) {
    stop("Warning: measure does not meet requirement. Try measure.value()")
  }
  return(config)
}

#' private function
#' @noRd
assign.column.attribute.unit <- function(config, column.name, attribute, unit) {
  if (inherits(unit, "unit")) {
    config$columns[[column.name]][[attribute]] <- unclass(unit)
  } else if (is.single.string(unit) && !is.empty.string(unit)) {
    config <- assign.column.attribute.unit(config, column.name, attribute, unit = unit.value(unit))
  } else if (!is.empty.string(unit)) {
    stop("Warning: unit does not meet requirement. Try unit()")
  }
  return(config)
}
