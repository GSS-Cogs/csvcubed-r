#' private function
#' @noRd
is.single.string <- function(input) {
  is.character(input) & length(input) == 1
}

#' private function
#' @noRd
is.single.number <- function(input) {
  is.numeric(input) & length(input) == 1
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
  } else if (uri == "") {
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
    if (string != "") {
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
  } else if (boolean == "") {
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
  } else if (code.list == "") {
    return(config)
  } else {
    stop("Warning: code.list must be a boolean or a uri.")
  }
  return(config)
}

#' private function
#' @noRd
assign.themes <- function(config, themes) {
  if (is.character(themes)) {
    if (length(themes) > 1) {
      config$themes <- themes
    } else if (themes != "") {
      config$themes <- themes
    }
  } else {
    stop("Warning: 'themes' must be a single nonempty string or a vector of strings.")
  }
  return(config)
}

#' private function
#' @noRd
assign.keywords <- function(config, keywords) {
  if (is.character(keywords)) {
    if (length(keywords) > 1) {
      config$keywords <- keywords
    } else if (keywords != "") {
      config$keywords <- keywords
    }
  } else {
    stop("Warning: 'keywords' must be a single string or a vector of strings.")
  }
  return(config)
}

#' private function
#' @noRd
assign.column.attribute.values <- function(config, column.name, attribute, values) {
  if (rapportools::is.boolean(values)) {
    config$columns[[column.name]][[attribute]] <- values
  } else if (inherits(values,"values")) {
    config$columns[[column.name]][[attribute]] <- unclass(values)
  } else if (values != "") {
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
  } else if (unit.values != "") {
    stop("Warning: values does not meet requirement. Try using unit.values().")
  }
  return(config)
}

#' private function
#' @noRd
assign.column.attribute.measure <- function(config, column.name, attribute, measure) {
  if (inherits(measure,"value")) {
    config$columns[[column.name]][[attribute]] <- unclass(measure)
  } else if (measure != "") {
    stop("Warning: measure does not meet requirement. Try measure.value()")
  }
  return(config)
}

#' private function
#' @noRd
assign.column.attribute.unit <- function(config, column.name, attribute, unit) {
  if (inherits(unit,"unit")) {
    config$columns[[column.name]][[attribute]] <- unclass(unit)
  } else if (unit != "") {
    stop("Warning: unit does not meet requirement. Try unit()")
  }
  return(config)
}
