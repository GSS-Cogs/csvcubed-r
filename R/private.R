isSingleString <- function(input) {
  is.character(input) & length(input) == 1
}

isSingleNumber <- function(input){
  is.numberic(input) & length(input) ==1
}

is_valid_uri <- function(string) {
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

assign.column.attribute.uri <- function(config, column.name, attribute, uri) {
  if (is_valid_uri(uri)) {
    eval(str2expression(paste("config$columns$", column.name, "<- append(", "config$columns$", column.name, ", list(", attribute, "='", uri, "'))", sep = "")))
  } else if (uri == "") {
    return(config)
  } else {
    stop(paste("Warning: ", attribute, " must a uri.", sep = ""))
  }
  return(config)
}

assign.column.attribute.string <- function(config, column.name, attribute, string) {
  if (isSingleString(string)) {
    if (string != "") {
      eval(str2expression(paste("config$columns$", column.name, "<- append(", "config$columns$", column.name, ", list(", attribute, "='", string, "'))", sep = "")))
    }
  } else {
    stop(paste("Warning: ", attribute, " must a single nonempty string.", sep = ""))
  }
  return(config)
}

assign.column.attribute.boolean <- function(config, column.name, attribute, boolean) {
  if (rapportools:is.boolean(boolean)) {
    eval(str2expression(paste("config$columns$", column.name, "<- append(", "config$columns$", column.name, ", list(", attribute, "=", boolean, "))", sep = "")))
  } else if (boolean == "") {
    return(config)
  } else {
    stop("Warning: required must be a boolean.")
  }
  return(config)
}

assign.column.code.list <- function(config, column.name, code.list) {
  if (rapportools:is.boolean(code.list)) {
    eval(str2expression(paste("config$columns$", column.name, "<- append(", "config$columns$", column.name, ", list(code_list=", code.list, "'))", sep = "")))
  } else if (is_valid_uri(code.list)) {
    eval(str2expression(paste("config$columns$", column.name, "<- append(", "config$columns$", column.name, ", list(code_list=", code.list, "'))", sep = "")))
  } else if (code.list == "") {
    return(config)
  } else {
    stop("Warning: code.list must be a boolean or a uri.")
  }
  return(config)
}

assign.themes <- function(config, themes) {
  if (is.character(themes)) {
    if (length(themes) > 1) {
      config$themes <- themes
    } else if (themes != "") {
      config$themes <- themes
    } else {
      stop("Warning: 'themes' must be a single nonempty string or a vector of strings.")
    }
  } else {
    stop("Warning: 'themes' must be a single nonempty string or a vector of strings.")
  }
  return(config)
}

assign.keywords <- function(config, keywords) {
  if (is.character(keywords)) {
    if (length(keywords) > 1) {
      config$keywords <- keywords
    } else if (keywords != "") {
      config$keywords <- keywords
    } else {
      stop("Warning: 'keywords' must be a single string or a vector of strings.")
    }
  } else {
    stop("Warning: 'keywords' must be a single string or a vector of strings.")
  }
  return(config)
}


assign.column.values <- function(config, column.name, values) {
  if (rapportools:is.boolean(values) | is.list((values))) {
    eval(str2expression(paste("config$columns$", column.name, "<- append(", "config$columns$", column.name, ", list(values=", values, "'))", sep = "")))
  }
  return(config)
}

