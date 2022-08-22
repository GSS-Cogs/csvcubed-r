config_csv <- function(df,
                       id = "",
                       title = "",
                       description = "",
                       summary = "",
                       publisher = "",
                       creator = "",
                       themes = "",
                       keywords = "",
                       dataset.issued = "",
                       dataset.modified = "",
                       license = "",
                       public.contact.point.uri = "") {
  config <- list()
  class(config) <- "configuration"
  config$`$schema` <- "https://purl.org/csv-cubed/qube-config/v1"
  if (class(df) != "data.frame") {
    stop("Warning: df must be of data.frame class.")
  }
  if (id != "") {
    config$id <- if (isSingleString(id)) id else stop("Warning: 'id' must be a single nonempty string.")
  }
  if (title != "") {
    config$title <- if (isSingleString(title)) title else stop("Warning: 'title' must be a single nonempty string.")
  }
  if (summary != "") {
    config$summary <- if (isSingleString(summary)) summary else stop("Warning: 'summary' must be a single nonempty string.")
  }
  if (publisher != "") {
    config$publisher <- if (isSingleString(publisher) & is_valid_uri(publisher)) publisher else stop("Warning: 'publisher' must be a URL.")
  }
  if (creator != "") {
    config$creator <- if (isSingleString(creator) & is_valid_uri(creator)) creator else stop("Warning: 'creator' must be a URL .")
  }
  config <- assign.themes(config, themes)
  config <- assign.keywords(config, keywords)
  if (dataset.issued != "") {
    config$dataset_issued <- if (isSingleString(dataset.issued)) anytime::iso8601(anytime::anytime(dataset.issued)) else stop("Warning: 'dataset.issued' must be a single nonempty string.")
  }
  if (dataset.modified != "") {
    config$dataset_modified <- if (isSingleString(dataset.modified)) anytime::iso8601(anytime::anytime(dataset.modified)) else stop("Warning: 'dataset_modified' must be a single nonempty string.")
  }
  if (license != "") {
    config$license <- if (isSingleString(license) & is_valid_uri(license)) license else stop("Warning: 'license' must be an URL .")
  }
  if (public.contact.point.uri != "") {
    config$public_contact_point_uri <- if (isSingleString(public.contact.point.uri) & is_valid_uri(public.contact.point.uri)) public.contact.point.uri else print("Warning: 'public.contact.point.uri' must be an URL .")
  }
  config$column_names <- names(df)
  config$columns <- list()
  return(config)
}


add.dimension.configuration <- function(config,
                                        column.name = "",
                                        label = "",
                                        description = "",
                                        from.existing = "",
                                        definition.uri = "",
                                        uri.override = "",
                                        cell.uri.template = "",
                                        code.list = "",
                                        from.template = "") {
  config <- add.column.configuration(config,
    column.name = column.name,
    label = label,
    type = "dimension",
    description = description,
    from.existing = from.existing,
    definition.uri = definition.uri,
    uri.override = uri.override,
    cell.uri.template = cell.uri.template,
    code.list = code.list,
    from.template = from.template
  )
  return(config)
}

add.resource.attribute.configuration <- function(config,
                                                 column.name = "",
                                                 label = "",
                                                 description = "",
                                                 from.existing = "",
                                                 definition.uri = "",
                                                 values = "",
                                                 cell.uri.template = "",
                                                 required = "",
                                                 from.template = "") {
  config <- add.column.configuration(config,
    column.name = column.name,
    label = label,
    type = "attribute",
    description = description,
    from.existing = from.existing,
    definition.uri = definition.uri,
    values = values,
    cell, uri.template = cell.uri.template,
    required = required,
    from.template = from.template
  )
  return(config)
}

add.literal.attribute.configuration <- function(config,
                                                column.name = "",
                                                label = "",
                                                description = "",
                                                from.existing = "",
                                                definition.uri = "",
                                                data.type = "",
                                                required = "") {
  config <- add.column.configuration(config,
    column.name = column.name,
    type = "attribute",
    label = label,
    description = description,
    from.existing = from.existing,
    definition.uri = definition.uri,
    data.type = data.type,
    required = required
  )
  return(config)
}

add.attribute.configuration <- function(config,
                                        column.name = "",
                                        label = "",
                                        description = "",
                                        from.existing = "",
                                        definition_uri = "",
                                        data.type = "",
                                        required = "",
                                        values = "",
                                        from.template = "",
                                        cell.uri.template = "",
                                        resource.or.literal = "resource") {
  if (resource.or.literal == "resource") {
    config <- add.resource.attribute.configuration(config,
      column.name = column.name,
      label = label,
      description = description,
      from.existing = from.existing,
      definition.uri = definition.uri,
      values = values,
      cell.uri.template = cell.uri.template,
      required = required,
      from.template = from.template
    )
  } else if (resource.or.literal == "literal") {
    config <- add.literal.attribute.configuration(config,
      column.name = column.name,
      label = label,
      description = description,
      from.existing = from.existing,
      definition.uri = definition.uri,
      data.type = data.type,
      required = required
    )
  } else {
    stop("Warning: resource.or.literal can only take 'resource' or 'literal'. ")
  }
  return(config)
}

attribute.values <- function(label = "", description = "", definition.uri = "", from.existing = "") {
  if (!isSingleString(label) & label != "") {
    stop("Warning: Label must be a single nonempty string.")
  }
  if (!isSingleString(description) & description != "") {
    stop("Warning: description must be a single nonempty string.")
  }
  if (!isSingleString(definition.uri) & is_valid_uri(definition.uri)) {
    stop("Warning: definition.uri must be a uri.")
  }
  return(list(label = label, description = description, definition.uri = definition.uri, from_existing = from.existing))
}

measure.values <- function(label = "", description = "", definition.uri = "", from.existing = "") {
  if (!isSingleString(label) & label != "") {
    stop("Warning: Label must be a single nonempty string.")
  }
  if (!isSingleString(description) & description != "") {
    stop("Warning: description must be a single nonempty string.")
  }
  if (!isSingleString(definition.uri) & is_valid_uri(definition.uri)) {
    stop("Warning: definition.uri must be a uri.")
  }
  return(list(label = label, description = description, definition.uri = definition.uri, from_existing = from.existing))
}


add.measure.configuration <- function(config,
                                      column.name = "",
                                      cell.uri.template = "",
                                      values = "") {
  config <- add.column.configuration(
    config = config,
    column.name = column.name,
    cell.uri.template = cell.uri.template,
    values = values
  )
  return(config)
}

add.observation.configuration <- function(config,
                                          column.name = "",
                                          measure = "",
                                          unit = "") {
  config <- add.column.configuration(config,
    column.name = column.name,
    measure = measure,
    unit = unit
  )
  config.
}

add.column.configuration <- function(config,
                                     column.name = "",
                                     label = "",
                                     type = "",
                                     description = "",
                                     from.existing = "",
                                     definition.uri = "",
                                     uri.override = "",
                                     cell.uri.template = "",
                                     code.list = "",
                                     from.template = "",
                                     data.type = "",
                                     required = "",
                                     values = "") {
  if (column.name %in% config$column_names) {
    eval(str2expression(paste("config$columns<- append(config$columns, list(", column.name, "= list()", "))", sep = "")))
  } else {
    stop(paste('Warning: label \"', column.name, '\"is not one of the columns.', sep = ""))
  }
  eval(str2expression(paste("config$columns$", column.name, "<- append(", "config$columns$", column.name, ", list(type='", type, "'))", sep = "")))
  config <- assign.column.attribute.string(config, column.name, "description", description)
  config <- assign.column.attribute.uri(config, column.name, "from_existing", from.existing)
  config <- assign.column.attribute.uri(config, column.name, "definition_uri", definition.uri)
  config <- assign.column.attribute.string(config, column.name, "uri_override", uri.override)
  config <- assign.column.attribute.uri(config, column.name, "cell_uri_template", uri.override)
  config <- assign.column.code.list(config, column.name, code.list)
  config <- assign.column.attribute.string(config, column.name, "from_template", from.template)
  config <- assign.column.attribute.string(config, column.name, "data_type", data.type)
  config <- assign.column.attribute.boolean(config, column.name, "required", required)
  config <- assign.column.values(config, column.name, values)
  return(config)
}

generate.json.configuration <- function(config, file = "") {
  config_list <- unclass(config)
  config_list <- within(config_list, rm(column_names))
  if (length(config_list$columns) == 0) {
    config_list <- within(config_list, rm(columns))
  }
  config_json_str <- jsonlite::toJSON(config_list, auto_unbox = T, pretty = T)
  print(config_json_str)
  write(config_json_str, file = file)
}

isSingleString <- function(input) {
  is.character(input) & length(input) == 1
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
