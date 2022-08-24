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

possible.attribute.values <- list()

attribute.values <- function(label, description = "", definition.uri = "", from.existing = "") {
  if (!isSingleString(label) & label != "") {
    stop("Warning: Label must be a single nonempty string.")
  }
  if (!isSingleString(description) & description != "") {
    stop("Warning: description must be a single nonempty string.")
  }
  if (!isSingleString(definition.uri) & is_valid_uri(definition.uri)) {
    stop("Warning: definition.uri must be a uri.")
  }
  unique_identifier <- paste(label, 12312312)
  possible.attribute.values[unique_identifier] <- list(label = label, description = description, definition.uri = definition.uri, from_existing = from.existing)
  return(unique_identifier)
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
  return(config)
}

add.unit.configuration <- function(config,
                                   column.name,
                                   unit.values = "",
){
  config <- add.column.configuration(config, type = "unit", column.name, unit.values)
  return(config)
}

UnitValues <- function(label,
                       description = '',
                       from.existing = "",
                       definition.uri = "",
                       scaling.factor = "",
                       quantitiy.kind = "",
                       si.scaling.factor = "",){
  if(!isSingleString(label)|label==''){
    stop('Warning: label must be single nonempty string!')
  }
  if(!isSingleString(description)|description==''){
    stop("Warning: description must be single nonempty string!")
  }
  if(!is_valid_uri(from.existing)){
    stop("Warning: from.existing must be a uri.")
  }
  if(!is_valid_uri(definition.uri)){
    stop("Warning: definition.uri must be a uri.")
  }
  if(!isSingleNumber(scaling.factor)){
    stop("Warning: scaling.factor must be a single number.")
  }
  if(!is_valid_uri(quantity.kind)){
    stop("Warning: quantity.kind must be a uri.")
  }
  if(!isSingleNumber(si.scaling.factor)){
    stop("Warning: si.scaling.factor must be a single number.")
  }
  return(list(label= label, description = description, from_existing= from.existing,
              definition_uri = definition.uri, scaling_factor = scaling.factor,
              quantitiy_kind = quantitiy.kind, si_scaling_factor =si.scaling.factor))
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
                                     values = "",
                                     unit.values = "") {
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
  config <- assign.column.unit.values(config, column.name, unit.values)
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
