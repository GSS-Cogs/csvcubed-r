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
    config$id <- if (is.single.string(id)) id else stop("Warning: 'id' must be a single nonempty string.")
  }
  if (title != "") {
    config$title <- if (is.single.string(title)) title else stop("Warning: 'title' must be a single nonempty string.")
  }
  if (summary != "") {
    config$summary <- if (is.single.string(summary)) summary else stop("Warning: 'summary' must be a single nonempty string.")
  }
  if (publisher != "") {
    config$publisher <- if (is.single.string(publisher) & is_valid_uri(publisher)) publisher else stop("Warning: 'publisher' must be a URL.")
  }
  if (creator != "") {
    config$creator <- if (is.single.string(creator) & is_valid_uri(creator)) creator else stop("Warning: 'creator' must be a URL .")
  }
  config <- assign.themes(config, themes)
  config <- assign.keywords(config, keywords)
  if (dataset.issued != "") {
    config$dataset_issued <- if (is.single.string(dataset.issued)) anytime::iso8601(anytime::anytime(dataset.issued)) else stop("Warning: 'dataset.issued' must be a single nonempty string.")
  }
  if (dataset.modified != "") {
    config$dataset_modified <- if (is.single.string(dataset.modified)) anytime::iso8601(anytime::anytime(dataset.modified)) else stop("Warning: 'dataset_modified' must be a single nonempty string.")
  }
  if (license != "") {
    config$license <- if (is.single.string(license) & is_valid_uri(license)) license else stop("Warning: 'license' must be an URL .")
  }
  if (public.contact.point.uri != "") {
    config$public_contact_point_uri <- if (is.single.string(public.contact.point.uri) & is_valid_uri(public.contact.point.uri)) public.contact.point.uri else print("Warning: 'public.contact.point.uri' must be an URL .")
  }
  config$column_names <- names(df)
  config$columns <- list()
  return(config)
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
                                     unit = "",
                                     measure = "") {
  if (column.name %in% config$column_names) {
    eval(str2expression(paste("config$columns<- append(config$columns, list(", column.name, "= list()", "))", sep = "")))
  } else {
    stop(paste('Warning: label \"', column.name, '\"is not one of the columns.', sep = ""))
  }
  eval(str2expression(paste("config$columns$", column.name, "<- append(", "config$columns$", column.name, ", list(type='", type, "'))", sep = "")))
  config <- assign.column.attribute.string(config, column.name, "label", label)
  config <- assign.column.attribute.string(config, column.name, "description", description)
  config <- assign.column.attribute.uri(config, column.name, "from_existing", from.existing)
  config <- assign.column.attribute.uri(config, column.name, "definition_uri", definition.uri)
  config <- assign.column.attribute.string(config, column.name, "uri_override", uri.override)
  config <- assign.column.attribute.uri(config, column.name, "cell_uri_template", uri.override)
  config <- assign.column.attribute.code.list(config, column.name, code.list)
  config <- assign.column.attribute.string(config, column.name, "from_template", from.template)
  config <- assign.column.attribute.string(config, column.name, "data_type", data.type)
  config <- assign.column.attribute.boolean(config, column.name, "required", required)
  config <- assign.column.attribute.values(config, column.name, "values", values)
  config <- assign.column.attribute.values(config, column.name, "measure", measure)
  config <- assign.column.attribute.unit.values(config, column.name, "values", unit)
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
