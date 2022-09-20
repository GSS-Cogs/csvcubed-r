#' creating the configuration
#'
#' Users can use this function to configure the metadata of the configuration, and then add column configurations by using other functions.
#'
#' @param df A dataframe. The dataframe should be in the standard shape, refer to https://gss-cogs.github.io/csvcubed-docs/external/guides/shape-data/#standard-shape.
#' @param id A string. A unique id for the cube.
#' @param title A string. A title for this data set.
#' @param description A string. A longer description of the cube.
#' @param summary  A string. A shorter summary of the data set.
#' @param publisher A string. The publisher of the data set (uri). Can use built-in list "organizations" to auto-fill.
#' @param creator A string. The creator of the data set (uri). Can use built-in list "organizations" to auto-fill.
#' @param themes A vector of strings. The themes of the data set.
#' @param keywords A vector of strings. The keywords of the dataset.
#' @param dataset.issued A String. Date time that the data set was initially published in ISO 8601 format, e.g. 2022-03-31 or 2022-03-31T12:54:30Z.
#' @param dataset.modified A String. Date time that the data set was last modified published in ISO 8601 format, e.g. 2022-03-31 or 2022-03-31T12:54:30Z.
#' @param license A String. URI that represents the copyright license applying to this cube. Can use built-in list "licences" to auto-fill.
#' @param public.contact.point.uri A string. URI that provides a public contact point for discussion of the data set, e.g. mailto:contact.point@example.com.
#' @return A configuration object.
#' @examples
#' #load data
#' data(example.data)
#' #create a configuration for example.data
#' config <- create.config(example.data, title = "sweden_at_eurovision_no_missing",
#'    dataset.issued ="2016-09-01 10:11:12",
#'    publisher = organizations$`ONS Geography Linked Data`,
#'    themes = c("abc", "def"))
#' @export
create.config <- function(df,
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
  if (!inherits(df, "data.frame")) {
    stop("Warning: df must be of data.frame class.")
  }
  if (is.single.string(id)) {
    config$id <- if (id != "") id
  } else {
    stop("Warning: 'id' must be a single nonempty string.")
  }
  if (is.single.string(title)) {
    config$title <- if (title != "") title
  } else {
    stop("Warning: 'title' must be a single nonempty string.")
  }
  if (is.single.string(summary)) {
    config$summary <- if (summary != "") summary
  } else {
    stop("Warning: 'summary' must be a single nonempty string.")
  }
  if (is.valid.uri(publisher)) {
    config$publisher <- publisher
  } else if (publisher != "") {
    stop("Warning: 'publisher' must be a URI.")
  }
  if (is.valid.uri(creator)) {
    config$creator <- creator
  } else if (creator != "") {
    stop("Warning: 'creator' must be a URI .")
  }
  config <- assign.themes(config, themes)
  config <- assign.keywords(config, keywords)
  if (is.single.string(dataset.issued)) {
    config$dataset_issued <- if (dataset.issued != "") anytime::iso8601(anytime::anytime(dataset.issued))
  } else {
    stop("Warning: 'dataset.issued' must be a single nonempty string.")
  }
  if (is.single.string(dataset.modified)) {
    config$dataset_modified <- if (dataset.modified != "") anytime::iso8601(anytime::anytime(dataset.modified))
  } else {
    stop("Warning: 'dataset_modified' must be a single nonempty string.")
  }
  if (is.valid.uri(license)) {
    config$license <- license
  } else if (license != "") {
    stop("Warning: 'license' must be a URI .")
  }
  if (is.valid.uri(public.contact.point.uri)) {
    config$public_contact_point_uri <- public.contact.point.uri
  } else if (public.contact.point.uri != "") {
    stop("Warning: 'public.contact.point.uri' must be a URI.")
  }
  config$column_names <- names(df)
  config$columns <- list()
  return(config)
}

#'private function
#' @noRd
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
                                     measure = "",
                                     unit.values = "") {
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
  config <- assign.column.attribute.uri(config, column.name, "cell_uri_template", cell.uri.template)
  config <- assign.column.attribute.code.list(config, column.name, code.list)
  config <- assign.column.attribute.string(config, column.name, "from_template", from.template)
  config <- assign.column.attribute.string(config, column.name, "data_type", data.type)
  config <- assign.column.attribute.boolean(config, column.name, "required", required)
  config <- assign.column.attribute.values(config, column.name, "values", values)
  config <- assign.column.attribute.measure(config, column.name, "measure", measure)
  config <- assign.column.attribute.unit(config, column.name, "unit", unit)
  config <- assign.column.attribute.unit.values(config, column.name, "values", unit.values)
  return(config)
}


#' Generate the configuration json file to the connection diretory. Returns the json string.
#'
#' @param config An configuration object. The configuration to be serialized into json.
#' @param file A string. The directory path and file name of the json file.
#' @return The json scheme string of the  configuration.
#' @examples
#' data(example.data)
#' config <- create.config(df = example.data,
#'    id = "12341241", title = "sweden_at_eurovision_no_missing",
#'    dataset.issued = "2022-04-08",
#'    keywords = c("Eurovision", "Song Contest", "Sweden", "European Broadcasting Union"))
#' # the following will return the json string and generate a json file
#' # called "config.json" in the current working directory.
#' json.string <- generate.json.configuration(config, "config.json")
#'
#' #remove the file
#' file.remove("config.json")
#' @export
generate.json.configuration <- function(config, file) {
  if (inherits(config,"configuration")) {
    config_list <- unclass(config)
    config_list <- within(config_list, rm("column_names"))
    if (length(config_list$columns) == 0) {
      config_list <- within(config_list, rm("columns"))
    }
    config_json_str <- jsonlite::toJSON(config_list, auto_unbox = T, pretty = T)
    write(config_json_str, file = file)
    return(config_json_str)
  } else {
    stop("config must be of class \"configuration\", try using creat.config().")
  }
}
