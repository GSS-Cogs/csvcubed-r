#' creating the configuration
#'
#' Users can use this function to configure the metadata of the configuration, and then add column configurations by using other functions.
#'
#' @param df A dataframe. The dataframe should be in the standard shape, refer to https://gss-cogs.github.io/csvcubed-docs/external/guides/shape-data/#standard-shape.
#' @param id A string. A unique id for the cube.
#' @param title A string. A title for this data set.
#' @param description A string. A longer description of the cube.
#' @param summary  A string. A shorter summary of the data set.
#' @param publisher A string. The publisher of the data set (uri). Can use built-in list "Organizations" to auto-fill.
#' @param creator A string. The creator of the data set (uri). Can use built-in list "Organizations" to auto-fill.
#' @param themes A vector of strings. The themes of the data set.
#' @param keywords A vector of strings. The keywords of the dataset.
#' @param dataset.issued A String. Date time that the data set was initially published in anytime::anytime() supported formate. See help(anytime())
#' @param dataset.modified A String. Date time that the data set was last modified published in anytime::anytime() supported formate. See help(anytime())
#' @param license A String. URI that represents the copyright license applying to this cube. Can use built-in list "Licences" to auto-fill.
#' @param public.contact.point.uri A string. URI that provides a public contact point for discussion of the data set, e.g. mailto:contact.point@example.com.
#' @return A configuration object.
#' @examples
#' # load data
#' data(example.data)
#' # create a configuration for example.data
#' config <- create.config(example.data,
#'   title = "Sweden at Eurovision",
#'   summary = "List of Swedish entries to the Eurovision Song Contest since 1958.",
#'   license = "https://creativecommons.org/licenses/by/4.0/",
#'   publisher = Organizations$Open_Knowledge_Foundation,
#'   dataset.issued = "2022-04-08",
#'   keywords = c("Eurovision", "Song Contest", "Sweden", "European Broadcasting Union")
#' )
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
    config$id <- if (!is.empty.string(id)) id
  } else {
    stop("Warning: 'id' must be a single nonempty string.")
  }
  if (is.single.string(title)) {
    config$title <- if (!is.empty.string(title)) title
  } else {
    stop("Warning: 'title' must be a single nonempty string.")
  }
  if (is.single.string(summary)) {
    config$summary <- if (!is.empty.string(summary)) summary
  } else {
    stop("Warning: 'summary' must be a single nonempty string.")
  }
  if (is.valid.uri(publisher)) {
    config$publisher <- publisher
  } else if (!is.empty.string(publisher)) {
    stop("Warning: 'publisher' must be a URI.")
  }
  if (is.valid.uri(creator)) {
    config$creator <- creator
  } else if (!is.empty.string(creator)) {
    stop("Warning: 'creator' must be a URI .")
  }
  config <- assign.themes(config, themes)
  config <- assign.keywords(config, keywords)
  if (is.single.string(dataset.issued)) {
    config$dataset_issued <- if (!is.empty.string(dataset.issued)) anytime::iso8601(anytime::anytime(dataset.issued))
  } else {
    stop("Warning: 'dataset.issued' must be a single nonempty string.")
  }
  if (is.single.string(dataset.modified)) {
    config$dataset_modified <- if (!is.empty.string(dataset.modified)) anytime::iso8601(anytime::anytime(dataset.modified))
  } else {
    stop("Warning: 'dataset_modified' must be a single nonempty string.")
  }
  if (is.valid.uri(license)) {
    config$license <- license
  } else if (!is.empty.string(license)) {
    stop("Warning: 'license' must be a URI .")
  }
  if (is.valid.uri(public.contact.point.uri)) {
    config$public_contact_point_uri <- public.contact.point.uri
  } else if (!is.empty.string(public.contact.point.uri)) {
    stop("Warning: 'public.contact.point.uri' must be a URI.")
  }
  config$column_names <- names(df)
  config$columns <- list()
  return(config)
}

#' private function
#' @noRd
add.column.configuration <- function(config,
                                     column.name,
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
  if (column.name %in% names(config$columns)) {
    stop(paste("Warning: column \"", column.name, "\" is already configured. ", sep = ""))
  }
  if (column.name %in% config$column_names) {
    config$columns[[column.name]] <- list()
  } else {
    stop(paste('Warning: column \"', column.name, '\"is not one of the columns.', sep = ""))
  }
  config <- assign.column.attribute.string(config, column.name, "type", type)
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


#' Generate the configuration json file
#'
#' Generate the configuration json file to the working diretory. Returns the json string.
#' @param config An configuration object. The configuration to be serialized into json.
#' @param file A string. The directory path and file name of the json file.
#' @return The json scheme string of the  configuration.
#' @examples
#' \dontrun{
#' data(example.data)
#' config <- create.config(
#'   df = example.data,
#'   id = "12341241", title = "sweden_at_eurovision_no_missing",
#'   dataset.issued = "2022-04-08",
#'   keywords = c("Eurovision", "Song Contest", "Sweden", "European Broadcasting Union")
#' )
#'
#' # the following will return the json string and generate a json file
#' # called "my-data.json" in the current working directory.
#' generate.json.configuration(config, "my-data.json")
#'
#' # Make sure your csv file has the same name
#' write.csv(example.data, "my-data.csv")
#' }
#' @export
generate.json.configuration <- function(config, file) {
  if (inherits(config, "configuration")) {
    config_list <- unclass(config)
    config_list <- within(config_list, rm("column_names"))
    if (length(config_list$columns) == 0) {
      config_list <- within(config_list, rm("columns"))
    }
    config_json_str <- jsonlite::toJSON(config_list, auto_unbox = T, pretty = T)
    write(config_json_str, file = file)
    return(config_json_str)
  } else {
    stop("config must be of class \"configuration\", try using create.config().")
  }
}
