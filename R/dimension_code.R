#' Add a dimension attribution column
#'
#' Add a dimension column to the configuration.
#'
#' @param config A configuration object.The configuration object to add the column to.
#' @param column.name String. A column name in the Data Frame.
#' @param label String. Label describing the Dimension.
#' @param description String. A explanation of what the Dimension represents.
#' @param from.existing String. URI of an existing dimension to reuse or extend. Use built-in list Dimensions to auto-fill.
#' @param definition.uri String. Source URI for the definition of the Dimension
#' @param cell.uri.template String. A template used to map the cell values in this column into URIs, e.g. 'http://example.com/code-list/{+column_csvw_name}'
#' @param code.list "A code list defines the unique values permitted in a dimension"
#'    Boolean: Whether or not to generate a code list defining the unique values permitted in this dimension.
#'    String:The URI of a code list defining the unique values permitted in this dimension
#'    String: The path to the code list json defined using the code list config schema.
#' @param from.template String. Dimension column templates. Use built-in list DimensionTemplates to autofill. Also, Users can use add.year.column() and other functions to achieve the same effect.
#' @return A configuration object with the new column added to it.
#' @examples
#' data(example.data)
#' config <- create.config(example.data, title = "sweden_at_eurovision_no_missing")
#' config <- add.dimension.column(config,
#'   column.name = "Year",
#'   from.template = DimensionTemplates$year,
#'   from.existing = Dimensions$timePeriod
#' )
#' @export
add.dimension.column <- function(config,
                                 column.name,
                                 label = "",
                                 description = "",
                                 from.existing = "",
                                 definition.uri = "",
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
    cell.uri.template = cell.uri.template,
    code.list = code.list,
    from.template = from.template
  )
  return(config)
}
