#' Add a year dimension attribution column
#'
#' Add a year dimension column to the configuration.
#'
#' @param config A configuration ob.The configuration object to add the column to.
#' @param column.name String. A column name in the Data Frame.
#' @param label String. Label describing the Dimension.
#' @param description String. A explanation of what the Dimension represents.
#' @param from.existing String. URI of an existing dimension to reuse or extend. Use list dimension.from.existing to auto-fill.
#' @param definition.uri String. Source URI for the definition of the Dimension
#' @param cell.uri.template String. A template used to map the cell values in this column into URIs, e.g. 'http://example.com/code-list/{+column_csvw_name}'
#' @param code.list "A code list defines the unique values permitted in a dimension"
#'   Boolean: Whether or not to generate a code list defining the unique values permitted in this dimension.
#'   String:The URI of a code list defining the unique values permitted in this dimension
#'   String: The path to the code list json defined using the code list config schema.
#'
#' @return A configuration object with the new year column added to it.
#' @export
add.year.column <- function(config, column.name, label = "", description = "", from.existing = "", definition.uri = "", cell.uri.template = "", code.list = "") {
  config <- add.dimension.column(config = config, column.name = column.name, label = label, description = description, from.existing = from.existing, definition.uri = definition.uri, cell.uri.template = cell.uri.template, code.list = code.list, from.template = "year")
  return(config)
}

#' Add a halfyear dimension attribution column
#'
#' Add a halfyear dimension column to the configuration.
#'
#' @param config A configuration ob.The configuration object to add the column to.
#' @param column.name String. A column name in the Data Frame.
#' @param label String. Label describing the Dimension.
#' @param description String. A explanation of what the Dimension represents.
#' @param from.existing String. URI of an existing dimension to reuse or extend. Use list dimension.from.existing to auto-fill.
#' @param definition.uri String. Source URI for the definition of the Dimension
#' @param cell.uri.template String. A template used to map the cell values in this column into URIs, e.g. 'http://example.com/code-list/{+column_csvw_name}'
#' @param code.list "A code list defines the unique values permitted in a dimension"
#'   Boolean: Whether or not to generate a code list defining the unique values permitted in this dimension.
#'   String:The URI of a code list defining the unique values permitted in this dimension
#'   String: The path to the code list json defined using the code list config schema.
#'
#' @return A configuration object with the new halfyear column added to it.
#' @export
add.halfyear.column <- function(config, column.name, label = "", description = "", from.existing = "", definition.uri = "", cell.uri.template = "", code.list = "") {
  config <- add.dimension.column(config = config, column.name = column.name, label = label, description = description, from.existing = from.existing, definition.uri = definition.uri, cell.uri.template = cell.uri.template, code.list = code.list, from.template = "half-year")
  return(config)
}

#' Add a quarter dimension attribution column
#'
#' Add a quarter dimension column to the configuration.
#'
#' @param config A configuration ob.The configuration object to add the column to.
#' @param column.name String. A column name in the Data Frame.
#' @param label String. Label describing the Dimension.
#' @param description String. A explanation of what the Dimension represents.
#' @param from.existing String. URI of an existing dimension to reuse or extend. Use list dimension.from.existing to auto-fill.
#' @param definition.uri String. Source URI for the definition of the Dimension
#' @param cell.uri.template String. A template used to map the cell values in this column into URIs, e.g. 'http://example.com/code-list/{+column_csvw_name}'
#' @param code.list "A code list defines the unique values permitted in a dimension"
#'   Boolean: Whether or not to generate a code list defining the unique values permitted in this dimension.
#'   String:The URI of a code list defining the unique values permitted in this dimension
#'   String: The path to the code list json defined using the code list config schema.
#'
#' @return A configuration object with the new quarter column added to it.
#' @export
add.quarter.column <- function(config, column.name, label = "", description = "", from.existing = "", definition.uri = "", cell.uri.template = "", code.list = "") {
  config <- add.dimension.column(config = config, column.name = column.name, label = label, description = description, from.existing = from.existing, definition.uri = definition.uri, cell.uri.template = cell.uri.template, code.list = code.list, from.template = "quarter")
  return(config)
}

#' Add a month dimension attribution column
#'
#' Add a month dimension column to the configuration.
#'
#' @param config A configuration ob.The configuration object to add the column to.
#' @param column.name String. A column name in the Data Frame.
#' @param label String. Label describing the Dimension.
#' @param description String. A explanation of what the Dimension represents.
#' @param from.existing String. URI of an existing dimension to reuse or extend. Use list dimension.from.existing to auto-fill.
#' @param definition.uri String. Source URI for the definition of the Dimension
#' @param cell.uri.template String. A template used to map the cell values in this column into URIs, e.g. 'http://example.com/code-list/{+column_csvw_name}'
#' @param code.list "A code list defines the unique values permitted in a dimension"
#'   Boolean: Whether or not to generate a code list defining the unique values permitted in this dimension.
#'   String:The URI of a code list defining the unique values permitted in this dimension
#'   String: The path to the code list json defined using the code list config schema.
#'
#' @return A configuration object with the new month column added to it.
#' @export
add.month.column <- function(config, column.name, label = "", description = "", from.existing = "", definition.uri = "", cell.uri.template = "", code.list = "") {
  config <- add.dimension.column(config = config, column.name = column.name, label = label, description = description, from.existing = from.existing, definition.uri = definition.uri, cell.uri.template = cell.uri.template, code.list = code.list, from.template = "month")
  return(config)
}

#' Add a week dimension attribution column
#'
#' Add a week dimension column to the configuration.
#'
#' @param config A configuration ob.The configuration object to add the column to.
#' @param column.name String. A column name in the Data Frame.
#' @param label String. Label describing the Dimension.
#' @param description String. A explanation of what the Dimension represents.
#' @param from.existing String. URI of an existing dimension to reuse or extend. Use list dimension.from.existing to auto-fill.
#' @param definition.uri String. Source URI for the definition of the Dimension
#' @param cell.uri.template String. A template used to map the cell values in this column into URIs, e.g. 'http://example.com/code-list/{+column_csvw_name}'
#' @param code.list "A code list defines the unique values permitted in a dimension"
#'   Boolean: Whether or not to generate a code list defining the unique values permitted in this dimension.
#'   String:The URI of a code list defining the unique values permitted in this dimension
#'   String: The path to the code list json defined using the code list config schema.
#'
#' @return A configuration object with the new week column added to it.
#' @export
add.week.column <- function(config, column.name, label = "", description = "", from.existing = "", definition.uri = "", cell.uri.template = "", code.list = "") {
  config <- add.dimension.column(config = config, column.name = column.name, label = label, description = description, from.existing = from.existing, definition.uri = definition.uri, cell.uri.template = cell.uri.template, code.list = code.list, from.template = "week")
  return(config)
}

#' Add a day dimension attribution column
#'
#' Add a day dimension column to the configuration.
#'
#' @param config A configuration ob.The configuration object to add the column to.
#' @param column.name String. A column name in the Data Frame.
#' @param label String. Label describing the Dimension.
#' @param description String. A explanation of what the Dimension represents.
#' @param from.existing String. URI of an existing dimension to reuse or extend. Use list dimension.from.existing to auto-fill.
#' @param definition.uri String. Source URI for the definition of the Dimension
#' @param cell.uri.template String. A template used to map the cell values in this column into URIs, e.g. 'http://example.com/code-list/{+column_csvw_name}'
#' @param code.list "A code list defines the unique values permitted in a dimension"
#'   Boolean: Whether or not to generate a code list defining the unique values permitted in this dimension.
#'   String:The URI of a code list defining the unique values permitted in this dimension
#'   String: The path to the code list json defined using the code list config schema.
#'
#' @return A configuration object with the new day column added to it.
#' @export
add.day.column <- function(config, column.name, label = "", description = "", from.existing = "", definition.uri = "", cell.uri.template = "", code.list = "") {
  config <- add.dimension.column(config = config, column.name = column.name, label = label, description = description, from.existing = from.existing, definition.uri = definition.uri, cell.uri.template = cell.uri.template, code.list = code.list, from.template = "day")
  return(config)
}

#' Add a hour dimension attribution column
#'
#' Add a hour dimension column to the configuration.
#'
#' @param config A configuration ob.The configuration object to add the column to.
#' @param column.name String. A column name in the Data Frame.
#' @param label String. Label describing the Dimension.
#' @param description String. A explanation of what the Dimension represents.
#' @param from.existing String. URI of an existing dimension to reuse or extend. Use list dimension.from.existing to auto-fill.
#' @param definition.uri String. Source URI for the definition of the Dimension
#' @param cell.uri.template String. A template used to map the cell values in this column into URIs, e.g. 'http://example.com/code-list/{+column_csvw_name}'
#' @param code.list "A code list defines the unique values permitted in a dimension"
#'   Boolean: Whether or not to generate a code list defining the unique values permitted in this dimension.
#'   String:The URI of a code list defining the unique values permitted in this dimension
#'   String: The path to the code list json defined using the code list config schema.
#'
#' @return A configuration object with the new hour column added to it.
#' @export
add.hour.column <- function(config, column.name, label = "", description = "", from.existing = "", definition.uri = "", cell.uri.template = "", code.list = "") {
  config <- add.dimension.column(config =config, column.name = column.name, label = label, description= description, from.existing =from.existing, definition.uri =definition.uri,  cell.uri.template = cell.uri.template, code.list = code.list, from.template = "hour")
  return(config)
}

#' Add a minute dimension attribution column
#'
#' Add a minute dimension column to the configuration.
#'
#' @param config A configuration ob.The configuration object to add the column to.
#' @param column.name String. A column name in the Data Frame.
#' @param label String. Label describing the Dimension.
#' @param description String. A explanation of what the Dimension represents.
#' @param from.existing String. URI of an existing dimension to reuse or extend. Use list dimension.from.existing to auto-fill.
#' @param definition.uri String. Source URI for the definition of the Dimension
#' @param cell.uri.template String. A template used to map the cell values in this column into URIs, e.g. 'http://example.com/code-list/{+column_csvw_name}'
#' @param code.list "A code list defines the unique values permitted in a dimension"
#'   Boolean: Whether or not to generate a code list defining the unique values permitted in this dimension.
#'   String:The URI of a code list defining the unique values permitted in this dimension
#'   String: The path to the code list json defined using the code list config schema.
#'
#' @return A configuration object with the new minute column added to it.
#' @export
add.minute.column <- function(config, column.name, label = "", description = "", from.existing = "", definition.uri = "", cell.uri.template = "", code.list = "") {
  config <- add.dimension.column(config = config, column.name = column.name, label = label, description = description, from.existing = from.existing, definition.uri = definition.uri, cell.uri.template = cell.uri.template, code.list = code.list, from.template = "minute")
  return(config)
}

#' Add a second dimension attribution column
#'
#' Add a second dimension column to the configuration.
#'
#' @param config A configuration ob.The configuration object to add the column to.
#' @param column.name String. A column name in the Data Frame.
#' @param label String. Label describing the Dimension.
#' @param description String. A explanation of what the Dimension represents.
#' @param from.existing String. URI of an existing dimension to reuse or extend. Use list dimension.from.existing to auto-fill.
#' @param definition.uri String. Source URI for the definition of the Dimension
#' @param cell.uri.template String. A template used to map the cell values in this column into URIs, e.g. 'http://example.com/code-list/{+column_csvw_name}'
#' @param code.list "A code list defines the unique values permitted in a dimension"
#'   Boolean: Whether or not to generate a code list defining the unique values permitted in this dimension.
#'   String:The URI of a code list defining the unique values permitted in this dimension
#'   String: The path to the code list json defined using the code list config schema.
#'
#' @return A configuration object with the new second column added to it.
#' @export
add.second.column <- function(config, column.name, label = "", description = "", from.existing = "", definition.uri = "", cell.uri.template = "", code.list = "") {
  config <- add.dimension.column(config = config, column.name = column.name, label = label, description = description, from.existing = from.existing, definition.uri = definition.uri, cell.uri.template = cell.uri.template, code.list = code.list, from.template = "second")
  return(config)
}

#' Add a government year dimension attribution column
#'
#' Add a government year dimension column to the configuration.
#'
#' @param config A configuration ob.The configuration object to add the column to.
#' @param column.name String. A column name in the Data Frame.
#' @param label String. Label describing the Dimension.
#' @param description String. A explanation of what the Dimension represents.
#' @param from.existing String. URI of an existing dimension to reuse or extend. Use list dimension.from.existing to auto-fill.
#' @param definition.uri String. Source URI for the definition of the Dimension
#' @param cell.uri.template String. A template used to map the cell values in this column into URIs, e.g. 'http://example.com/code-list/{+column_csvw_name}'
#' @param code.list "A code list defines the unique values permitted in a dimension"
#'   Boolean: Whether or not to generate a code list defining the unique values permitted in this dimension.
#'   String:The URI of a code list defining the unique values permitted in this dimension
#'   String: The path to the code list json defined using the code list config schema.
#'
#' @return A configuration object with the new government year column added to it.
#' @export
add.governmentyear.column <- function(config, column.name, label = "", description = "", from.existing = "", definition.uri = "", cell.uri.template = "", code.list = "") {
  config <- add.dimension.column(config = config, column.name = column.name, label = label, description = description, from.existing = from.existing, definition.uri = definition.uri, cell.uri.template = cell.uri.template, code.list = code.list, from.template = "government-year")
  return(config)
}

#' Add a government half year dimension attribution column
#'
#' Add a government half year dimension column to the configuration.
#'
#' @param config A configuration ob.The configuration object to add the column to.
#' @param column.name String. A column name in the Data Frame.
#' @param label String. Label describing the Dimension.
#' @param description String. A explanation of what the Dimension represents.
#' @param from.existing String. URI of an existing dimension to reuse or extend. Use list dimension.from.existing to auto-fill.
#' @param definition.uri String. Source URI for the definition of the Dimension
#' @param cell.uri.template String. A template used to map the cell values in this column into URIs, e.g. 'http://example.com/code-list/{+column_csvw_name}'
#' @param code.list "A code list defines the unique values permitted in a dimension"
#'   Boolean: Whether or not to generate a code list defining the unique values permitted in this dimension.
#'   String:The URI of a code list defining the unique values permitted in this dimension
#'   String: The path to the code list json defined using the code list config schema.
#'
#' @return A configuration object with the new government half year column added to it.
#' @export
add.governmenthalfyear.column <- function(config, column.name, label = "", description = "", from.existing = "", definition.uri = "", cell.uri.template = "", code.list = "") {
  config <- add.dimension.column(config = config, column.name = column.name, label = label, description = description, from.existing = from.existing, definition.uri = definition.uri, cell.uri.template = cell.uri.template, code.list = code.list, from.template = "government-half-year")
  return(config)
}

#' Add a government quarter dimension attribution column
#'
#' Add a government quarter dimension column to the configuration.
#'
#' @param config A configuration ob.The configuration object to add the column to.
#' @param column.name String. A column name in the Data Frame.
#' @param label String. Label describing the Dimension.
#' @param description String. A explanation of what the Dimension represents.
#' @param from.existing String. URI of an existing dimension to reuse or extend. Use list dimension.from.existing to auto-fill.
#' @param definition.uri String. Source URI for the definition of the Dimension
#' @param cell.uri.template String. A template used to map the cell values in this column into URIs, e.g. 'http://example.com/code-list/{+column_csvw_name}'
#' @param code.list "A code list defines the unique values permitted in a dimension"
#'   Boolean: Whether or not to generate a code list defining the unique values permitted in this dimension.
#'   String:The URI of a code list defining the unique values permitted in this dimension
#'   String: The path to the code list json defined using the code list config schema.
#'
#' @return A configuration object with the new government quarter column added to it.
#' @export
add.governmentquarter.column <- function(config, column.name, label = "", description = "", from.existing = "", definition.uri = "", cell.uri.template = "", code.list = "") {
  config <- add.dimension.column(config = config, column.name = column.name, label = label, description = description, from.existing = from.existing, definition.uri = definition.uri, cell.uri.template = cell.uri.template, code.list = code.list, from.template = "government-quarter")
  return(config)
}

#' Add a government week dimension attribution column
#'
#' Add a government week dimension column to the configuration.
#'
#' @param config A configuration ob.The configuration object to add the column to.
#' @param column.name String. A column name in the Data Frame.
#' @param label String. Label describing the Dimension.
#' @param description String. A explanation of what the Dimension represents.
#' @param from.existing String. URI of an existing dimension to reuse or extend. Use list dimension.from.existing to auto-fill.
#' @param definition.uri String. Source URI for the definition of the Dimension
#' @param cell.uri.template String. A template used to map the cell values in this column into URIs, e.g. 'http://example.com/code-list/{+column_csvw_name}'
#' @param code.list "A code list defines the unique values permitted in a dimension"
#'   Boolean: Whether or not to generate a code list defining the unique values permitted in this dimension.
#'   String:The URI of a code list defining the unique values permitted in this dimension
#'   String: The path to the code list json defined using the code list config schema.
#'
#' @return A configuration object with the new government week column added to it.
#' @export
add.governmentweek.column <- function(config, column.name, label = "", description = "", from.existing = "", definition.uri = "", cell.uri.template = "", code.list = "") {
  config <- add.dimension.column(config = config, column.name = column.name, label = label, description = description, from.existing = from.existing, definition.uri = definition.uri, cell.uri.template = cell.uri.template, code.list = code.list, from.template = "government-week")
  return(config)
}

#' Add a gregorian instant dimension attribution column
#'
#' Add a gregorian instant dimension column to the configuration.
#'
#' @param config A configuration ob.The configuration object to add the column to.
#' @param column.name String. A column name in the Data Frame.
#' @param label String. Label describing the Dimension.
#' @param description String. A explanation of what the Dimension represents.
#' @param from.existing String. URI of an existing dimension to reuse or extend. Use list dimension.from.existing to auto-fill.
#' @param definition.uri String. Source URI for the definition of the Dimension
#' @param cell.uri.template String. A template used to map the cell values in this column into URIs, e.g. 'http://example.com/code-list/{+column_csvw_name}'
#' @param code.list "A code list defines the unique values permitted in a dimension"
#'   Boolean: Whether or not to generate a code list defining the unique values permitted in this dimension.
#'   String:The URI of a code list defining the unique values permitted in this dimension
#'   String: The path to the code list json defined using the code list config schema.
#'
#' @return A configuration object with the new gregorian instant column added to it.
#' @export
add.gregorianinstant.column <- function(config, column.name, label = "", description = "", from.existing = "", definition.uri = "", cell.uri.template = "", code.list = "") {
  config <- add.dimension.column(config = config, column.name = column.name, label = label, description = description, from.existing = from.existing, definition.uri = definition.uri, cell.uri.template = cell.uri.template, code.list = code.list, from.template = "gregorian-instant")
  return(config)
}
