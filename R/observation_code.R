#' add a observation column
#'
#' add a observation column to the configuration object.
#'
#' @param config A configuration object.The configuration object to add the column to.
#' @param column.name String. A column name in the Data Frame.
#' @param measure A value object. The characteristic which was measured in all observation. Use measure.value() to create such object for this argument.
#' @param unit A unit object. The unit that all observations were measured in. Use unit.value() to create such object for this argument.
#' @param data.type String. "The data type of the attribute values". Can use the built-in list "DataTypes" to auto-fill.
#' @examples
#' data(example.data)
#' config <- create.config(example.data, title = "sweden_at_eurovision_no_missing")
#' config <- add.observation.column(config, column.name = "Value")
#' @export
add.observation.column <- function(config,
                                   column.name,
                                   data.type = "",
                                   measure = "",
                                   unit = "") {
  config <- add.column.configuration(config,
    column.name = column.name,
    measure = measure,
    data.type = data.type,
    unit = unit,
    type = "observations"
  )
  return(config)
}
