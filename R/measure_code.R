#' create a value object for filling in the values() function
#'
#' create one measure value object for the value argument in the add.measure.column() function.
#' @param label String.A label describing this measure value
#' @param description String. The description of the measure values
#' @param definition.uri String. The URI link to an existing definition.
#' @param from.existing String. The URI to an existing measure values to extend. Users can use the list Measures for auto-filling.
#' @return a value object storing the measure value information.
#' @examples
#' data(example.data)
#' config <- create.config(example.data, title = "sweden_at_eurovision_no_missing")
#' config <- add.measure.column(config,
#'   column.name = "Measure",
#'   values = values(
#'     measure.value(label = "Final Rank"),
#'     measure.value(label = "Final Points"), measure.value(label = "People on Stage")
#'   )
#' )
#' @export
measure.value <- attribute.value

#' Add a measure column.
#'
#' Add a measure column to the configuration.
#' @param config A configuration object. The configuration object to add the column to.
#' @param column.name String. A column name in the Data Frame.
#' @param values Can be:
#'    boolean: Whether to automatically measure resources for each unique value in the column.
#'    A values object. A list of measures permitted in this column. Created by the values() function filled with measure.value().
#' @return the new configuration object with the measure column added.
#' @examples
#' data(example.data)
#' config <- create.config(example.data, title = "sweden_at_eurovision_no_missing")
#' config <- add.measure.column(config,
#'   column.name = "Measure",
#'   values = values(
#'     measure.value(label = "Final Rank"), measure.value(label = "Final Points"),
#'     measure.value(label = "People on Stage")
#'   )
#' )
#' @export
add.measure.column <- function(config,
                               column.name,
                               values = "") {
  config <- add.column.configuration(
    config = config,
    column.name = column.name,
    values = values,
    type = "measures"
  )
  return(config)
}
