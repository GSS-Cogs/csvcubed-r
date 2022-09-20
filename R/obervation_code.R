#' add a observation column
#'
#' add a observation column to the configuration object.
#'
#' @param config A configuration object.The configuration object to add the column to.
#' @param column.name String. A column name in the Data Frame.
#' @param measure A value object. The characteristic which was measured in all observation. Use measure.value() to create such object for this argument.
#' @param unit A unit object. The unit that all observations were measured in. Use unit() to create such object for this argument.
#' @examples
#' data(example.data)
#' config <- create.config(example.data, title = "sweden_at_eurovision_no_missing")
#' config <-  add.observation.column(config, column.name = "Value")
#' @export
add.observation.column <- function(config,
                                   column.name = "",
                                   measure = "",
                                   unit = "") {
  config <- add.column.configuration(config,
    column.name = column.name,
    measure = measure,
    unit = unit,
    type = "observations"
  )
  return(config)
}
