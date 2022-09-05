measure.value <- function(label, description = "", definition.uri = "", from.existing = "") {
  return(attribute.value(label = label, description = description, definition.uri = definition.uri, from.existing = from.existing))
}


add.measure.configuration <- function(config,
                                      column.name = "",
                                      cell.uri.template = "",
                                      values = "") {
  config <- add.column.configuration(
    config = config,
    column.name = column.name,
    cell.uri.template = cell.uri.template,
    values = values,
    type = "measures"
  )
  return(config)
}
