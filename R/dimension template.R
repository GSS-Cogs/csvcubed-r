add.year.column <- function(config, column.name, label = "", description = "", from.existing = "", definition.uri = "", uri.override = "", cell.uri.template = "", code.list = "") {
  config <- add.dimension.configuration(config, column.name, label, description, from.existing, definition.uri, uri.override, cell.uri.template, code.list, from.template = "year")
  return(config)
}

add.halfyear.column <- function(config, column.name, label = "", description = "", from.existing = "", definition.uri = "", uri.override = "", cell.uri.template = "", code.list = "") {
  config <- add.dimension.configuration(config, column.name, label, description, from.existing, definition.uri, uri.override, cell.uri.template, code.list, from.template = "half-year")
  return(config)
}

add.quarter.column <- function(config, column.name, label = "", description = "", from.existing = "", definition.uri = "", uri.override = "", cell.uri.template = "", code.list = "") {
  config <- add.dimension.configuration(config, column.name, label, description, from.existing, definition.uri, uri.override, cell.uri.template, code.list, from.template = "quarter")
  return(config)
}

add.month.column <- function(config, column.name, label = "", description = "", from.existing = "", definition.uri = "", uri.override = "", cell.uri.template = "", code.list = "") {
  config <- add.dimension.configuration(config, column.name, label, description, from.existing, definition.uri, uri.override, cell.uri.template, code.list, from.template = "month")
  return(config)
}

add.week.column <- function(config, column.name, label = "", description = "", from.existing = "", definition.uri = "", uri.override = "", cell.uri.template = "", code.list = "") {
  config <- add.dimension.configuration(config, column.name, label, description, from.existing, definition.uri, uri.override, cell.uri.template, code.list, from.template = "week")
  return(config)
}

add.day.column <- function(config, column.name, label = "", description = "", from.existing = "", definition.uri = "", uri.override = "", cell.uri.template = "", code.list = "") {
  config <- add.dimension.configuration(config, column.name, label, description, from.existing, definition.uri, uri.override, cell.uri.template, code.list, from.template = "day")
  return(config)
}

add.hour.column <- function(config, column.name, label = "", description = "", from.existing = "", definition.uri = "", uri.override = "", cell.uri.template = "", code.list = "") {
  config <- add.dimension.configuration(config, column.name, label, description, from.existing, definition.uri, uri.override, cell.uri.template, code.list, from.template = "hour")
  return(config)
}

add.minute.column <- function(config, column.name, label = "", description = "", from.existing = "", definition.uri = "", uri.override = "", cell.uri.template = "", code.list = "") {
  config <- add.dimension.configuration(config, column.name, label, description, from.existing, definition.uri, uri.override, cell.uri.template, code.list, from.template = "minute")
  return(config)
}

add.second.column <- function(config, column.name, label = "", description = "", from.existing = "", definition.uri = "", uri.override = "", cell.uri.template = "", code.list = "") {
  config <- add.dimension.configuration(config, column.name, label, description, from.existing, definition.uri, uri.override, cell.uri.template, code.list, from.template = "second")
  return(config)
}

add.governmentyear.column <- function(config, column.name, label = "", description = "", from.existing = "", definition.uri = "", uri.override = "", cell.uri.template = "", code.list = "") {
  config <- add.dimension.configuration(config, column.name, label, description, from.existing, definition.uri, uri.override, cell.uri.template, code.list, from.template = "government-year")
  return(config)
}

add.governmenthalfyear.column <- function(config, column.name, label = "", description = "", from.existing = "", definition.uri = "", uri.override = "", cell.uri.template = "", code.list = "") {
  config <- add.dimension.configuration(config, column.name, label, description, from.existing, definition.uri, uri.override, cell.uri.template, code.list, from.template = "government-half-year")
  return(config)
}

add.governmentquarter.column <- function(config, column.name, label = "", description = "", from.existing = "", definition.uri = "", uri.override = "", cell.uri.template = "", code.list = "") {
  config <- add.dimension.configuration(config, column.name, label, description, from.existing, definition.uri, uri.override, cell.uri.template, code.list, from.template = "government-quarter")
  return(config)
}

add.governmentweek.column <- function(config, column.name, label = "", description = "", from.existing = "", definition.uri = "", uri.override = "", cell.uri.template = "", code.list = "") {
  config <- add.dimension.configuration(config, column.name, label, description, from.existing, definition.uri, uri.override, cell.uri.template, code.list, from.template = "government-week")
  return(config)
}

add.gregorianinstant.column <- function(config, column.name, label = "", description = "", from.existing = "", definition.uri = "", uri.override = "", cell.uri.template = "", code.list = "") {
  config <- add.dimension.configuration(config, column.name, label, description, from.existing, definition.uri, uri.override, cell.uri.template, code.list, from.template = "gregorian-instant")
  return(config)
}
