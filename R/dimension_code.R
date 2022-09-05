add.dimension.configuration <- function(config,
                                        column.name = "",
                                        label = "",
                                        description = "",
                                        from.existing = "",
                                        definition.uri = "",
                                        uri.override = "",
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
    uri.override = uri.override,
    cell.uri.template = cell.uri.template,
    code.list = code.list,
    from.template = from.template
  )
  return(config)
}
