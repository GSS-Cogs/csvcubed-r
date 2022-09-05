add.resource.attribute.configuration <- function(config,
                                                 column.name = "",
                                                 label = "",
                                                 description = "",
                                                 from.existing = "",
                                                 definition.uri = "",
                                                 values = "",
                                                 cell.uri.template = "",
                                                 required = "",
                                                 from.template = "") {
  config <- add.column.configuration(config,
    column.name = column.name,
    label = label,
    type = "attribute",
    description = description,
    from.existing = from.existing,
    definition.uri = definition.uri,
    values = values,
    cell, uri.template = cell.uri.template,
    required = required,
    from.template = from.template
  )
  return(config)
}

add.literal.attribute.configuration <- function(config,
                                                column.name = "",
                                                label = "",
                                                description = "",
                                                from.existing = "",
                                                definition.uri = "",
                                                data.type = "",
                                                required = "") {
  config <- add.column.configuration(config,
    column.name = column.name,
    type = "attribute",
    label = label,
    description = description,
    from.existing = from.existing,
    definition.uri = definition.uri,
    data.type = data.type,
    required = required
  )
  return(config)
}

add.attribute.configuration <- function(config,
                                        column.name = "",
                                        label = "",
                                        description = "",
                                        from.existing = "",
                                        definition_uri = "",
                                        data.type = "",
                                        required = "",
                                        values = "",
                                        from.template = "",
                                        cell.uri.template = "",
                                        resource.or.literal = "resource") {
  if (resource.or.literal == "resource") {
    config <- add.resource.attribute.configuration(config,
      column.name = column.name,
      label = label,
      description = description,
      from.existing = from.existing,
      definition.uri = definition.uri,
      values = values,
      cell.uri.template = cell.uri.template,
      required = required,
      from.template = from.template
    )
  } else if (resource.or.literal == "literal") {
    config <- add.literal.attribute.configuration(config,
      column.name = column.name,
      label = label,
      description = description,
      from.existing = from.existing,
      definition.uri = definition.uri,
      data.type = data.type,
      required = required
    )
  } else {
    stop("Warning: resource.or.literal can only take 'resource' or 'literal'. ")
  }
  return(config)
}


attribute.value <- function(label, description = "", definition.uri = "", from.existing = "") {
  if (!is.single.string(label) & label == "") {
    stop("Warning: Label must be a single nonempty string.")
  }
  if (!is.single.string(description)) {
    stop("Warning: description must be a single nonempty string.")
  }
  if (!(is_valid_uri(definition.uri) | definition.uri == "")) {
    stop("Warning: definition.uri must be a uri.")
  }
  if (!(is_valid_uri(from.existing) | from.existing == "")) {
    stop("Warning: from.existing must be a uri.")
  }
  value <- list()
  class(value) <- "value"
  value$label <- label
  value$description <- if (description != "") description
  value$definition_uri <- if (definition.uri != "") definition.uri
  value$from_existing <- if (from.existing != "") from.existing
  return(value)
}
