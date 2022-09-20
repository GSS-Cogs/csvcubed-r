#' add a resource attribute column
#'
#' add a resource type attribute column to be included in the configuration object.
#'
#' @param config A configuration object. The column configuration will be added to the configuration object.
#' @param column.name String. A column name in the dataframe that the configuration object corresponds.
#' @param label String. Label describing the Attribute.
#' @param description String. A explanation of what the Attribute represents.
#' @param from.existing String. URI of an existing attribute to reuse or extend. Can use the built-in list "attribute.from.existing" to auto-fill.
#' @param definition.uri String. Source URI for the definition of the Attribute.
#' @param values Can  be:
#'    boolean: Whether to automatically generate attribute value resources for each unique value in the column.
#'    A "values" object. Created by the values() function filled with attribute.value(). The distinct attribute values which can be used in the column
#' @param from.template String. Attribute template uris.
#' @param required Boolean. Whether or not it is required that every observation has a value for this attribute.
#' @return A configuration object with the new column added to it.
#' @examples
#' data(example.data)
#' config <- create.config(example.data, title = "sweden_at_eurovision_no_missing")
#' config <- add.resource.attribute.column(config, column.name = "Status", label = "status",
#'    values = values(attribute.value(label = "Final"), attribute.value(label = "Provisional")))
#'
#' @export
add.resource.attribute.column <- function(config,
                                          column.name = "",
                                          label = "",
                                          description = "",
                                          from.existing = "",
                                          definition.uri = "",
                                          values = "",
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
    required = required,
    from.template = from.template
  )
  return(config)
}

#' add a literal attribution column
#'
#' add a literal type attribute column to be included in the configuration object.
#'
#' @param config A configuration object. The column configuration will be added to the configuration object.
#' @param column.name String. A column name in the dataframe that the configuration object corresponds.
#' @param label String. Label describing the Attribute.
#' @param description String. A explanation of what the Attribute represents.
#' @param from.existing String. URI of an existing attribute to reuse or extend. Can use the built-in list "attribute.from.existing" to auto-fill.
#' @param definition.uri String. Source URI for the definition of the Attribute.
#' @param required Boolean. Whether or not it is required that every observation has a value for this attribute.
#' @param data.type String. "The data type of the attribute values"
#' @return A configuration object with the new column added to it.
#' @examples
#' data(example.data)
#' config <- create.config(example.data, title = "sweden_at_eurovision_no_missing")
#' config <- add.literal.attribute.column(config, column.name = "Status", label = "status")
#' @export
add.literal.attribute.column <- function(config,
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

#' create a value object for filling in the values() function
#'
#' create one attribute value object for the value argument in the add.resource.attribute.column() function.
#' @param label String.A label describing this attribute value
#' @param description String. The description of the attribute values
#' @param definition.uri String. The URI link to an existing definition.
#' @param from.existing String. The URI to an existing attribute values to extend. Users can use the list attribute.value.from.existing for auto-filling.
#'
#' @return a value object storing the attribute value information.
#' @export
attribute.value <- function(label, description = "", definition.uri = "", from.existing = "") {
  if (!is.single.string(label) & label == "") {
    stop("Warning: Label must be a single nonempty string.")
  }
  if (!is.single.string(description)) {
    stop("Warning: description must be a single nonempty string.")
  }
  if (!(is.valid.uri(definition.uri) | definition.uri == "")) {
    stop("Warning: definition.uri must be a uri.")
  }
  if (!(is.valid.uri(from.existing) | from.existing == "")) {
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
