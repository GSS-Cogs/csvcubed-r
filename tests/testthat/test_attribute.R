


test_that("resource attribute completeness test", {
  config <- create.config(df = example.data, id = "sweden_at_eurovision_no_missing") %>%
    add.resource.attribute.column(
      label = "status",
      column.name = "Status",
      description = "the status",
      from.existing = Attributes$embargoTime,
      definition.uri = "foo://example.com:8042/over/there?name=ferret#nose",
      values = values(attribute.value(label = "Final", description = "final", definition.uri = "foo://example.com:8042/over/there?name=ferret#nose", from.existing = AttributeValues$estimated))
    )
  json_string <- generate.json.configuration(config, file = "config.json")
  validation_errors <- validate_qube_config(json_string)
  expect_length(validation_errors, 0)
})

test_that("literal attribute completeness test", {
  config <- create.config(df = example.data, id = "sweden_at_eurovision_no_missing") %>%
    add.resource.attribute.column(
      label = "status",
      column.name = "Status",
      description = "the status",
      from.existing = Attributes$embargoTime,
      definition.uri = "foo://example.com:8042/over/there?name=ferret#nose",
      values = T,
      required = T
    )
  json_string <- generate.json.configuration(config, file = "config.json")
  validation_errors <- validate_qube_config(json_string)
  expect_length(validation_errors, 0)
})

test_that("URI checking test", {
  config <- create.config(df = example.data, id = "sweden_at_eurovision_no_missing")
  expect_error(add.resource.attribute.column(config,
    label = "status",
    column.name = "Status",
    from.existing = "hello"
  ), "Warning: from_existing must a uri.")
})

test_that("value checking test", {
  config <- create.config(df = example.data, id = "sweden_at_eurovision_no_missing")
  expect_error(add.resource.attribute.column(config,
    label = "status",
    column.name = "Status",
    values = 134
  ), "Warning: values does not meet requirement. Try using values().")
})

test_that("required argument test", {
  config <- create.config(df = example.data, id = "sweden_at_eurovision_no_missing")
  expect_error(add.literal.attribute.column(config,
    label = "status",
    column.name = "Status",
    description = "the status",
    from.existing = Attributes$embargoTime,
    definition.uri = "foo://example.com:8042/over/there?name=ferret#nose",
    data.type = "int",
    required = 123432412
  ), "Warning: required must be a boolean.")
})


test_that("values function string test", {
  config <- create.config(df = example.data, id = "sweden_at_eurovision_no_missing") %>%
    add.resource.attribute.column(
      label = "status",
      column.name = "Status",
      description = "the status",
      from.existing = Attributes$embargoTime,
      definition.uri = "foo://example.com:8042/over/there?name=ferret#nose",
      values = values("Final", "Provisional")
    )
  json_string <- generate.json.configuration(config, file = "config.json")
  validation_errors <- validate_qube_config(json_string)
  expect_length(validation_errors, 0)
})

test_that("values function mixed types test", {
  config <- create.config(df = example.data, id = "sweden_at_eurovision_no_missing") %>%
    add.resource.attribute.column(
      label = "status",
      column.name = "Status",
      description = "the status",
      from.existing = Attributes$embargoTime,
      definition.uri = "foo://example.com:8042/over/there?name=ferret#nose",
      values = values("Final", attribute.value("Provisional"))
    )
  json_string <- generate.json.configuration(config, file = "config.json")
  validation_errors <- validate_qube_config(json_string)
  expect_length(validation_errors, 0)
})


if (file.exists("config.json")) {
  file.remove("config.json")
}
