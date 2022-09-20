


test_that("attribute_test1", {
  config <- create.config(df = example.data, id = "sweden_at_eurovision_no_missing") %>%
    add.resource.attribute.column(
      label = "status",
      column.name = "Status",
      description = "the status",
      from.existing = attribute.from.existing$embargoTime,
      definition.uri = "https://www.gcores.com",
      values = values(attribute.value(label = "Final", description = "final", definition.uri = "https://www.youtube.com/watch?v=JXoDP24WTiQ", from.existing = attribute.value.from.existing$profMeth))
    )
  json_string <- generate.json.configuration(config, file = "config.json")
  validation_errors <- validate_qube_config(json_string)
  expect_length(validation_errors, 0)
})

test_that("attribute_test2", {
  config <- create.config(df = example.data, id = "sweden_at_eurovision_no_missing") %>%
    add.resource.attribute.column(
      label = "status",
      column.name = "Status",
      description = "the status",
      from.existing = attribute.from.existing$embargoTime,
      definition.uri = "https://www.gcores.com",
      values = T,
      required = T
    )
  json_string <- generate.json.configuration(config, file = "config.json")
  validation_errors <- validate_qube_config(json_string)
  expect_length(validation_errors, 0)
})

test_that("attribute_test3", {
  config <- create.config(df = example.data, id = "sweden_at_eurovision_no_missing")
  expect_error(add.resource.attribute.column(config,
    label = "status",
    column.name = "Status",
    from.existing = "hello"
  ), "Warning: from_existing must a uri.")
})

test_that("attribute_test4", {
  config <- create.config(df = example.data, id = "sweden_at_eurovision_no_missing")
  expect_error(add.resource.attribute.column(config,
    label = "status",
    column.name = "Status",
    values = 134
  ), "Warning: values does not meet requirement. Try using values().")
})

test_that("attribute_test5", {
  config <- create.config(df = example.data, id = "sweden_at_eurovision_no_missing")
  expect_error(add.literal.attribute.column(config,
    label = "status",
    column.name = "Status",
    description = "the status",
    from.existing = attribute.from.existing$embargoTime,
    definition.uri = "https://www.gcores.com",
    data.type = "int",
    required = 123432412
  ), "Warning: required must be a boolean.")
})

if (file.exists("config.json")) {
  file.remove("config.json")
}
