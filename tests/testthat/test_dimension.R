
test_that("completeness test1 code list boolean", {
  config <- create.config(df = example.data, id = "sweden_at_eurovision_no_missing") %>%
    add.dimension.column(
      column.name = "Entrant",
      label = "entrant",
      description = "entrant",
      from.existing = Dimensions$age,
      definition.uri = "foo://example.com:8042/over/there?name=ferret#nose",
      cell.uri.template = "foo://example.com:8042/over/there?name=ferret#nose",
      code.list = T,
      from.template = "year"
    )
  json_string <- generate.json.configuration(config, file = "config.json")
  validation_errors <- validate_qube_config(json_string)
  expect_length(validation_errors, 0)
})

test_that("completeness test 2 code list uri", {
  config <- create.config(df = example.data, id = "sweden_at_eurovision_no_missing") %>%
    add.dimension.column(
      column.name = "Entrant",
      label = "entrant",
      description = "entrant",
      from.existing = Dimensions$age,
      definition.uri = "foo://example.com:8042/over/there?name=ferret#nose",
      cell.uri.template = "foo://example.com:8042/over/there?name=ferret#nose",
      code.list = "foo://example.com:8042/over/there?name=ferret#nose",
      from.template = DimensionTemplates$year
    )
  json_string <- generate.json.configuration(config, file = "config.json")
  validation_errors <- validate_qube_config(json_string)
  expect_length(validation_errors, 0)
})

test_that("dimension_test3", {
  config <- create.config(df = example.data, id = "sweden_at_eurovision_no_missing") %>%
    add.dimension.column(
      column.name = "Entrant",
      label = "entrant",
      description = "entrant",
      from.existing = Dimensions$age,
      definition.uri = "foo://example.com:8042/over/there?name=ferret#nose",
      cell.uri.template = "foo://example.com:8042/over/there?name=ferret#nose",
      code.list = "foo://example.com:8042/over/there?name=ferret#nose",
      from.template = DimensionTemplates$year
    )
  expect_error(config <- add.dimension.column(config, column.name = "Entrant", label = "entrant"), "Warning: column \"Entrant\" is already configured.")
})


if (file.exists("config.json")) {
  file.remove("config.json")
}
