

test_that("completeness test", {
  config <- create.config(df = example.data, id = "sweden_at_eurovision_no_missing") %>%
    add.measure.column(
      column.name = "Measure",
      values = values(
        measure.value(label = "Final Rank", description = "jfeaiji", definition.uri = "foo://example.com:8042/over/there?name=ferret#nose", from.existing = Measures$count),
        measure.value(label = "Final Points")
      )
    )
  json_string <- generate.json.configuration(config, file = "config.json")
  validation_errors <- validate_qube_config(json_string)
  expect_length(validation_errors, 0)
})

test_that("uri test", {
  config <- create.config(df = example.data, id = "sweden_at_eurovision_no_missing")
  expect_error(config <- add.measure.column(config,
    column.name = "Measure",
    values = values(
      measure.value(label = "Final Rank", description = "jfeaiji", definition.uri = "foo://example.com:8042/over/there?name=ferret#nose", from.existing = Measures$count),
      measure.value(label = "Final Points", definition.uri = "eaijfjfi")
    )
  ), "Warning: definition.uri must be a uri.")
})

test_that("uri error test", {
  config <- create.config(df = example.data, id = "sweden_at_eurovision_no_missing")
  expect_error(config <- add.measure.column(config,
    column.name = "Measure",
    values = values(
      measure.value(label = "Final Rank", description = "jfeaiji", definition.uri = "foo://example.com:8042/over/there?name=ferret#nose", from.existing = Measures$count),
      measure.value(label = "Final Points", definition.uri = "eaijfjfi")
    )
  ), "Warning: definition.uri must be a uri.")
})


test_that("uri value boolean test", {
  config <- create.config(df = example.data, id = "sweden_at_eurovision_no_missing")
  config <- add.measure.column(config,
    column.name = "Measure",
    values = T
  )
  json_string <- generate.json.configuration(config, file = "config.json")
  validation_errors <- validate_qube_config(json_string)
  expect_length(validation_errors, 0)
})

if (file.exists("config.json")) {
  file.remove("config.json")
}
