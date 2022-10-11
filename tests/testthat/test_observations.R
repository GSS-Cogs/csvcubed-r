
test_that("completeness test", {
  config <- create.config(df = example.data, id = "sweden_at_eurovision_no_missing") %>%
    add.observation.column(column.name = "Measure", measure = measure.value(label = "jufiaejf"), unit = unit.value(label = "meter", description = "meter", from.existing = Units$M, definition.uri = "foo://example.com:8042/over/there?name=ferret#nose", scaling.factor = 1, quantity.kind = QuantityKinds$Length, si.scaling.factor = 1))
  json_string <- generate.json.configuration(config, file = "config.json")
  validation_errors <- validate_qube_config(json_string)
  expect_length(validation_errors, 0)
})

test_that("errors test", {
  config <- create.config(df = example.data, id = "sweden_at_eurovision_no_missing")
  expect_error(config <- add.observation.column(config, column.name = "Measure", measure = T), "Warning: measure does not meet requirement. Try measure.value()")
  expect_error(config <- add.observation.column(config, column.name = "Measure", unit = T), "Warning: unit does not meet requirement. Try unit()")
})


if (file.exists("config.json")) {
  file.remove("config.json")
}
