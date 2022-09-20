
test_that("unit test 1", {
  config <- create.config(df = example.data, id = "sweden_at_eurovision_no_missing")
  config <- add.unit.column(config, column.name = "Unit", unit.values =unit.values(unit(label = "Numberless"), unit(label = "Number")))
  json_string <- generate.json.configuration(config, "config.json")
  validation_errors <- validate_qube_config(json_string)
  expect_length(validation_errors, 0)
})

test_that("unit test 2", {
  config <- create.config(df = example.data, id = "sweden_at_eurovision_no_missing")
  config <- add.unit.column(config, column.name = "Unit", unit.values = T)
  json_string <- generate.json.configuration(config, "config.json")
  validation_errors <- validate_qube_config(json_string)
  expect_length(validation_errors, 0)
})

test_that("unit test 3", {
  config <- create.config(df = example.data, id = "sweden_at_eurovision_no_missing")
  expect_error(config <- add.unit.column(config, column.name = "Unit", unit.values = 123432),"Warning: values does not meet requirement. Try using unit.values().")
})


if (file.exists("config.json")) {
  file.remove("config.json")
}
