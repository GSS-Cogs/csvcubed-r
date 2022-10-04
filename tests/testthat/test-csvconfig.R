test_that("We can define attribute values which are correctly written to JSON", {
  config <- create.config(df = example.data, id = "12341241", title = "sweden_at_eurovision_no_missing")

  # test
  config <- add.resource.attribute.column(config, column.name = "Status", label = "Observation Status", values = values(attribute.value("Provisional"), attribute.value("Final")))
  config_string <- generate.json.configuration(config, "config.json")
  validation_errors <- validate_qube_config(config_string)

  expect_length(validation_errors, 0)
})


test_that("We can define attribute values which are correctly written to JSON", {
  config <- create.config(df = example.data, id = "12341241", title = "sweden_at_eurovision_no_missing")
  config <- add.resource.attribute.column(config, column.name = "Status", label = "Observation Status", values = values(attribute.value("Provisional"), attribute.value("Final")))
  config_string <- generate.json.configuration(config, "config.json")
  validation_errors <- validate_qube_config(config_string)
  expect_length(validation_errors, 0)
})

test_that("create.config function error tests", {
  expect_error(config <- create.config(df = example.data, id = c("sweden_at_eurovision_no_missing", "fejaifj")), "Warning: 'id' must be a single nonempty string.")
  expect_error(config <- create.config(df = example.data, id = "12341242342", title = c("sweden_at_eurovision_no_missing", "fejaifj")), "Warning: 'title' must be a single nonempty string.")
  expect_error(config <- create.config(example.data, id = "12341234213", title = "sweden_at_eurovision_no_missing", license = "ifeiajijfid"), "Warning: 'license' must be a URI.")
  expect_is(config <- create.config(example.data, id = "12341234213", title = "sweden_at_eurovision_no_missing", license = Organizations$home_office), "configuration")
})

if (file.exists("config.json")) {
  file.remove("config.json")
}
