
test_that("Something happens", {
  # setwd("~/Desktop/卡迪夫学习/Dissertation")
  expect_error(unit(description = "feajifeji"), "argument \"label\" is missing, with no default")
  expect_error(unit(label = "year", description = 123), "Warning: description must be single nonempty string!")
  expect_error(unit(label = "year", scaling.factor = "feaff"), "Warning: scaling.factor must be a single number.")
  expect_is(unit(label = "year", description = "year", from.existing = "http://qudt.org/vocab/unit/MicroW-PER-M2", scaling.factor = 10, quantity.kind = "http://qudt.org/vocab/quantitykind/Resistivity", si.scaling.factor = 10), "unit")
  expect_error(unit(label = "year", from.existing = "faeufjesafji"), "Warning: from.existing must be a uri.")
  expect_is(attribute.value(label = "faeifa", description = "year", from.existing = "http://qudt.org/vocab/unit/MicroW-PER-M2"), "value")
  expect_is(unit.values(unit(label = "Unitless"), unit(label = "Number")), "unit.values")
})


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

test_that("create.config function tests", {
  expect_error(config <- create.config(df = example.data, id = c("sweden_at_eurovision_no_missing", "fejaifj")), "Warning: 'id' must be a single nonempty string.")
  expect_error(config <- create.config(df = example.data, id = "12341242342", title = c("sweden_at_eurovision_no_missing", "fejaifj")), "Warning: 'title' must be a single nonempty string.")
  expect_error(config <- create.config(example.data, id = "12341234213", title = "sweden_at_eurovision_no_missing", license = "ifeiajijfid"), "Warning: 'license' must be a URI.")
  expect_is(config <- create.config(example.data, id = "12341234213", title = "sweden_at_eurovision_no_missing", license = organizations$`ONS Geography Linked Data`), "configuration")
})

if (file.exists("config.json")) {
  file.remove("config.json")
}
