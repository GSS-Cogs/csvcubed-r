
test_that("observation test1", {
  config <- create.config(df = example.data, id = "sweden_at_eurovision_no_missing") %>%
    add.observation.column(column.name = "Measure",measure = measure.value(label = "jufiaejf"), unit = unit(label = "meter", description = "meter", from.existing = unit.value.from.existing$M,definition.uri ="https://www.qq.com", scaling.factor = 1, quantity.kind = quantity.kind$Length, si.scaling.factor = 1))
  json_string <- generate.json.configuration(config, file = "config.json")
  validation_errors <- validate_qube_config(json_string)
  expect_length(validation_errors, 0)
})

test_that("observation test2", {
  config <- create.config(df = example.data, id = "sweden_at_eurovision_no_missing")
  expect_error(config <- add.observation.column(config,column.name = "Measure",measure = T),"Warning: measure does not meet requirement. Try measure.value()")
  expect_error(config <- add.observation.column(config,column.name = "Measure",unit = T),"Warning: unit does not meet requirement. Try unit()")
})


if (file.exists("config.json")) {
  file.remove("config.json")
}
