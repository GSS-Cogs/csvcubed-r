

test_that("measure_test1", {
  config <- create.config(df = example.data, id = "sweden_at_eurovision_no_missing") %>%
    add.measure.column(
      column.name = "Measure",
      values = values(
        measure.value(label = "Final Rank", description = "jfeaiji", definition.uri = "https://www.amazon.co.uk/s?k=apple+gift+card+—+app+store%2C+itunes%2C+iphone%2C+ipad%2C+airpods%2C+macbook%2C+accessories+and+more+%28email+delivery%29+–+for+uk&crid=AMQTV99P1F42&sprefix=%2Caps%2C51&ref=nb_sb_ss_recent_1_0_recent", from.existing = measure.value.from.existing$`travel-time`),
        measure.value(label = "Final Points")
      )
    )
  json_string <- generate.json.configuration(config, file = "config.json")
  validation_errors <- validate_qube_config(json_string)
  expect_length(validation_errors, 0)
})

test_that("measure_test2", {
  config <- create.config(df = example.data, id = "sweden_at_eurovision_no_missing")
  expect_error(config <- add.measure.column(config,
    column.name = "Measure",
    values = values(
      measure.value(label = "Final Rank", description = "jfeaiji", definition.uri = "https://www.amazon.co.uk/s?k=apple+gift+card+—+app+store%2C+itunes%2C+iphone%2C+ipad%2C+airpods%2C+macbook%2C+accessories+and+more+%28email+delivery%29+–+for+uk&crid=AMQTV99P1F42&sprefix=%2Caps%2C51&ref=nb_sb_ss_recent_1_0_recent", from.existing = measure.value.from.existing$`travel-time`),
      measure.value(label = "Final Points", definition.uri = "eaijfjfi")
    )
  ), "Warning: definition.uri must be a uri.")
})

test_that("measure_test3", {
  config <- create.config(df = example.data, id = "sweden_at_eurovision_no_missing")
  expect_error(config <- add.measure.column(config,
    column.name = "Measure",
    values = values(
      measure.value(label = "Final Rank", description = "jfeaiji", definition.uri = "https://www.amazon.co.uk/s?k=apple+gift+card+—+app+store%2C+itunes%2C+iphone%2C+ipad%2C+airpods%2C+macbook%2C+accessories+and+more+%28email+delivery%29+–+for+uk&crid=AMQTV99P1F42&sprefix=%2Caps%2C51&ref=nb_sb_ss_recent_1_0_recent", from.existing = measure.value.from.existing$`travel-time`),
      measure.value(label = "Final Points", definition.uri = "eaijfjfi")
    )
  ), "Warning: definition.uri must be a uri.")
})


test_that("measure_test4", {
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
