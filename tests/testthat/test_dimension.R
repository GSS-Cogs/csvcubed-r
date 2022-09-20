
test_that("dimension_test1", {
  config <- create.config(df = example.data, id = "sweden_at_eurovision_no_missing") %>%
    add.dimension.column(
      column.name = "Entrant",
      label = "entrant",
      description = "entrant",
      from.existing = dimension.from.existing$age,
      definition.uri = "https://www.youtube.com/watch?v=qCCpwFYibmU&list=PLSLc7UMdyVg590pc_gWh-d5ny0_PGC6Te&index=4",
      cell.uri.template = "https://www.youtube.com/watch?v=qCCpwFYibmU&list=PLSLc7UMdyVg590pc_gWh-d5ny0_PGC6Te&index=4",
      code.list = T,
      from.template = "year"
    )
  json_string <- generate.json.configuration(config, file = "config.json")
  validation_errors <- validate_qube_config(json_string)
  expect_length(validation_errors, 0)
})

test_that("dimension_test2", {
  config <- create.config(df = example.data, id = "sweden_at_eurovision_no_missing") %>%
    add.dimension.column(
      column.name = "Entrant",
      label = "entrant",
      description = "entrant",
      from.existing = dimension.from.existing$age,
      definition.uri = "https://www.youtube.com/watch?v=qCCpwFYibmU&list=PLSLc7UMdyVg590pc_gWh-d5ny0_PGC6Te&index=4",
      cell.uri.template = "https://www.youtube.com/watch?v=qCCpwFYibmU&list=PLSLc7UMdyVg590pc_gWh-d5ny0_PGC6Te&index=4",
      code.list = "https://www.google.com/webhp?hl=en&ictx=2&sa=X&ved=0ahUKEwjx_ZLl05n6AhWPY8AKHY-FC8oQPQgE",
      from.template = dimension.from.template$year
    )
  json_string <- generate.json.configuration(config, file = "config.json")
  validation_errors <- validate_qube_config(json_string)
  expect_length(validation_errors, 0)
})


if (file.exists("config.json")) {
  file.remove("config.json")
}
