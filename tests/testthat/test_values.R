

test_that("value test 1", {
  expect_is(unit(
    label = "meter", description = "feaiejf", from.existing = unit.value.from.existing$`MicroW-PER-M2`,
    definition.uri = "https://www.amazon.co.uk/gp/css/gc/balance?ref_=ya_d_c_gc",
    scaling.factor = 1234.2341, quantity.kind = quantity.kind$PropellantMeanBulkTemperature, si.scaling.factor = 124
  ), "unit")
})

test_that("value test 2", {
  expect_error(unit(
    label = "meter", description = "feaiejf", from.existing = unit.value.from.existing$`MicroW-PER-M2`,
    definition.uri = "https://www.amazon.co.uk/gp/css/gc/balance?ref_=ya_d_c_gc",
    scaling.factor = "feaijefi", quantity.kind = quantity.kind$PropellantMeanBulkTemperature, si.scaling.factor = 124
  ), "Warning: scaling.factor must be a single number.")
})


test_that("value test 3", {
  expect_error(unit(
    label = "meter", description = "feaiejf", from.existing = unit.value.from.existing$`MicroW-PER-M2`,
    definition.uri = "https://www.amazon.co.uk/gp/css/gc/balance?ref_=ya_d_c_gc",
    scaling.factor = 132, quantity.kind = quantity.kind$PropellantMeanBulkTemperature, si.scaling.factor = "efaifjeijf"
  ), "Warning: si.scaling.factor must be a single number.")
})



if (file.exists("config.json")) {
  file.remove("config.json")
}
