

test_that("value completeness test", {
  expect_is(unit.value(
    label = "meter", description = "feaiejf", from.existing = Units$NanoH,
    definition.uri = "foo://example.com:8042/over/there?name=ferret#nose",
    scaling.factor = 1234.2341, quantity.kind = QuantityKinds$PropellantMeanBulkTemperature, si.scaling.factor = 124
  ), "unit")
})

test_that("value scaling factor error test", {
  expect_error(unit.value(
    label = "meter", description = "feaiejf", from.existing = Units$NanoH,
    definition.uri = "foo://example.com:8042/over/there?name=ferret#nose",
    scaling.factor = "feaijefi", quantity.kind = QuantityKinds$PropellantMeanBulkTemperature, si.scaling.factor = 124
  ), "Warning: scaling.factor must be a single number.")
})


test_that("si scaling factor error test", {
  expect_error(unit.value(
    label = "meter", description = "feaiejf", from.existing = Units$NanoH,
    definition.uri = "foo://example.com:8042/over/there?name=ferret#nose",
    scaling.factor = 132, quantity.kind = QuantityKinds$PropellantMeanBulkTemperature, si.scaling.factor = "efaifjeijf"
  ), "Warning: si.scaling.factor must be a single number.")
})



if (file.exists("config.json")) {
  file.remove("config.json")
}
