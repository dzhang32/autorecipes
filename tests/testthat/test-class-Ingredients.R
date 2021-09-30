test_that("default ingredients look correct", {
  test_ingred <- new("Ingredients", name = 1, amount = c(1))

})

test_that("Ingredients catches user-input errors", {
  expect_error(Ingredients(name = "chicken", amount = c(1, 2)),
               "object@name and object@amount must have equal lengths")
  expect_error(Ingredients(name = "Chicken", amount = c(1)),
               "All object@name values must be lower case")
})



