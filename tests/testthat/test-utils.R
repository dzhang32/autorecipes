##### weekdays #####

test_that("weekdays output looks correct", {
  expect_true(identical(
    weekdays(),
    c("Mon", "Tues", "Wed", "Thurs", "Fri", "Sat", "Sun")
  ))
  expect_true(identical(
    weekdays(1:3),
    c("Mon", "Tues", "Wed")
  ))
  expect_true(identical(
    weekdays(c("Mon", "Tues", "Wed")),
    c("Mon", "Tues", "Wed")
  ))
})

test_that("weekdays catches user-input errors", {
  expect_error(weekdays(10), "When an numeric, which_days must be one of 1:7")
  expect_error(weekdays("NA"), "When an character, which_days must be one of:")
})

##### .check_object #####

test_that(".check_object output looks correct", {
  expect_true(.check_object(recipebook_example, "RecipeBook"))
})

test_that(".check_object catches user-input errors", {
  expect_error(
    .check_object(recipebook_example, "character"),
    "object is not an instance of character-class"
  )
})
