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
