##### contructor #####

test_that("default ingredients look correct", {
  test_ingred <- new("Ingredients", names = "chicken")
  expect_true(test_ingred@names == "chicken")
  expect_true(is.na(test_ingred@amounts))
  expect_true(is.na(test_ingred@units))
})

test_that("Ingredients constructer helps users", {
  test_ingred <- Ingredients(
    names = c("Chicken", "Salt"),
    amounts = c(NA_real_, NA_real_)
  )
  expect_true(identical(test_ingred@names, c("chicken", "salt")))
  expect_true(identical(test_ingred@amounts, c(1, 1)))
})

##### validator #####

test_that("Ingredients validator catches user-input errors", {
  expect_error(new("Ingredients", amounts = c(1, 2)))
  expect_error(
    new("Ingredients", names = "chicken", amounts = c(1, 2)),
    "object@names and object@amounts must have equal lengths"
  )
  expect_error(
    new("Ingredients", names = "Chicken", amounts = c(1)),
    "All object@names values must be lower case"
  )
  expect_error(
    new("Ingredients", names = "chicken", amounts = 1, units = c("g", "g")),
    "object@units must be the same length"
  )
  expect_error(
    new("Ingredients", names = "chicken", amounts = 1, units = "not_a_unit"),
    "and be one of; NA, g"
  )
})

##### .convert_fractions #####

test_that(".convert_fractions output looks correct", {
  test_fractions <- .convert_fractions(c("1/100", "1/4", 1, 1.3, NA))

  expect_true(is.double(test_fractions))
  expect_equal(
    test_fractions,
    c(0.01, 0.25, 1.00, 1.30, NA)
  )
})

##### .all_valid_units #####

test_that(".all_valid_units output looks correct", {
  valid_units <- .all_valid_units()
  valid_units_no_na <- .all_valid_units(no_na = TRUE)
  valid_units_regex <- .all_valid_units(no_na = TRUE, regex = TRUE)

  expect_true(sum(is.na(valid_units)) == 1)
  expect_true(is.character(valid_units))

  expect_true(sum(is.na(valid_units_no_na)) == 0)

  expect_true(identical(
    valid_units_regex,
    valid_units_no_na %>%
      stringr::str_c(., " ") %>%
      stringr::str_c(collapse = "|")
  ))
})

test_that(".all_valid_units catches user_input errors", {
  expect_error(
    .all_valid_units(regex = TRUE),
    "when regex is TRUE, no_na should be TRUE"
  )
})
