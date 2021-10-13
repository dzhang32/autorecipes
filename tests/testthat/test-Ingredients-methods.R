test_ingred <- Ingredients(
  names = c(
    "chicken",
    "salt",
    "pepper"
  ),
  amounts = c(1, 2, 3),
  units = c(NA_character_, "g", "g")
)

##### show #####

test_that("Ingredients print as expected", {
  expect_output(
    print(test_ingred),
    "1 Chicken\n2 g Salt\n3 g Pepper"
  )
})

##### getters #####

test_that("Ingredients getters work as expected", {
  expect_true(
    identical(names(test_ingred), stringr::str_to_title(test_ingred@names))
  )
  expect_true(identical(amounts(test_ingred), test_ingred@amounts))
  expect_true(identical(units(test_ingred), test_ingred@units))
})

##### as.data.frame #####

test_that("Test converting Ingredients-class to data.frame", {
  test_ingred_df <- as.data.frame(test_ingred)
  expect_true(is.data.frame(test_ingred_df))
  expect_true(identical(
    test_ingred_df,
    data.frame(
      names = test_ingred@names,
      amounts = test_ingred@amounts,
      units = test_ingred@units
    )
  ))
})
