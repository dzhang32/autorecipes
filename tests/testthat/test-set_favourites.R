test_recipebook <- recipebook_example

##### set_favourites #####

test_that("favourites setter (index) output looks correct", {
  test_recipebook <- add_favourites(test_recipebook, 1:3)
  expect_true(identical(
    which(test_recipebook@recipes[["fav"]]),
    c(1, 2, 3) %>% as.integer()
  ))

  test_recipebook <- rm_favourites(test_recipebook, 1:3)
  expect_false(any(test_recipebook@recipes[["fav"]]))
})

##### .set_favourites_manual #####

test_that(".set_favourites_manual output looks correct", {
  # setup testing to mirror manual input
  # based on https://debruine.github.io/posts/interactive-test/
  input <- file()
  lines <- "1,2,3"
  write(lines, input)
  expect_message(
    test_manual <- .set_favourites_manual(
      test_recipebook,
      value = TRUE,
      con = input
    ),
    "select the recipes you would like to favourite in your recipebook."
  )

  expect_true(identical(
    test_manual,
    1:3
  ))

  recipes(test_recipebook)[["fav"]][1:3] <- TRUE
  expect_message(
    test_manual <- .set_favourites_manual(
      test_recipebook,
      value = FALSE,
      con = input
    ),
    "select the recipes you would like to unfavourite from your recipebook"
  )

  close(input)
})

test_that(".set_favourites_manual catches user-input errors", {
  input <- file()
  lines <- "1,2,3"
  write(lines, input)
  expect_error(
    test_manual <- .set_favourites_manual(
      test_recipebook,
      value = FALSE,
      con = input
    ),
    "No recipes found with favourite as FALSE"
  )
  close(input)

  recipes(test_recipebook)[["fav"]][1:3] <- TRUE
  input <- file()
  lines <- "4,5"
  write(lines, input)
  expect_error(
    test_manual <- .set_favourites_manual(
      test_recipebook,
      value = FALSE,
      con = input
    ),
    "Invalid indexes found in input: 4, 5"
  )
  close(input)
})
