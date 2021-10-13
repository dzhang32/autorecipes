test_RecipeBook <- RecipeBook_example

##### favourites #####

test_that("favourites getter errors when favourites is empty", {
  expect_error(
    favourites(RecipeBook_example),
    "No favourite recipes set, have you run `favorites<-`?"
  )
})

test_that("favourites getter errors has correct output", {
  test_RecipeBook@recipes[["fav"]][1:3] <- TRUE
  expect_true(identical(
    favourites(test_RecipeBook),
    test_RecipeBook@recipes %>% dplyr::filter(fav)
  ))
})

##### favourites<- #####

test_that("favourites setter (numeric) output looks correct", {
  favourites(test_RecipeBook) <- c(1, 2, 3)
  expect_true(identical(
    which(test_RecipeBook@recipes[["fav"]]),
    c(1, 2, 3) %>% as.integer()
  ))
})

test_that("favourites setter (logical) output looks correct", {
  favourites(test_RecipeBook) <- rep(TRUE, nrow(test_RecipeBook@recipes))
  expect_true(all(test_RecipeBook@recipes[["fav"]]))

  # reset all favourites to false
  favourites(test_RecipeBook) <- FALSE
  expect_false(any(test_RecipeBook@recipes[["fav"]]))
})

test_that("favourites setter (character/manual) output looks correct", {
  # setup testing to mirror manual input
  # based on https://debruine.github.io/posts/interactive-test/
  input <- file()
  lines <- "1,2,3"
  write(lines, input)
  test_manual <- .set_favourites_manual(
    test_RecipeBook, "manual",
    con = input
  )
  close(input)

  expect_true(identical(
    which(test_manual@recipes[["fav"]]),
    c(1, 2, 3) %>% as.integer()
  ))
})

test_that("favourites setter (character/manual) catches user input errors", {
  expect_error(
    favourites(test_RecipeBook) <- "not_manual",
    "If favourites<- is set by character, value must be 'manual'"
  )
})
