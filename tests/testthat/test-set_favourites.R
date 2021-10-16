test_recipebook <- recipebook_example

##### set_favourites #####

test_that("favourites setter (numeric) output looks correct", {
  test_recipebook <- add_favourites(test_recipebook, method = "index", 1:3)
  expect_true(identical(
    which(test_recipebook@recipes[["fav"]]),
    c(1, 2, 3) %>% as.integer()
  ))

  test_recipebook <- rm_favourites(test_recipebook, method = "index", 1:3)
  expect_false(any(test_recipebook@recipes[["fav"]]))
})

test_that(".set_favourites_manual output looks correct", {
  # setup testing to mirror manual input
  # based on https://debruine.github.io/posts/interactive-test/
  input <- file()
  lines <- "1,2,3"
  write(lines, input)
  test_manual <- .set_favourites_manual(
    test_recipebook,
    con = input
  )
  close(input)

  expect_true(identical(
    test_manual,
    1:3
  ))
})
