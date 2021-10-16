test_recipebook <- recipebook_example

##### add_recipes #####

test_that("add_recipes output looks correct", {
  test_recipebook_base <- test_recipebook
  test_recipes_to_add <- recipes(test_recipebook_base) %>%
    .[11:length(test_recipebook_base), ]
  recipes(test_recipebook_base) <- recipes(test_recipebook_base)[1:10, ]
  expect_true(identical(
    add_recipes(
      test_recipebook_base,
      test_recipes_to_add[["names"]],
      test_recipes_to_add[["ingredients"]]
    ),
    test_recipebook
  ))
})

##### rm_recipes #####

test_that("rm_recipes (index) output looks correct", {
  test_recipebook <- create_meal_plan(test_recipebook)
  test_recipebook_rmed <- rm_recipes(test_recipebook, 1:10, "index")
  expect_true(length(test_recipebook_rmed) == length(test_recipebook) - 10)
  expect_true(identical(
    recipes(test_recipebook_rmed)[["names"]],
    recipes(test_recipebook)[["names"]][-c(1:10)]
  ))
  expect_true(identical(
    recipes(test_recipebook_rmed)[["index"]],
    seq_along(recipes(test_recipebook_rmed)[["index"]])
  ))
  expect_true(identical(
    dplyr::tibble(),
    test_recipebook_rmed@meal_plan
  ))
})

test_that("rm_recipes (index) output looks correct", {
  test_recipebook <- create_meal_plan(test_recipebook)
  test_recipebook_rmed <- rm_recipes(test_recipebook, 1:10, "index")
  expect_true(length(test_recipebook_rmed) == length(test_recipebook) - 10)
  expect_true(identical(
    recipes(test_recipebook_rmed)[["names"]],
    recipes(test_recipebook)[["names"]][-c(1:10)]
  ))
  expect_true(identical(
    recipes(test_recipebook_rmed)[["index"]],
    seq_along(recipes(test_recipebook_rmed)[["index"]])
  ))
  expect_true(identical(
    dplyr::tibble(),
    test_recipebook_rmed@meal_plan
  ))
})

test_that("rm_recipes (manual) output looks correct", {
  # setup testing to mirror manual input
  # based on https://debruine.github.io/posts/interactive-test/
  input <- file()
  lines <- "1,2,3"
  write(lines, input)
  test_manual <- .rm_recipes_manual(
    test_recipebook,
    con = input
  )
  close(input)

  expect_true(identical(
    test_manual,
    1:3
  ))
})
