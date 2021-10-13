test_RecipeBook <- RecipeBook_example
test_RecipeBook <- create_meal_plan(test_RecipeBook)

##### create_shopping_list #####

test_that("create_shopping_list (none) output looks correct", {
  test_shopping_list <- create_shopping_list(test_RecipeBook)
  expect_false(any(duplicated(test_shopping_list[["names"]])))
  expect_true(ncol(test_shopping_list) == 2)
  expect_true(any(.all_store_cupboard() %in% test_shopping_list[["names"]]))
})

test_that("create_shopping_list (minimal) output looks correct", {
  test_shopping_list <- create_shopping_list(test_RecipeBook, "minimal")
  expect_false(any(.all_store_cupboard() %in% test_shopping_list[["names"]]))
})

test_that("create_shopping_list catches user-input errors", {
  expect_error(
    create_shopping_list(RecipeBook_example, method = "not_a_method"),
    "should be one of"
  )
  expect_error(
    create_shopping_list(tibble::tibble()),
    "recipebook must be instance of RecipeBook-class"
  )
  expect_error(
    create_shopping_list(RecipeBook_example),
    "No meal plan found, have you run create_meal_plan"
  )
})

##### .filter_shopping_list_manual #####

test_that(".filter_shopping_list_manual output looks correct", {
  test_shopping_list <- create_shopping_list(test_RecipeBook, method = "none")
  test_sc <- .all_store_cupboard()
  # setup testing to mirror manual input
  # based on https://debruine.github.io/posts/interactive-test/
  input <- file()
  lines <- "1,2,3"
  write(lines, input)
  test_manual <- .filter_shopping_list_manual(
    test_shopping_list,
    con = input
  )
  close(input)

  expect_true(nrow(test_manual) < nrow(test_shopping_list))
  expect_false(any(test_sc[4:length(test_sc)] %in% test_manual[["names"]]))
})

##### .extract_ingredients #####

test_that(".extract_ingredients output looks correct", {
  recipes_test <- recipes(test_RecipeBook) %>%
    .[meal_plan(test_RecipeBook)[["recipe_index"]], ]
  ingredients_test <- recipes_test[["ingredients"]] %>%
    lapply(function(x) x@names) %>%
    unlist() %>%
    sort()

  test_ingredients_df <- .extract_ingredients(test_RecipeBook)
  expect_true(identical(
    sort(test_ingredients_df[["names"]]),
    ingredients_test
  ))
})

##### .collapse_ingredients #####

test_that(".collapse_ingredients output looks correct", {
  test_ingredients_collapsed <- .collapse_ingredients(dplyr::tibble(
    names = stringr::str_c("ingred_", c(1, 1, 2, 3))
  ))
  expect_true(is.data.frame(test_ingredients_collapsed))
  expect_true(identical(
    test_ingredients_collapsed[["names"]],
    c("ingred_1", "ingred_2", "ingred_3")
  ))
  expect_true(identical(
    test_ingredients_collapsed[["n"]],
    c(2L, 1L, 1L)
  ))
})
