test_recipebook <- recipebook_example
test_recipebook <- create_meal_plan(test_recipebook)

##### create_shopping_list #####

test_that("create_shopping_list (none) output looks correct", {
  test_recipebook <- create_shopping_list(test_recipebook)
  expect_false(any(duplicated(test_recipebook@shopping_list[["names"]])))
  expect_true(ncol(test_recipebook@shopping_list) == 2)
  expect_true(any(
    .all_store_cupboard() %in% test_recipebook@shopping_list[["names"]]
  ))
})

test_that("create_shopping_list (minimal) output looks correct", {
  test_recipebook <- create_shopping_list(test_recipebook, "minimal")
  expect_false(any(
    .all_store_cupboard() %in% test_recipebook@shopping_list[["names"]]
  ))
})

test_that("create_shopping_list catches user-input errors", {
  expect_error(
    create_shopping_list(recipebook_example, method = "not_a_method"),
    "should be one of"
  )
  expect_error(
    create_shopping_list(tibble::tibble()),
    "is not an instance of RecipeBook-class"
  )
  expect_error(
    create_shopping_list(recipebook_example),
    "No meal plan found, have you run create_meal_plan"
  )
})

##### .filter_shopping_list_manual #####

test_that(".filter_shopping_list_manual output looks correct", {
  test_recipebook <- create_shopping_list(test_recipebook, method = "none")
  test_sc <- .all_store_cupboard()
  # setup testing to mirror manual input
  # based on https://debruine.github.io/posts/interactive-test/
  input <- file()
  lines <- "1,2,3"
  write(lines, input)
  test_manual <- .filter_shopping_list_manual(
    test_recipebook@shopping_list,
    con = input
  )
  close(input)

  expect_true(nrow(test_manual) < nrow(test_recipebook@shopping_list))
  expect_false(any(test_sc[4:length(test_sc)] %in% test_manual[["names"]]))
})

##### .extract_ingredients #####

test_that(".extract_ingredients output looks correct", {
  recipes_test <- recipes(test_recipebook) %>%
    .[meal_plan(test_recipebook)[["recipe_index"]], ]
  ingredients_test <- recipes_test[["ingredients"]] %>%
    lapply(function(x) x@names) %>%
    unlist() %>%
    sort()

  test_ingredients_df <- .extract_ingredients(test_recipebook)
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
