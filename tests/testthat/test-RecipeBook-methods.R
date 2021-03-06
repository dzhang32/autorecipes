##### show #####

test_that("RecipeBook prints as expected", {
  expect_true(identical(
    capture.output(print(recipebook_example)),
    capture.output(print(recipebook_example@recipes))
  ))
})

##### getters #####

test_that("RecipeBook getters work as expected", {
  expect_true(
    identical(
      names(recipebook_example),
      recipebook_example@recipes[["names"]]
    )
  )
  expect_true(
    identical(
      recipes(recipebook_example),
      recipebook_example@recipes
    )
  )
})

test_that("RecipeBook meal_plan getters errors on empty meal_plan", {
  expect_error(
    meal_plan(recipebook_example),
    "No meal plan found, have you run create_meal_plan()?"
  )
})

test_that("favourites getter errors when favourites is empty", {
  expect_error(
    favourites(recipebook_example),
    "No favourite recipes set, have you run `favorites<-`?"
  )
})

test_that("favourites getter errors has correct output", {
  test_recipebook <- recipebook_example
  test_recipebook@recipes[["fav"]][1:3] <- TRUE
  expect_true(identical(
    favourites(test_recipebook),
    test_recipebook@recipes %>% dplyr::filter(fav)
  ))
})

##### setters - meal_plan #####

test_that("RecipeBook meal_plan setter works as expected", {
  test_recipebook <- recipebook_example
  test_meal_plan <- dplyr::tibble(
    day = "Mon",
    meal = "Lunch",
    recipe_index = 1
  )

  meal_plan(test_recipebook) <- test_meal_plan
  expect_true(identical(
    meal_plan(test_recipebook),
    test_meal_plan %>%
      dplyr::left_join(recipes(test_recipebook),
        by = c("recipe_index" = "index")
      )
  ))

  expect_message(
    meal_plan(test_recipebook) <- test_meal_plan,
    "Overwriting existing meal plan"
  )
})

test_that("RecipeBook meal_plan setter catches user input errors", {
  expect_error(
    meal_plan(recipebook_example) <- dplyr::tibble(invalid = "x"),
    "object@meal_plan must have the columns:"
  )
  expect_error(
    meal_plan(recipebook_example) <- dplyr::tibble(days = "x", meals = 1),
    "object@meal_plan must have the columns:"
  )
})

##### setters - shopping_list #####

test_that("RecipeBook shopping_list setter works as expected", {
  test_recipebook <- recipebook_example
  test_shopping_list <- dplyr::tibble(
    names = "Chicken",
    n = 1
  )

  shopping_list(test_recipebook) <- test_shopping_list
  expect_true(identical(
    shopping_list(test_recipebook),
    test_shopping_list
  ))

  expect_message(
    shopping_list(test_recipebook) <- test_shopping_list,
    "Overwriting existing shopping list"
  )
})

test_that("RecipeBook shopping_list setter catches user input errors", {
  expect_error(
    shopping_list(recipebook_example) <- dplyr::tibble(invalid = "x"),
    "object@shopping_list must have the columns:"
  )
})
