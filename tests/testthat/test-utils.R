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

##### .check_object #####

test_that(".check_object output looks correct", {
  expect_true(.check_object(recipebook_example, "RecipeBook"))
})

test_that(".check_object catches user-input errors", {
  expect_error(
    .check_object(recipebook_example, "character"),
    "object is not an instance of character-class"
  )
})

##### .select_index_message #####

test_that(".check_object output looks correct", {
  expect_message(
    .select_index_message(
      object = c("recipe_1", "recipe_2"),
      what = "recipes",
      to = "favourite",
      in_from = "recipebook"
    ),
    "1 - recipe_1\n2 - recipe_2"
  )
  expect_message(
    .select_index_message(
      object = c("recipe_1", "recipe_2"),
      what = "recipes",
      to = "favourite",
      in_from = "recipebook"
    ),
    "select the recipes you would like to favourite in your recipebook"
  )
  expect_message(
    .select_index_message(
      object = c("ingred_1", "ingred_2"),
      what = "ingredients",
      to = "exclude",
      in_from = "shopping list"
    ),
    "1 - ingred_1\n2 - ingred_2"
  )
  expect_message(
    .select_index_message(
      object = c("ingred_1", "ingred_2"),
      what = "ingredients",
      to = "exclude",
      in_from = "shopping list"
    ),
    "select the ingredients you would like to exclude from your shopping list"
  )
})
