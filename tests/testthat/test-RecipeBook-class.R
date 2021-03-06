##### constructor #####

test_that("RecipeBook can be constructed correctly", {
  test_ingred <- Ingredients(
    names = c(
      "chicken",
      "salt",
      "pepper"
    ),
    amounts = c(1, 2, 3),
    units = c(NA_character_, "g", "g")
  )

  test_recipebook <- RecipeBook(
    names = c("Roast Chicken", "Another Roast"),
    ingredients = list(test_ingred, test_ingred)
  )

  expect_true(identical(
    test_recipebook@recipes[["index"]],
    1:2
  ))

  expect_true(identical(
    test_recipebook@recipes[["names"]],
    c("Roast Chicken", "Another Roast")
  ))

  expect_true(nrow(test_recipebook@recipes) == 2)

  expect_true(identical(
    test_recipebook@recipes[["ingredients"]][[1]],
    test_ingred
  ))

  expect_true(all(is.na(
    test_recipebook@recipes[["last_eaten"]]
  )))

  expect_true(nrow(test_recipebook@meal_plan) == 0)
  expect_true(nrow(test_recipebook@shopping_list) == 0)
})

test_that("RecipeBook can be constructed using character as ingredients", {
  test_recipebook <- RecipeBook(
    names = c("Roast Chicken", "Another Roast"),
    ingredients = c("1g salt;2 chicken thighs", "1g salt;2 chicken thighs")
  )

  expect_true(identical(
    test_recipebook@recipes[["names"]],
    c("Roast Chicken", "Another Roast")
  ))
  expect_true(nrow(test_recipebook@recipes) == 2)

  expect_true(identical(
    test_recipebook@recipes[["ingredients"]][[1]],
    Ingredients(
      names = c("Salt", "Chicken Thighs"),
      amounts = c(1, 2),
      units = c("g", NA_character_)
    )
  ))
})

##### validator #####

test_that("RecipeBook validator catches user-input errors", {
  expect_error(
    new("RecipeBook", names = character(), ingredients = integer()),
    "object@recipes must have > 0 rows"
  )

  expect_error(
    new("RecipeBook", names = "Roast Chicken", ingredients = NULL),
    "object@recipes must contain 'names' and 'ingredients' columns"
  )

  expect_error(
    new("RecipeBook", names = 1, ingredients = "chr"),
    "must be a character"
  )

  expect_error(
    new(
      "RecipeBook",
      names = "Roast Chicken", ingredients = "chr"
    ),
    "must be a list containing Ingredients-class objects"
  )
})

test_that("RecipeBook validator errors on indexes", {
  test_recipebook <- recipebook_example
  test_recipebook@recipes[["index"]][2] <- 3

  expect_error(
    validObject(test_recipebook),
    "should be equivalent to "
  )
})

test_that("RecipeBook validator catches meal_plan errors", {
  test_recipebook <- recipebook_example
  test_recipebook <- create_meal_plan(
    test_recipebook,
    c("Wed", "Thurs"),
    "Dinner"
  )

  expect_warning(
    test_recipebook@meal_plan[["day"]][1] <- "not_a_weekday",
    "invalid factor level"
  )
  expect_error(
    validObject(test_recipebook),
    "must all be one of"
  )

  test_recipebook@meal_plan[["day"]] <- NULL
  expect_error(
    validObject(test_recipebook),
    "object@meal_plan must have the columns:"
  )
})

test_that("RecipeBook validator catches meal_plan errors", {
  test_recipebook <- recipebook_example
  test_recipebook@shopping_list <- dplyr::tibble(
    names = "Chicken"
  )

  expect_error(
    validObject(test_recipebook),
    "object@shopping_list must have the columns: "
  )
})

##### read_ingredients #####

test_ingred <- recipes_example[["ingredients"]]

test_that("read_ingredients default works correctly", {
  expect_true(identical(
    read_ingredients(test_ingred[1:2]),
    .parse_ingredients_character(test_ingred[1:2], delim = ";")
  ))
})

test_that("read_ingredients catches input errors", {
  expect_error(
    read_ingredients(test_ingred[1:2], delim = NULL),
    "Currently, delim must be entered"
  )
})


##### .read_ingredient_auto #####

test_that(".read_ingredient_auto output looks broadly correctly", {
  test_read_ingred <- .parse_ingredients_character(test_ingred, delim = ";")

  expect_true(identical(length(test_read_ingred), length(test_ingred)))
  expect_true(all(
    lapply(test_read_ingred, function(x) methods::is(x, "Ingredients")) %>%
      unlist()
  ))
})

test_that(".read_ingredient_auto output looks correct", {
  test_read_ingred <- .parse_ingredients_character(
    "1 red onion;3 garlic cloves;500g salad potatoes;100ml hot water",
    delim = ";"
  )

  expect_true(length(test_read_ingred) == 1)
  expect_true(identical(
    test_read_ingred[[1]]@names,
    c("red onion", "garlic cloves", "salad potatoes", "hot water")
  ))
  expect_true(identical(
    test_read_ingred[[1]]@amounts,
    c(1, 3, 500, 100)
  ))
  expect_true(identical(
    test_read_ingred[[1]]@units,
    c(NA_character_, NA_character_, "g", "ml")
  ))
})
