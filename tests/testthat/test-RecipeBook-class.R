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
        test_recipebook@recipes[["names"]],
        c("Roast Chicken", "Another Roast")
    ))
    expect_true(nrow(test_recipebook@recipes) == 2)

    expect_true(identical(
        test_recipebook@recipes[["ingredients"]][[1]],
        test_ingred
    ))

    expect_true(nrow(test_recipebook@meal_plan) == 0)
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
        new("RecipeBook", recipes = dplyr::tibble()),
        "recipebook must have > 0 rows"
    )

    expect_error(
        new("RecipeBook", recipes = dplyr::tibble(names = "Roast Chicken")),
        "recipebook must contain 'names' and 'ingredients' columns"
    )

    expect_error(
        new("RecipeBook", recipes = dplyr::tibble(names = 1, ingredients = 1)),
        "names must be a character"
    )

    expect_error(
        new(
            "RecipeBook",
            recipes = dplyr::tibble(names = "1", ingredients = 1)
        ),
        "ingredients must be a list containing Ingredients-class objects"
    )
})

##### read_ingredients #####

test_ingred <- recipes_example[["ingredients"]]

test_that("read_ingredients default works correctly", {
    expect_true(identical(
        read_ingredients(test_ingred[1:2]),
        .read_ingredients_auto(test_ingred[1:2], delim = ";")
    ))
})

test_that("read_ingredients catches input errors", {
    expect_error(
        read_ingredients(test_ingred[1:2], delim = ";", method = "x"),
        "'arg' should be one of"
    )
    expect_error(
        read_ingredients(test_ingred[1:2], delim = NULL),
        "Currently, delim must be entered"
    )
})


##### .read_ingredient_auto #####

test_that(".read_ingredient_auto output looks broadly correctly", {
    test_read_ingred <- .read_ingredients_auto(test_ingred, delim = ";")

    expect_true(identical(length(test_read_ingred), length(test_ingred)))
    expect_true(all(
        lapply(test_read_ingred, function(x) methods::is(x, "Ingredients")) %>%
            unlist()
    ))
})

test_that(".read_ingredient_auto output looks correct", {
    test_read_ingred <- .read_ingredients_auto(
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
