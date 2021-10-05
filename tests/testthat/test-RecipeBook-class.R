test_ingred <- Ingredients(
    names = c(
        "chicken",
        "salt",
        "pepper"
    ),
    amounts = c(1, 2, 3),
    units = c(NA_character_, "g", "g")
)

##### RecipeBook-class #####

test_that("RecipeBook can be constructed correctly", {
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
