test_ingred <- Ingredients(
    names = c(
        "chicken",
        "salt",
        "pepper"
    ),
    amounts = c(1, 2, 3),
    units = c(NA_character_, "g", "g")
)

test_that("default RecipeBook look correct", {
    test_recipebook <- new("RecipeBook")
    expect_true(is.na(test_recipebook@names))
    expect_true(is.list(test_recipebook@ingredients))
    expect_true(length(test_recipebook@ingredients) == 0)
})

test_that("RecipeBook validator catches user-input errors", {
    expect_error(
        new(
            "RecipeBook",
            names = "Roast Chicken",
            ingredients = list(test_ingred, test_ingred)
        ),
        "object@names and object@ingredients must have equal lengths"
    )
    expect_error(
        new(
            "RecipeBook",
            names = "Roast Chicken",
            ingredients = list(1)
        ),
        "object@ingredients must be a list containing Ingredients-class objects"
    )
})

test_that("RecipeBook getters work as expected", {
    test_recipebook <- RecipeBook(
        names = c("Roast Chicken", "Roast Chicken"),
        ingredients = list(test_ingred, test_ingred)
    )

    expect_true(identical(
        names(test_recipebook),
        stringr::str_to_title(test_recipebook@names)
    ))
})
