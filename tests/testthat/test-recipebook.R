##### create_recipebook #####

test_ingred <- Ingredients(
    names = c(
        "chicken",
        "salt",
        "pepper"
    ),
    amounts = c(1, 2, 3),
    units = c(NA_character_, "g", "g")
)

test_that("create_recipebook can be created correctly", {
    test_recipebook <- create_recipebook(
        c("Roast Chicken", "Another Roast"), list(test_ingred, test_ingred)
    )
    expect_true(nrow(test_recipebook) == 2)
    expect_true(identical(
        test_recipebook[["names"]],
        c("Roast Chicken", "Another Roast")
    ))
})

test_that("create_recipebook catches user input errors", {
    expect_error(create_recipebook())
    expect_error(create_recipebook(names = character(), ingredients = list()))

    expect_error(
        valid_recipebook(dplyr::tibble()),
        "recipebook must contain 'names' and 'ingredients' columns"
    )

    expect_error(
        create_recipebook(
            c("Roast Chicken", "Another Roast"), list(test_ingred)
        ),
        "All arguments must of equal length"
    )

    expect_error(
        create_recipebook(1, list(test_ingred)),
        "names must be a character"
    )

    expect_error(
        create_recipebook(c("Roast Chicken"), 1),
        "ingredients must be a list containing Ingredients-class"
    )

    expect_error(
        create_recipebook(c("Roast Chicken"), list(1)),
        "ingredients must be a list containing Ingredients-class"
    )
})
