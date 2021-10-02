test_that("default ingredients look correct", {
    test_ingred <- new("Ingredients", names = "chicken")
    expect_true(test_ingred@names == "chicken")
    expect_true(is.na(test_ingred@amounts))
    expect_true(is.na(test_ingred@units))
})

test_that("Ingredients constructer helps users", {
    test_ingred <- Ingredients(
        names = c("Chicken", "Salt"),
        amounts = c(NA_real_, NA_real_)
    )
    expect_true(identical(test_ingred@names, c("chicken", "salt")))
    expect_true(identical(test_ingred@amounts, c(1, 1)))
})

test_that("Ingredients validator catches user-input errors", {
    expect_error(new("Ingredients", amounts = c(1, 2)))
    expect_error(
        new("Ingredients", names = "chicken", amounts = c(1, 2)),
        "object@names and object@amounts must have equal lengths"
    )
    expect_error(
        new("Ingredients", names = "Chicken", amounts = c(1)),
        "All object@names values must be lower case"
    )
    expect_error(
        new("Ingredients", names = "chicken", amounts = 1, units = c("g", "g")),
        "object@units must be the same length"
    )
    expect_error(
        new("Ingredients", names = "chicken", amounts = 1, units = "not_a_unit"),
        "and be one of; NA, g"
    )
})

test_that("Ingredients getters work as expected", {
    # also tests that attributes are vectors, not scalars
    test_ingred <- Ingredients(
        names = c(
            "chicken",
            "salt",
            "pepper"
        ),
        amounts = c(1, 2, 3),
        units = c(NA_character_, "g", "g")
    )

    expect_true(
        identical(names(test_ingred), stringr::str_to_title(test_ingred@names))
    )
    expect_true(identical(amounts(test_ingred), test_ingred@amounts))
    expect_true(identical(units(test_ingred), test_ingred@units))
})

test_that("Ingredients print as expected", {
    test_ingred <- Ingredients(names = "chicken")

    expect_output(print(test_ingred), "1 Chicken")
})

