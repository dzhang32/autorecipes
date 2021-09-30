test_that("default ingredients look correct", {
    test_ingred <- new("Ingredients", name = "chicken")
    expect_true(test_ingred@name == "chicken")
    expect_true(is.na(test_ingred@amount))
    expect_true(is.na(test_ingred@units))
})

test_that("Ingredients constructer helps users", {
    test_ingred <- Ingredients(name = "Chicken")
    expect_true(test_ingred@name == "chicken")
    expect_true(test_ingred@amount == 1)
})

test_that("Ingredients validator catches user-input errors", {
    expect_error(new("Ingredients", amount = c(1, 2)))
    expect_error(
        new("Ingredients", name = "chicken", amount = c(1, 2)),
        "object@name and object@amount must have equal lengths"
    )
    expect_error(
        new("Ingredients", name = "Chicken", amount = c(1)),
        "All object@name values must be lower case"
    )
    expect_error(
        new("Ingredients", name = "chicken", amount = 1, units = c("g", "g")),
        "object@units must be the same length"
    )
    expect_error(
        new("Ingredients", name = "chicken", amount = 1, units = "not_a_unit"),
        "and be one of; NA, g"
    )
})

test_that("Ingredients getters work as expected", {
    # also tests that attributes are vectors, not scalars
    test_ingred <- Ingredients(
        name = c(
            "chicken",
            "salt",
            "pepper"
        ),
        amount = c(1, 2, 3),
        units = c(NA_character_, "g", "g")
    )

    expect_true(
        identical(name(test_ingred), stringr::str_to_title(test_ingred@name))
    )
    expect_true(identical(amount(test_ingred), test_ingred@amount))
    expect_true(identical(units(test_ingred), test_ingred@units))
})
