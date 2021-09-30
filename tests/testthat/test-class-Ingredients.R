test_that("default ingredients look correct", {
    test_ingred <- new("Ingredients", name = 1, amount = c(1))
})

test_that("Ingredients catches user-input errors", {
    expect_error(Ingredients(amount = c(1, 2)))
    expect_error(
        Ingredients(name = "chicken", amount = c(1, 2)),
        "object@name and object@amount must have equal lengths"
    )
    expect_error(
        Ingredients(name = "Chicken", amount = c(1)),
        "All object@name values must be lower case"
    )
    expect_error(
        Ingredients(name = "chicken", amount = 1, units = c("g", "g")),
        "object@units must be the same length"
    )
    expect_error(
        Ingredients(name = "chicken", amount = 1, units = "not_a_unit"),
        "and be one of; NA, g"
    )
})
