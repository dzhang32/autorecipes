##### show #####

test_that("RecipeBook prints as expected", {
    expect_true(identical(
        capture.output(print(RecipeBook_example)),
        capture.output(print(RecipeBook_example@recipes))
    ))
})

##### getters #####

test_that("RecipeBook getters work as expected", {
    expect_true(
        identical(
            names(RecipeBook_example),
            RecipeBook_example@recipes[["names"]]
        )
    )
    expect_true(
        identical(
            recipes(RecipeBook_example),
            RecipeBook_example@recipes
        )
    )
    expect_message(
        meal_plan(RecipeBook_example),
        "No meal plan found, have you run create_meal_plan()?"
    )
})

##### setters #####

test_that("RecipeBook meal_plan setter works as expected", {
    test_RecipeBook <- RecipeBook_example
    test_meal_plan <- dplyr::tibble(
        day = "Mon",
        meal = "Lunch",
        recipe_index = 1
    )

    meal_plan(test_RecipeBook) <- test_meal_plan
    expect_true(identical(meal_plan(test_RecipeBook), test_meal_plan))

    expect_message(
        {
            meal_plan(test_RecipeBook) <- test_meal_plan
        },
        "overwriting existing meal plan"
    )
})

test_that("RecipeBook meal_plan setter catches user input errors", {
    expect_error(
        meal_plan(RecipeBook_example) <- dplyr::tibble(invalid = "x"),
        "object@meal_plan must have the columns;"
    )
    expect_error(
        meal_plan(RecipeBook_example) <- dplyr::tibble(days = "x", meals = 1),
        "object@meal_plan must have the columns;"
    )
})
