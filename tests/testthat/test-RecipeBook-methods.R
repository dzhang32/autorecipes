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
