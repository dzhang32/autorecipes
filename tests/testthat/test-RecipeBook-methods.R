##### show #####

test_that("RecipeBook prints as expected", {
    expect_true(identical(
        capture.output(print(RecipeBook_example)),
        capture.output(print(RecipeBook_example@recipes))
    ))
})

##### getters #####

test_that("Ingredients getters work as expected", {
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
    expect_message(
        favourites(RecipeBook_example),
        "No favourite recipes set, have you run `favorites<-`?"
    )
})

##### setters #####

test_that("Ingredients setters work as expected", {
    favourites(RecipeBook_example) <- c(1, 2, 3)

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
    expect_message(
        favourites(RecipeBook_example),
        "No favourite recipes set, have you run `favorites<-`?"
    )
})

##### as.data.frame #####

test_that("Test converting Ingredients-class to data.frame", {
    test_ingred_df <- as.data.frame(test_ingred)
    expect_true(is.data.frame(test_ingred_df))
    expect_true(identical(
        test_ingred_df,
        data.frame(
            names = test_ingred@names,
            amounts = test_ingred@amounts,
            units = test_ingred@units
        )
    ))
})
