test_RecipeBook <- RecipeBook_example
test_RecipeBook <- create_meal_plan(test_RecipeBook)
meal_plan(test_RecipeBook)

##### create_meal_plan #####

test_that("create_meal_plan output looks broadly correct", {
    expect_true(nrow(meal_plan(test_RecipeBook)) == 14)
    expect_false(any(duplicated(test_RecipeBook@meal_plan[["recipe_index"]])))
})

test_that("create_meal_plan works for a single recipe", {
    test_RecipeBook@recipes <- test_RecipeBook@recipes[1, ]
    test_RecipeBook <- create_meal_plan(test_RecipeBook)
    expect_true(
        length(unique(meal_plan(test_RecipeBook)[["recipe_index"]])) == 1
    )
})

test_that("create_meal_plan works for favourite recipes", {
    favourites(test_RecipeBook) <- 1:3
    test_RecipeBook <- create_meal_plan(test_RecipeBook, fav_only = TRUE)
    expect_true(identical(
        sort(unique(meal_plan(test_RecipeBook)[["recipe_index"]])),
        1:3
    ))
})

##### .create_calendar #####

test_that(".create_calendar output looks correct", {
    test_calendar <- .create_calendar()

    expect_true(is.factor(test_calendar[["day"]]))
    expect_true(is.factor(test_calendar[["meal"]]))
    expect_true(nrow(test_calendar) == 14)
    expect_true(identical(
        levels(test_calendar[["day"]]),
        c("Mon", "Tues", "Wed", "Thurs", "Fri", "Sat", "Sun")
    ))
    expect_true(identical(
        levels(test_calendar[["meal"]]),
        c("Lunch", "Dinner")
    ))
})

test_that(".create_calendar works with when filtering by days/meals", {
    test_calendar <- .create_calendar(
        days = c("Mon", "Sun"),
        meals = c("Lunch")
    )

    expect_true(nrow(test_calendar) == 2)
    expect_true(identical(
        test_calendar[["day"]] %>% as.character(),
        c("Mon", "Sun")
    ))
    expect_true(identical(
        test_calendar[["meal"]] %>% as.character(),
        c("Lunch", "Lunch")
    ))
})

test_that(".create_calendar works catches input errors", {
    expect_error(
        .create_calendar(days = c("mon")),
        "days must be one of: Mon, Tues, Wed, Thurs, Fri, Sat, Sun"
    )

    expect_error(
        .create_calendar(meals = c("lunch")),
        "meals must be one of: Lunch, Dinner"
    )

    expect_warning(
        .create_calendar(days = c("Mon", "Mon")),
        "days or meals must not contain duplicated values, coercing"
    )

    expect_warning(
        .create_calendar(meals = c("Lunch", "Lunch")),
        "days or meals must not contain duplicated values, coercing"
    )
})
