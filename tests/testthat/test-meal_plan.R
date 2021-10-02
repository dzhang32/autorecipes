##### create_meal_plan #####

test_that("create_meal_plan output looks broadly correct", {
    test_meal_plan <- create_meal_plan(recipebook_example)
    expect_true(nrow(test_meal_plan) == 14)
    expect_false(any(duplicated(test_meal_plan[["names"]])))
})

test_that("create_meal_plan works for a single recipe", {
    test_meal_plan <- create_meal_plan(recipebook_example[1, ])
    expect_true(length(unique(test_meal_plan[["names"]])) == 1)
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

##### .filter_recipebook #####

test_that(".filter_recipebook output looks correct", {
    test_recipebook <- recipebook_example
    test_recipebook[["fav"]] <- FALSE
    test_recipebook[["fav"]][1:3] <- TRUE

    expect_true(identical(
        .filter_recipebook(test_recipebook, fav_only = TRUE),
        test_recipebook %>% dplyr::filter(fav)
    ))
    expect_true(identical(
        .filter_recipebook(recipebook_example, fav_only = FALSE),
        recipebook_example
    ))
})

test_that(".filter_recipebook catches user input errors", {
    expect_error(
        .filter_recipebook(recipebook_example, fav_only = TRUE),
        "To filter by favourites, recipebook must have the column 'fav'"
    )
})
