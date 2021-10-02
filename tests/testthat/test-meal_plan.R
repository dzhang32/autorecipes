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
