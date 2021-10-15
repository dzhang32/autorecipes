test_recipebook <- recipebook_example
test_recipebook <- create_meal_plan(test_recipebook, set_last_eaten = FALSE)

##### create_meal_plan #####

test_that("create_meal_plan output looks broadly correct", {
  expect_true(nrow(meal_plan(test_recipebook)) == 14)
  expect_false(any(duplicated(test_recipebook@meal_plan[["recipe_index"]])))
  expect_true(all(is.na(
    test_recipebook@recipes[["last_eaten"]]
  )))
})

test_that("create_meal_plan works for a single recipe", {
  test_recipebook@recipes <- test_recipebook@recipes[1, ]
  test_recipebook <- create_meal_plan(test_recipebook)
  expect_true(
    length(unique(meal_plan(test_recipebook)[["recipe_index"]])) == 1
  )
  expect_true(
    test_recipebook@recipes[["last_eaten"]] == lubridate::today()
  )
})

test_that("create_meal_plan works for favourite recipes", {
  test_recipebook <- add_favourites(test_recipebook, "index", 6:10)
  test_recipebook <- create_meal_plan(
    test_recipebook,
    fav_only = TRUE
  )
  expect_true(identical(
    sort(unique(meal_plan(test_recipebook)[["recipe_index"]])),
    6:10
  ))
  expect_true(all(
    test_recipebook@recipes[["last_eaten"]][6:10] == lubridate::today()
  ))
  expect_true(all(is.na(
    test_recipebook@recipes[["last_eaten"]][1:5]
  )))
})

test_that("create_meal_plan works for varying days/meals", {
  test_recipebook <- create_meal_plan(
    test_recipebook,
    c("Wed", "Thurs"),
    "Dinner"
  )
  expect_true(identical(
    as.character(meal_plan(test_recipebook)[["day"]]),
    c("Wed", "Thurs")
  ))
  expect_true(identical(
    as.character(unique(meal_plan(test_recipebook)[["meal"]])),
    c("Dinner")
  ))
})

##### .create_meal_plan_auto #####

test_that(".create_meal_plan_auto works correctly", {
  test_recipebook@recipes[["last_eaten"]][1:2] <- lubridate::today()
  test_recipebook@recipes[["last_eaten"]][3:4] <- lubridate::today() - 5

  set.seed(32)
  test_recipe_indexes <- .create_meal_plan_auto(
    recipes = test_recipebook@recipes[1:6, ],
    num_required = 3
  )

  exp_rank <- rep(1:3, 2) %>%
    sort() %>%
    rank()
  exp_prob <- exp_rank / sum(exp_rank)
  set.seed(32)
  exp_recipe_indexes <- sample(1:6, 3, prob = exp_prob)

  expect_true(length(test_recipe_indexes) == 3)
  expect_true(identical(
    test_recipe_indexes,
    exp_recipe_indexes
  ))
})

##### .create_meal_plan_random #####

test_that(".create_meal_plan_random works correctly", {
  test_recipe_indexes <- .create_meal_plan_random(
    recipes = test_recipebook@recipes[1:10, ],
    num_required = 20
  )

  expect_true(length(test_recipe_indexes) == 20)

  # make sure that the first 10 recipes do not repeat
  expect_true(length(unique(test_recipe_indexes[1:10])) == 10)
  expect_true(length(unique(test_recipe_indexes)) == 10)
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
    c("Mon", "Sun"),
    c("Lunch")
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
    .create_calendar(which_days = c("mon")),
    "which_days must be one of: Mon, Tues, Wed, Thurs, Fri, Sat, Sun"
  )

  expect_error(
    .create_calendar(which_meals = c("lunch")),
    "meals must be one of: Lunch, Dinner"
  )

  expect_warning(
    .create_calendar(which_meals = c("Lunch", "Lunch")),
    "which_meals must not contain duplicated values, coercing"
  )
})
