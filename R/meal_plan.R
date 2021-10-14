#' Create a meal plan
#'
#' @param recipebook a `RecipeBook-class` object.
#' @param days `character()` which days of the week to plan for.
#' @param meals `character()` which meals to plan for, out of "Lunch" and
#'   "Dinner".
#' @param method `character()` the method to use when creating a meal plan.
#' @param fav_only `logical()` whether to create a meal plan only using
#'   favourite recipes.
#' @param set_last_eaten `logical()` whether to save the recipes
#'   were eaten today. Used to optimise the meal planning algorithm when the
#'   `method` is set to "auto".
#'
#' @return a `RecipeBook-class` object containing the created `meal_plan`.
#' @export
#'
#' @examples
#' recipebook <- create_meal_plan(recipebook_example)
#'
#' meal_plan(recipebook)
create_meal_plan <- function(recipebook,
                             days = c("Mon", "Tues", "Wed", "Thurs", "Fri", "Sat", "Sun"),
                             meals = c("Lunch", "Dinner"),
                             method = c("auto", "random"),
                             fav_only = FALSE,
                             set_last_eaten = TRUE) {
  if (!is(recipebook, "RecipeBook")) {
    stop("recipebook must be instance of RecipeBook-class")
  }
  validObject(recipebook)

  method <- match.arg(method)
  calendar <- .create_calendar(days, meals)

  meal_plan_func <- .dispatch_meal_planner(method)

  if (fav_only) {
    chosen_recipe_indexes <- meal_plan_func(
      favourites(recipebook),
      nrow(calendar)
    )
  } else {
    chosen_recipe_indexes <- meal_plan_func(
      recipes(recipebook),
      nrow(calendar)
    )
  }

  if (set_last_eaten) {
    last_eaten(recipebook) <- chosen_recipe_indexes
  }

  meal_plan(recipebook) <- calendar %>%
    dplyr::mutate(recipe_index = chosen_recipe_indexes)

  return(recipebook)
}

#' @keywords internal
#' @noRd
.create_calendar <- function(days = c("Mon", "Tues", "Wed", "Thurs", "Fri", "Sat", "Sun"),
                             meals = c("Lunch", "Dinner")) {
  valid_days <- c("Mon", "Tues", "Wed", "Thurs", "Fri", "Sat", "Sun")
  valid_meals <- c("Lunch", "Dinner")

  if (any(duplicated(days)) | any(duplicated(meals))) {
    warning("days or meals must not contain duplicated values, coercing")
    days <- unique(days)
    meals <- unique(meals)
  }

  if (any(!(days %in% valid_days))) {
    stop(
      "days must be one of: ",
      stringr::str_c(valid_days, collapse = ", ")
    )
  }

  if (any(!(meals %in% valid_meals))) {
    stop(
      "meals must be one of: ",
      stringr::str_c(valid_meals, collapse = ", ")
    )
  }

  calendar <- tidyr::expand_grid(
    day = factor(days, levels = valid_days),
    meal = factor(meals, levels = valid_meals)
  )

  return(calendar)
}

#' @keywords internal
#' @noRd
.dispatch_meal_planner <- function(method) {
  switch(method,
    "auto" = .create_meal_plan_random,
    "random" = .create_meal_plan_random
  )
}

#' @keywords internal
#' @noRd
.create_meal_plan_random <- function(recipes, num_required) {
  # need to extract [["index"]] vs seq_along()
  # in the case recipes are filtered by favourites()
  # while used to make sure recipes don't repeat until they're all used once
  # avoid e.g. 1, 1, 1, 3, 4 ... occurring if replace = TRUE
  chosen_recipe_indexes <- c()

  while (length(chosen_recipe_indexes) < num_required) {
    chosen_recipe_indexes <- c(
      chosen_recipe_indexes,
      sample(recipes[["index"]], nrow(recipes))
    )
  }

  chosen_recipe_indexes <- chosen_recipe_indexes[1:num_required]

  return(chosen_recipe_indexes)
}
