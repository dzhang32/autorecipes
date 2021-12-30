#' Create a customisable meal plan
#'
#' `create_meal_plan()` expects a `RecipeBook-class` object as input. The
#' available options for customizing the meal plan include; the days of the week
#' and the meals ("Lunch" or "Dinner") to plan for, the method to use and
#' whether to only use favourite recipes.
#'
#' @param recipebook a `RecipeBook-class` object.
#' @param which_days `character()` which days of the week to plan for, in the
#'   format "Mon"
#' @param which_meals `character()` which meals to plan for, one of "Lunch" or
#'   "Dinner".
#' @param method `character()` the method to use when creating a meal plan, one
#'   of "auto", or "random".
#' @param fav_only `logical()` whether to only pick from favourite recipes.
#' @param set_last_eaten `logical()` whether to record the that selected recipes
#'   were used for a meal plan today. This info is taken into account when
#'   optimising the meal planning algorithm if `method` is set to "auto".
#'
#' @return a `RecipeBook-class` object containing the created `meal_plan`.
#' @export
#'
#' @examples
#' recipebook <- create_meal_plan(recipebook_example)
#'
#' meal_plan(recipebook)
create_meal_plan <- function(recipebook,
                             which_days = weekdays(),
                             which_meals = c("Lunch", "Dinner"),
                             method = c("auto", "random"),
                             fav_only = FALSE,
                             set_last_eaten = TRUE) {
  .check_object(recipebook, "RecipeBook")
  method <- match.arg(method)

  calendar <- .create_calendar(which_days, which_meals)

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
.create_calendar <- function(which_days = weekdays(),
                             which_meals = c("Lunch", "Dinner")) {
  valid_days <- weekdays()
  valid_meals <- c("Lunch", "Dinner")

  # will de-duplicate and check which_days are valid
  which_days <- weekdays(which_days)

  if (any(duplicated(which_meals))) {
    warning("which_meals must not contain duplicated values, coercing")
    which_meals <- unique(which_meals)
  }

  if (any(!(which_meals %in% valid_meals))) {
    stop(
      "meals must be one of: ",
      stringr::str_c(valid_meals, collapse = ", ")
    )
  }

  calendar <- tidyr::expand_grid(
    day = factor(which_days, levels = valid_days),
    meal = factor(which_meals, levels = valid_meals)
  )

  return(calendar)
}

#' @keywords internal
#' @noRd
.dispatch_meal_planner <- function(method) {
  switch(method,
    "auto" = .create_meal_plan_auto,
    "random" = .create_meal_plan_random
  )
}

#' @keywords internal
#' @noRd
.create_meal_plan_auto <- function(recipes, num_required) {
  if (!all(is.na(recipes[["last_eaten"]]))) {
    # set a probability so the later the recipe was last eaten
    # the greater the chance it's picked:
    # 1. set NAs (never eaten) to earliest date - 1
    # 2. rank by last_eaten, the default method only allows ascending
    # 3. re-rank using -rank (descending so earlier dates = higher rank)
    # 4. normalise ranks into prob by dividing by sum(rank)
    prob <- recipes %>%
      dplyr::mutate(
        last_eaten = dplyr::if_else(
          is.na(last_eaten),
          min(last_eaten, na.rm = TRUE) - 1,
          last_eaten
        ),
        rank_last_eaten = rank(last_eaten),
        rank_last_eaten = rank(-rank_last_eaten),
        prob = rank_last_eaten / sum(rank_last_eaten)
      ) %>%
      .[["prob"]]
  } else {
    prob <- NULL
  }

  chosen_recipe_indexes <- .create_meal_plan_random(recipes, num_required, prob)

  return(chosen_recipe_indexes)
}

#' @keywords internal
#' @noRd
.create_meal_plan_random <- function(recipes, num_required, prob = NULL) {
  # need to extract [["index"]] rather than seq_along()
  # in the case recipes are filtered by favourites()
  # while() is used so recipes don't repeat until all used at least once
  # this avoids e.g. 1, 1, 1, 3, 4 ... occurring if replace = TRUE was used
  chosen_recipe_indexes <- c()

  while (length(chosen_recipe_indexes) < num_required) {
    chosen_recipe_indexes <- c(
      chosen_recipe_indexes,
      sample(recipes[["index"]], nrow(recipes), prob = prob)
    )
  }

  chosen_recipe_indexes <- chosen_recipe_indexes[1:num_required]

  return(chosen_recipe_indexes)
}
