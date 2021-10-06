#' Create a meal plan
#'
#' @param recipebook a `RecipeBook-class` object.
#' @param days `character()` which days of the week to plan for.
#' @param meals `character()` which meals to plan for, out of "Lunch" and
#'   "Dinner".
#' @param method `character()` the method to use when creating a meal plan.
#' @param fav_only `logical()` whether to create a meal plan only using
#'   favourite recipes.
#'
#' @return a `RecipeBook-class` object containing the created `meal_plan`.
#' @export
#'
#' @examples
#' meal_plan(RecipeBook_example) <- create_meal_plan(RecipeBook_example)
#'
#' meal_plan(RecipeBook_example)
create_meal_plan <- function(recipebook,
    days = c("Mon", "Tues", "Wed", "Thurs", "Fri", "Sat", "Sun"),
    meals = c("Lunch", "Dinner"),
    method = c("auto", "random"),
    fav_only = FALSE) {
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
        chosen_recipe_indexes <- meal_plan_func(recipes(recipebook), nrow(calendar))
    }

    meal_plan <- calendar %>%
        dplyr::mutate(recipe_index = chosen_recipe_indexes)

    return(meal_plan)
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
    replace <- if (nrow(recipes) < num_required) TRUE else FALSE

    chosen_recipe_indexes <- sample(
        recipes[["index"]], num_required,
        replace = replace
    )

    return(chosen_recipe_indexes)
}
