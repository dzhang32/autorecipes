#' Create a meal plan
#'
#' @param recipebook `tibble::tibble()` containing the recipe information,
#'   possibly created using `create_recipebook()`.
#' @param method `character()` the method to use when creating a meal plan.
#' @param fav_only `logical()` whether to create a meal plan only using
#'   favourite recipes.
#'
#' @return `tibble::tibble()` containing a meal plan.
#' @export
#'
#' @examples
#' meal_plan <- create_meal_plan(recipebook_example)
#'
#' meal_plan
create_meal_plan <- function(recipebook,
    method = c("auto", "random"),
    fav_only = FALSE) {
    calendar <- .create_calendar()
    method <- match.arg(method)

    recipebook <- .filter_recipebook(recipebook, fav_only)
    .valid_recipebook(recipebook)

    meal_plan_func <- .dispatch_meal_planner(method)

    chosen_recipe_indexes <- meal_plan_func(recipebook, nrow(calendar))

    meal_plan <- dplyr::bind_cols(calendar, recipebook[chosen_recipe_indexes, ])

    return(meal_plan)
}

#' @keywords internal
#' @noRd
.create_calendar <- function() {
    days <- c("Mon", "Tues", "Wed", "Thurs", "Fri", "Sat", "Sun")
    meals <- c("Lunch", "Dinner")

    calendar <- tidyr::expand_grid(
        day = factor(days, levels = days),
        meal = factor(meals, levels = meals)
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
.create_meal_plan_random <- function(recipebook, num_required) {
    replace <- if (nrow(recipebook) < num_required) TRUE else FALSE

    chosen_recipe_indexes <- sample(
        seq_len(nrow(recipebook)), num_required,
        replace = replace
    )

    return(chosen_recipe_indexes)
}

#' @keywords internal
#' @noRd
.filter_recipebook <- function(recipebook, fav_only) {
    if (fav_only) {
        if (!("fav" %in% colnames(recipebook))) {
            stop("To filter by favourites, recipebook must have the column 'fav'")
        }
        recipebook <- recipebook %>% dplyr::filter(fav)
    }

    return(recipebook)
}
