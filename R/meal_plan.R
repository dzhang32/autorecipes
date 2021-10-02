#' Create a meal plan
#'
#' @param recipebook `tibble::tibble()` containing the recipes from which meal
#'   plan will be created.
#' @param method `character()` the method to use when creating a meal plan.
#'
#' @return A meal plan
#' @export
create_meal_plan <- function(recipebook,
    method = c("auto", "random")) {
    calender <- .create_calender()
    method <- match.arg(method)

    meal_plan_func <- .dispatch_meal_plan(method)

    chosen_recipe_indexes <- meal_plan_func(recipebook, nrow(calender))

    meal_plan <- dplyr::bind_rows(calender, recipebook[, chosen_recipe_indexes])

    return(meal_plan)
}

#' @keywords internal
#' @noRd
.create_calender <- function() {
    calender <- tidyr::expand_grid(
        day = c("Mon", "Tues", "Wed", "Thurs", "Fri", "Sat", "Sun"),
        meal = c("Lunch", "Dinner")
    )

    return(calender)
}

#' @keywords internal
#' @noRd
.dispatch_meal_plan <- function(method) {
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
