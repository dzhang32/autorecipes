#' Create a shopping list
#'
#' @inheritParams create_meal_plan
#'
#' @return `tibble::tibble()` containing a shopping list.
#' @export
#'
#' @examples
#' meal_plan <- create_meal_plan(recipebook_example)
#' shopping_list <- create_shopping_list(meal_plan)
#'
#' shopping_list
create_shopping_list <- function(recipebook) {
    .valid_recipebook(recipebook)

    ingredients_df <- .extract_ingredients(recipebook)
    ingredients_collapsed <- .collapse_ingredients(ingredients_df)

    return(ingredients_collapsed)
}

#' @keywords internal
#' @noRd
.extract_ingredients <- function(recipebook) {
    ingredients_df <- recipebook[["ingredients"]] %>%
        lapply(FUN = as.data.frame) %>%
        do.call(rbind, .)

    return(ingredients_df)
}

#' @keywords internal
#' @noRd
.collapse_ingredients <- function(ingredients_df) {
    ingredients_collapsed <- ingredients_df %>%
        dplyr::group_by(names) %>%
        dplyr::count() %>%
        dplyr::ungroup()

    return(ingredients_collapsed)
}
