##### set_recipes #####

#' Add or remove recipes
#'
#' @inheritParams RecipeBook-class
#' @inheritParams set_favourites
#' @param recipebook object of `RecipeBook-class`.
#'
#' @name set_recipes
NULL

#' @rdname set_recipes
#' @export
add_recipes <- function(recipebook,
                        names,
                        ingredients) {

  # Use RecipeBook constructor
  # inefficient, but ensures correct format of recipes
  recipes_to_add <- RecipeBook(names = names, ingredients = ingredients) %>%
    recipes()

  recipes(recipebook) <- recipes(recipebook) %>%
    dplyr::bind_rows(recipes_to_add) %>%
    dplyr::mutate(index = dplyr::row_number())

  validObject(recipebook)

  return(recipebook)
}

#' @rdname set_recipes
#' @export
rm_recipes <- function(recipebook,
                       indexes = NULL,
                       method = c("manual", "index")) {
  .check_object(recipebook, "RecipeBook")
  method <- match.arg(method)

  recipes_remover_func <- .dispatch_recipe_remover(method)
  indexes <- recipes_remover_func(recipebook, indexes)

  recipes(recipebook) <- recipes(recipebook) %>%
    dplyr::filter(!(index %in% indexes)) %>%
    dplyr::mutate(index = dplyr::row_number())

  recipebook <- .reset_meal_plan(recipebook)

  return(recipebook)
}

#' @keywords internal
#' @noRd
.dispatch_recipe_remover <- function(method) {
  switch(method,
    "index" = .rm_recipes_index,
    "manual" = .rm_recipes_manual
  )
}

#' @keywords internal
#' @noRd
.rm_recipes_index <- function(recipebook, indexes) {
  if (is.null(indexes)) {
    stop("'index' method requires non-NULL indexes")
  } else if (!is.numeric(indexes)) {
    stop("indexes must be numeric")
  }
  indexes <- as.integer(indexes)

  return(indexes)
}

#' @keywords internal
#' @noRd
.rm_recipes_manual <- function(recipebook, indexes, con = stdin()) {
  .select_index_message(
    names(recipebook),
    "recipes", "favourite", "recipebook"
  )

  indexes <- readLines(con = con, n = 1L)
  indexes <- .check_tidy_index_input(indexes, seq_along(names(recipebook)))

  return(indexes)
}

#' @keywords internal
#' @noRd
.reset_meal_plan <- function(recipebook) {
  if (nrow(recipebook@meal_plan) > 0) {
    message("Resetting meal plan, as indexes of recipes have been changed")
    meal_plan(recipebook) <- dplyr::tibble()
  }

  return(recipebook)
}
