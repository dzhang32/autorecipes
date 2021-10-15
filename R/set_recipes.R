##### set_recipes #####

#' Add or remove recipes
#'
#' @param recipebook object of `RecipeBook-class`.
#'
#' @name set_recipes
NULL

#' @rdname set_favourites
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
