#' Create a shopping list
#'
#' @inheritParams create_meal_plan
#' @param filter_method `character()` the method to used to filter out store
#'   cupboard ingredients from your shopping list.
#'
#' @return `tibble::tibble()` containing a shopping list.
#' @export
create_shopping_list <- function(recipebook,
                                 filter_method = c(
                                   "none",
                                   "manual",
                                   "minimal"
                                 )) {
  .check_object(recipebook, "RecipeBook")
  filter_method <- match.arg(filter_method)

  ingredients_df <- .extract_ingredients(recipebook)
  shopping_list <- .collapse_ingredients(ingredients_df)

  if (filter_method != "none") {
    filter_shopping_list_func <- .dispatch_shopping_list_filter(filter_method)
    store_cupboard <- filter_shopping_list_func()

    shopping_list <- shopping_list %>%
      dplyr::filter(!(names %in% store_cupboard))
  }

  shopping_list(recipebook) <- shopping_list

  return(recipebook)
}

#' @keywords internal
#' @noRd
.extract_ingredients <- function(recipebook) {
  # TODO account for when multiple of the same recipe are in meal plan
  # currently this will not take into account duplicates
  ingredients_df <- recipes(recipebook) %>%
    dplyr::filter(index %in% meal_plan(recipebook)[["recipe_index"]]) %>%
    .[["ingredients"]] %>%
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
    dplyr::ungroup() %>%
    dplyr::arrange(dplyr::desc(n))

  return(ingredients_collapsed)
}

#' @keywords internal
#' @noRd
.dispatch_shopping_list_filter <- function(filter_method) {
  switch(filter_method,
    "manual" = .filter_shopping_list_manual,
    "minimal" = .all_store_cupboard
  )
}

#' @keywords internal
#' @noRd
.filter_shopping_list_manual <- function(con = stdin()) {
  store_cupboard <- .all_store_cupboard()

  .select_index_message(
    store_cupboard,
    "store cupboard ingredients", "include", "shopping list"
  )

  indexes <- readLines(con = con, n = 1L)
  indexes <- .check_tidy_index_input(indexes, seq_along(store_cupboard))

  store_cupboard <- store_cupboard[-c(indexes)]

  return(store_cupboard)
}

#' @keywords internal
#' @noRd
.all_store_cupboard <- function() {
  store_cupboard <- c(
    "garlic clove", "garlic",
    "onion", "red onion",
    "ginger",
    "lime", "lemon",
    "butter", "italian style hard cheese",
    "egg",
    "plain flour", "polenta",
    "bouillon powder", "salt", "pepper", "sugar",
    "balsamic vinegar",
    "chilli flakes", "dried chilli flakes",
    "tomato puree", "worcester sauce",
    "coarse grain mustard", "dijon mustard",
    "harissa", "pine nuts",
    "honey",
    "dried oregano", "dried rosemary", "dried thyme", "dried mint",
    "ground ginger", "ground turmeric", "ground cumin",
    "smoked paprika", "turmeric",
    "hoisin sauce", "mirin", "sweet chilli sauce", "toasted sesame oil",
    "tamari",
    "lasagne spice pot", "masala spice pot", "refried bean spice pot",
    "seed spice pot", "spiced tomato spice pot", "tostada spice pot",
    "red pesto", "chipotle paste",
    "tins chopped tomatoes"
  )

  return(store_cupboard)
}
