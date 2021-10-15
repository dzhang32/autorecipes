#' Create a shopping list
#'
#' @inheritParams create_meal_plan
#'
#' @return `tibble::tibble()` containing a shopping list.
#' @export
create_shopping_list <- function(recipebook,
                                 method = c("none", "manual", "minimal")) {
  .check_object(recipebook, "RecipeBook")
  method <- match.arg(method)

  ingredients_df <- .extract_ingredients(recipebook)
  shopping_list <- .collapse_ingredients(ingredients_df)

  filter_shopping_list_func <- .dispatch_shopping_list_filter(method)
  shopping_list(recipebook) <- filter_shopping_list_func(shopping_list)

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
.dispatch_shopping_list_filter <- function(method) {
  switch(method,
    "none" = .filter_shopping_list_none,
    "manual" = .filter_shopping_list_manual,
    "minimal" = .filter_shopping_list_minimal
  )
}

#' @keywords internal
#' @noRd
.filter_shopping_list_none <- function(shopping_list) {
  return(shopping_list)
}

#' @keywords internal
#' @noRd
.filter_shopping_list_manual <- function(shopping_list, con = stdin()) {
  store_cupboard <- .all_store_cupboard()

  message(
    stringr::str_c(
      seq_along(store_cupboard), " - ",
      store_cupboard, "\n"
    ),
    "\nPlease select the store cupboard ingredients you want to include ",
    "in your shopping list from the above.",
    "\nEnter the indexes, separated with a ',' - for example '1,2,3' to ",
    "include the first three ingredients."
  )

  include_indexes <- readLines(con = con, n = 1L)

  if (include_indexes != "") {
    include_indexes <- include_indexes %>%
      stringr::str_split(",") %>%
      unlist() %>%
      stringr::str_trim() %>%
      as.integer()

    store_cupboard <- store_cupboard[-c(include_indexes)]
  }

  shopping_list <- shopping_list %>%
    dplyr::filter(!(names %in% store_cupboard))

  return(shopping_list)
}

#' @keywords internal
#' @noRd
.filter_shopping_list_minimal <- function(shopping_list) {
  store_cupboard <- .all_store_cupboard()

  shopping_list <- shopping_list %>%
    dplyr::filter(!(names %in% store_cupboard))

  return(shopping_list)
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
