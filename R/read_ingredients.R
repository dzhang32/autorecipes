#' Read in a list of ingredients
#'
#' @param ingredients `character()` containing ingredient info separated by
#'   `delim`. Each element must denote the ingredients coming from each recipe.
#' @param delim `character()` that separates each ingredient.
#' @param method `character()` used to determine the method of parsing
#'   ingredients, placeholder for future implementations (e.g. with data.frame
#'   input).
#'
#' @return `list()` of elements of `Ingredients-class`.
#' @export
#'
#' @examples
#'
#' read_ingredients(
#'     c(
#'         "butter;6 eggs;1 bag coriander;1 punnet mixed baby tomatoes",
#'         "1 tsp garam masala;1 bag baby spinach;1 lemon;1 chilli"
#'     ),
#'     delim = ";"
#' )
read_ingredients <- function(ingredients,
    delim = NULL,
    method = "auto") {
    method <- match.arg(method)

    parse_ingredient_func <- .dispatch_ingredient_reader(method)

    ingredients_tidy <- parse_ingredient_func(ingredients, delim)

    return(ingredients_tidy)
}

#' @keywords internal
#' @noRd
.dispatch_ingredient_reader <- function(method) {
    switch(method,
        "auto" = .read_ingredients_auto
    )
}

#' @keywords internal
#' @noRd
.read_ingredients_auto <- function(ingredients, delim) {
    if (!is.null(delim)) {
        ingredients_parsed <- ingredients %>%
            stringr::str_split(delim)
    } else {
        stop("Currently, delim must be entered")
    }

    n_ingred_per_recipe <- ingredients_parsed %>%
        lapply(FUN = function(x) length(x)) %>%
        unlist()

    ingredients_tibble <-
        dplyr::tibble(
            recipe_index = rep(seq_along(ingredients_parsed), n_ingred_per_recipe),
            ingredients = unlist(ingredients_parsed)
        )

    amounts_regex <- "[1-9]/?[0-9]?[0-9]?"
    units_regex <- .all_valid_units(no_na = TRUE, regex = TRUE)

    # extract each individual component
    ingredients_tibble <- ingredients_tibble %>%
        dplyr::mutate(
            amounts = ingredients %>% stringr::str_extract(amounts_regex),
            units = ingredients %>% stringr::str_extract(units_regex) %>%
                stringr::str_trim(),
            names = ingredients %>%
                stringr::str_remove(amounts_regex) %>%
                stringr::str_remove(units_regex) %>%
                stringr::str_trim()
        )

    ingredients_tidy <- vector(mode = "list", length = length(ingredients_parsed))
    for (i in seq_along(ingredients_parsed)) {
        recipe_curr <- ingredients_tibble %>% dplyr::filter(recipe_index == i)

        ingredients_tidy[[i]] <- Ingredients(
            names = recipe_curr[["names"]],
            amounts = recipe_curr[["amounts"]],
            units = recipe_curr[["units"]]
        )
    }

    return(ingredients_tidy)
}
