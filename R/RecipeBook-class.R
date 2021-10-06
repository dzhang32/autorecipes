#' RecipeBook containing recipes and ingredients
#'
#' @description
#'
#' A book of recipes from which meal plans can be generated.
#'
#' @param x a `RecipeBook` object.
#'
#' @name RecipeBook-class
#' @aliases RecipeBook
#' @exportClass RecipeBook
setClass("RecipeBook",
    slots = c(
        recipes = "tbl_df",
        meal_plan = "tbl_df"
    )
)

setOldClass(class(dplyr::tibble()))

##### constructor #####

#' @keywords internal
#' @noRd
initialize_RecipeBook <- function(.Object, names, ingredients, ...) {
    .Object <- callNextMethod(.Object, ...)
    recipes <- dplyr::tibble(
        names = names,
        ingredients = ingredients,
        fav = FALSE
    )
    meal_plan <- dplyr::tibble()

    .Object@recipes <- recipes
    .Object@meal_plan <- meal_plan
    validObject(.Object)
    .Object
}

setMethod("initialize", "RecipeBook", initialize_RecipeBook)

##### constructor #####

RecipeBook <- function(names, ingredients) {
    names <- as.character(names)

    # allow users to put in a character for ingredients
    if (is.character(ingredients)) {
        ingredients <- read_ingredients(ingredients)
    }

    new("RecipeBook", names = names, ingredients = ingredients)
}

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
    delim = ";",
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

##### validator #####

#' @keywords internal
#' @noRd
valid_RecipeBook <- function(object) {
    if (.check_recipes_nrow(object)) {
        "object@recipes must have > 0 rows"
    } else if (.check_recipes_colnames(object)) {
        "object@recipes must contain 'names' and 'ingredients' columns"
    } else if (.check_recipes_names(object)) {
        "object@recipes[['names']] must be a character"
    } else if (.check_recipes_ingredients(object)) {
        paste(
            "object@recipes[['ingredients']] must be a list",
            "containing Ingredients-class objects"
        )
    } else if (.check_recipes_fav(object)) {
        "object@recipes[['fav']] must be logical"
    }else {
        TRUE
    }
}


setValidity("RecipeBook", valid_RecipeBook)

#' @keywords internal
#' @noRd
.check_recipes_nrow <- function(object) {
    return(nrow(object@recipes) == 0)
}

#' @keywords internal
#' @noRd
.check_recipes_colnames <- function(object) {
    return(!all(c("names", "ingredients") %in% colnames(object@recipes)))
}

#' @keywords internal
#' @noRd
.check_recipes_names <- function(object) {
    return(!is.character(object@recipes[["names"]]))
}

#' @keywords internal
#' @noRd
.check_recipes_ingredients <- function(object) {
    check_ingredients <- FALSE

    if (!is.list(object@recipes[["ingredients"]])) {
        check_ingredients <- TRUE
    } else {
        all_ingredients <- object@recipes[["ingredients"]] %>%
            lapply(
                FUN = function(x) methods::is(x, "Ingredients")
            ) %>%
            unlist() %>%
            all()

        if (!all_ingredients) {
            check_ingredients <- TRUE
        }
    }

    return(check_ingredients)
}

.check_recipes_fav <- function(object) {
    return(!is.logical(object@recipes[["fav"]]))
}
