#' @title An S4 class representing a set of recipes
#'
#' @description Name, ingredients of recipes.
#'
#' @param x of class `RecipeBook-class`
#' @param names `character()` name of each recipe.
#' @param ingredients `list()` set of ingredients required for each recipe.
#'
#' @slot names name of each recipe.
#' @slot ingredients set of ingredients required for each recipe.
#'
#' @export RecipeBook
#' @rdname RecipeBook-class
setClass("RecipeBook",
    slots = c(
        names = "character",
        ingredients = "list"
    ),
    prototype = list(
        names = NA_character_,
        ingredients = list()
    )
)

##### constructor #####

#' @rdname RecipeBook-class
RecipeBook <- function(names,
    ingredients) {
    new("RecipeBook", names = names, ingredients = ingredients)
}

##### validator #####

validRecipeBook <- function(object) {
    valid_units <- c(NA_character_, "g")

    if (.check_RecipeBook_names_ingredients_lengths(object)) {
        "object@names and object@ingredients must have equal lengths"
    } else if (.check_RecipeBook_ingredients(object)) {
        "object@ingredients must be a list containing Ingredients-class objects"
    } else {
        TRUE
    }
}

setValidity("RecipeBook", validRecipeBook)

#' @keywords internal
#' @noRd
.check_RecipeBook_names_ingredients_lengths <- function(object) {
    return(length(object@names) != length(object@ingredients))
}

#' @keywords internal
#' @noRd
.check_RecipeBook_ingredients <- function(object) {
    check_ingredients <- FALSE

    if (length(object@ingredients) != 0) {
        all_ingredients <- lapply(
            object@ingredients,
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
