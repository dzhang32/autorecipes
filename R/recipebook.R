#' Create a recipebook - a tibble storing recipes
#'
#' @param names `character()` names of each recipe
#' @param ingredients `list()` of elements of `Ingredients-class`
#'
#' @return A `tibble::tibble()` containing recipe details.
#' @export
#'
#' @examples
#'
#' test_ingred <- Ingredients(
#'     names = c(
#'         "chicken",
#'         "salt",
#'         "pepper"
#'     ),
#'     amounts = c(1, 2, 3),
#'     units = c(NA_character_, "g", "g")
#' )
#'
#' create_recipebook(c("Roast Chicken"), list(test_ingred))
create_recipebook <- function(names, ingredients) {
    if (!identical(length(names), length(ingredients))) {
        stop("All arguments must of equal length")
    }
    if (length(names) == 0 || length(ingredients) == 0) {
        stop("All arguments must have length > 0")
    }

    recipebook <- dplyr::tibble(
        names = names,
        ingredients = ingredients
    )

    valid_recipebook(recipebook)

    return(recipebook)
}

#' @keywords internal
#' @noRd
valid_recipebook <- function(recipebook) {
    if (.check_recipebook_colnames(recipebook)) {
        stop("recipebook must contain 'names' and 'ingredients' columns")
    } else if (.check_recipebook_names(recipebook[["names"]])) {
        stop("names must be a character")
    } else if (.check_recipebook_ingredients(recipebook[["ingredients"]])) {
        stop("ingredients must be a list containing Ingredients-class objects")
    }
}

#' @keywords internal
#' @noRd
.check_recipebook_colnames <- function(recipebook) {
    return(!all(c("names", "ingredients") %in% colnames(recipebook)))
}

#' @keywords internal
#' @noRd
.check_recipebook_names <- function(names) {
    return(!is.character(names))
}

#' @keywords internal
#' @noRd
.check_recipebook_ingredients <- function(ingredients) {
    check_ingredients <- FALSE

    if (length(ingredients) != 0) {
        all_ingredients <- lapply(
            ingredients,
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
