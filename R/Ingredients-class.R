#' @title An S4 class representing a set of ingredients.
#'
#' @description Contains information regarding the name, the quantity and the
#' units for each ingredient.
#'
#' @param x of class `Ingredients-class`
#' @param names `character()` name of each ingredient.
#' @param amounts `numeric()` amount of each ingredient.
#' @param units `character()` units for the amount of each ingredient.
#'
#' @slot names name of each ingredient.
#' @slot amounts amount of each ingredient.
#' @slot units units for the amount of each ingredient.
#'
#' @export Ingredients
#' @rdname Ingredients-class
setClass("Ingredients",
    slots = c(
        names = "character",
        amounts = "numeric",
        units = "character"
    ),
    prototype = list(
        names = NA_character_,
        amounts = NA_real_,
        units = NA_character_
    )
)

##### constructor #####

#' @rdname Ingredients-class
Ingredients <- function(names,
    amounts = 1,
    units = rep(NA_character_, length(names))) {
    names <- stringr::str_to_lower(names)

    # convert fractions to decimals, also character to numeric
    amounts <- sapply(amounts, function(x) eval(parse(text = x)))

    # if no amount value, assume we want 1
    amounts <- ifelse(is.na(amounts), 1, amounts)

    new("Ingredients", names = names, amounts = amounts, units = units)
}

##### validator #####

valid_Ingredients <- function(object) {
    valid_units <- .all_valid_units()

    if (.check_Ingredient_names_amounts_length(object)) {
        "object@names and object@amounts must have equal lengths"
    } else if (.check_Ingredient_names(object)) {
        "All object@names values must be lower case"
    } else if (.check_Ingredient_units(object, valid_units)) {
        paste(
            "object@units must be the same length as",
            "object@names and object@amounts and be one of;",
            paste(valid_units, collapse = ", ")
        )
    } else {
        TRUE
    }
}

setValidity("Ingredients", valid_Ingredients)

#' @keywords internal
#' @noRd
.check_Ingredient_names_amounts_length <- function(object) {
    return(length(object@names) != length(object@amounts))
}

#' @keywords internal
#' @noRd
.check_Ingredient_names <- function(object) {
    # check all ingredient names are lowercase
    return(any(stringr::str_detect(object@names, "[[:upper:]]")))
}

#' @keywords internal
#' @noRd
.check_Ingredient_units <- function(object, valid_units) {
    check_units <- FALSE

    if (length(object@names) != length(object@units)) {
        check_units <- TRUE
    }

    if (!all(object@units %in% valid_units)) {
        check_units <- TRUE
    }

    return(check_units)
}

#' @keywords internal
#' @noRd
.all_valid_units <- function(no_na = FALSE, regex = FALSE) {
    valid_units <- c(
        "g", "kg",
        "tsp", "tbsp", "teaspoon", "teaspoon",
        "ml",
        "cm",
        "punnet",
        "bag", "pack", "carton", "piece", "pot", "tin"
    )

    # required for Ingredients-class constructor as some food has no units
    if (!no_na) {
        valid_units <- c(NA_character_, valid_units)
    }

    # if units used for regex, add a space to the end and collapse with an "|"
    # This avoids matching e.g. the unit "g" in the word "ginger"
    if (regex) {
        valid_units <- valid_units %>%
            stringr::str_c(., " ") %>%
            stringr::str_c(collapse = "|")
    }

    return(valid_units)
}
