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
Ingredients <- function(names,
    amounts = 1,
    units = rep(NA_character_, length(names))) {
    names <- stringr::str_to_lower(names)
    amounts <- ifelse(is.na(amounts), 1, amounts)
    new("Ingredients", names = names, amounts = amounts, units = units)
}

##### validator #####
validIngredients <- function(object) {
    valid_units <- c(NA_character_, "g")

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

setValidity("Ingredients", validIngredients)

##### show #####

#' @keywords internal
#' @noRd
.show_Ingredients <- function(object) {
    units_no_NA <- units(object)
    units_no_NA[is.na(units_no_NA)] <- ""

    cat(stringr::str_c(
        amounts(object),
        units_no_NA, " ",
        names(object), "\n"
    ), sep = "")
}

setMethod("show", "Ingredients", .show_Ingredients)

##### methods #####

# getters
setMethod("names", "Ingredients", function(x) stringr::str_to_title(x@names))
setMethod("amounts", "Ingredients", function(x) x@amounts)
setMethod("units", "Ingredients", function(x) x@units)
