setClass("Ingredients",
    slots = c(
        name = "character",
        amount = "numeric",
        units = "character"
    ),
    prototype = list(
        name = NA_character_,
        amount = NA_real_,
        units = NA_character_
    )
)

# constructor
Ingredients <- function(name, amount = 1, units = NA_character_) {
    new("Ingredients", name = name, amount = amount, units = units)
}

# validator
validIngredients <- function(object) {
    valid_units <- c(NA_character_, "g")

    if (.check_ingredient_name_amount_length(object)) {
        "object@name and object@amount must have equal lengths"
    } else if (.check_ingredient_name(object)) {
        "All object@name values must be lower case"
    } else if (.check_ingredient_units(object, valid_units)) {
        paste(
            "object@units must be the same length as",
            "object@name and object@amount and be one of;",
            paste(valid_units, collapse = ", ")
        )
    } else {
        TRUE
    }
}

.check_ingredient_name_amount_length <- function(object) {
    return(length(object@name) != length(object@amount))
}


.check_ingredient_name <- function(object) {
    # check all ingredient names are lowercase
    return(any(grepl("[[:upper:]]", object@name)))
}

.check_ingredient_units <- function(object, valid_units) {
    check_units <- FALSE

    if (length(object@name) != length(object@units)) {
        check_units <- TRUE
    }

    if (!all(object@units %in% valid_units)) {
        check_units <- TRUE
    }

    return(check_units)
}



setValidity("Ingredients", validIngredients)
