#' @title S4 class containing names, amounts and units of recipe ingredients
#'
#' @description
#'
#' An `Ingredients-class` object contains information regarding the name,
#' quantity and units for each ingredient. Although `autorecipes` provides an
#' `Ingredients()` constructor, it is unlikely that users will need to call this
#' to create `Ingredient-class` instances directly. Instead, when creating a
#' `RecipeBook-class` instance, helper functions are included to read
#' ingredients into the appropriate format such as the `RecipeBook()`
#' constructor and `read_ingredients()`.
#' 
#' @slot names `character()` containing the names of each ingredient.
#' @slot amounts `numeric()` containing the amounts of each ingredient.
#' @slot units `character()` containing the units for the amount of each
#'   ingredient.
#'
#' @param x an `Ingredients-class` object.
#' @param object an `Ingredients-class` object.
#'
#' @examples
#'
#' Ingredients("salt", 1, "tsp")
#'
#' # one instance can store multiple ingredients
#' Ingredients(c("salt", "tomatoes"), c(1, 400), c("tsp", "g"))
#'
#' # amounts and units are optional
#' Ingredients("Chicken")
#' @name Ingredients-class
#' @aliases Ingredients
#' @exportClass Ingredients
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
#'
#' @section Constructor:
#'
#'   `Ingredients(names, amounts, units)` creates an object of
#'   `Ingredients-class`.
#'
#' @param names `character()` containing the names of each ingredient.
#' @param amounts `numeric()` containing the amounts of each ingredient.
#' @param units `character()` containing the units for the amount of each
#'   ingredient.
#'
#' @export
Ingredients <- function(names, # nolint
                        amounts = 1,
                        units = rep(NA_character_, length(names))) {
  names <- stringr::str_to_lower(names)

  # convert fractions to decimals, also character to numeric
  amounts <- .convert_fractions(amounts)

  # if no amount value, assume we want 1
  amounts <- ifelse(is.na(amounts), 1, amounts)

  new("Ingredients", names = names, amounts = amounts, units = units)
}

#' @keywords internal
#' @noRd
.convert_fractions <- function(x) {
  purrr::map_dbl(x, function(y) eval(parse(text = y))) %>%
    unname()
}

##### validator #####

#' @keywords internal
#' @noRd
.valid_ingredients <- function(object) {
  valid_units <- .all_valid_units()

  if (.check_names_amounts_length(object)) {
    "object@names and object@amounts must have equal lengths"
  } else if (.check_names_upper_case(object)) {
    "All object@names values must be lower case"
  } else if (.check_valid_units(object, valid_units)) {
    paste(
      "object@units must be the same length as",
      "object@names and object@amounts and be one of;",
      paste(valid_units, collapse = ", ")
    )
  } else {
    TRUE
  }
}

setValidity("Ingredients", .valid_ingredients)

#' @keywords internal
#' @noRd
.check_names_amounts_length <- function(object) {
  return(length(object@names) != length(object@amounts))
}

#' @keywords internal
#' @noRd
.check_names_upper_case <- function(object) {
  # check all ingredient names are lowercase
  return(any(stringr::str_detect(object@names, "[[:upper:]]")))
}

#' @keywords internal
#' @noRd
.check_valid_units <- function(object, valid_units) {
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
  stopifnot(length(no_na) == 1 && length(regex) == 1)
  if (regex && !no_na) stop("when regex is TRUE, no_na should be TRUE")

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
