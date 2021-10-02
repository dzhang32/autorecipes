#' @include Ingredients-class.R
NULL

##### show #####

#' @keywords internal
#' @noRd
.show_Ingredients <- function(object) {
    units_no_NA <- units(object)
    units_no_NA[is.na(units_no_NA)] <- ""

    cat(stringr::str_c(
        amounts(object), " ",
        units_no_NA,
        ifelse(units_no_NA == "", "", " "),
        names(object), "\n"
    ), sep = "")
}

#' @importMethodsFrom methods show
setMethod("show", "Ingredients", .show_Ingredients)

##### getters #####

#' @describeIn Ingredients-class obtain names of ingredients
setMethod("names", "Ingredients", function(x) stringr::str_to_title(x@names))

#' @describeIn Ingredients-class obtain amounts of ingredients
amounts <- function(x) {
    x@amounts
}

#' @describeIn Ingredients-class obtain units of amounts for each ingredient
units <- function(x) {
    x@units
}
