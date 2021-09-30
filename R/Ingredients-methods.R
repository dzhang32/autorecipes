#' @include allGenerics.R Ingredients-class.R
NULL

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

#' @describeIn Ingredients Print set of ingredients
#' @export
setMethod("show", "Ingredients", .show_Ingredients)

##### getters #####

#' @describeIn Ingredients Getter for the names of ingredients
#' @export
setMethod("names", "Ingredients", function(x) stringr::str_to_title(x@names))

#' @describeIn Ingredients Getter for the amount of ingredients
#' @export
setMethod("amounts", "Ingredients", function(x) x@amounts)

#' @describeIn Ingredients Getter for the units of the amount
#' @export
setMethod("units", "Ingredients", function(x) x@units)
