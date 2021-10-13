#' @include Ingredients-class.R
NULL

##### show #####

#' @keywords internal
#' @noRd
.show_ingredients <- function(object) {
  units_no_NA <- units(object)
  units_no_NA[is.na(units_no_NA)] <- ""

  cat(stringr::str_c(
    amounts(object), " ",
    units_no_NA,
    ifelse(units_no_NA == "", "", " "),
    names(object), "\n"
  ), sep = "")
}

#' @rdname Ingredients-class
#' @section Displaying:
#'
#' `show(ingredients)` prints each ingredient.
#' @importMethodsFrom methods show
setMethod("show", "Ingredients", .show_ingredients)

##### getters #####

#' @rdname Ingredients-class
#' @section Getters:
#'
#' `names(ingredients)` obtain the names of each ingredient.
#' @export
setMethod("names", "Ingredients", function(x) stringr::str_to_title(x@names))

#' @rdname Ingredients-class
#' @section Getters:
#'
#' `amounts(ingredients)` obtain the amounts of each ingredient.
#' @export
amounts <- function(x) {
  x@amounts
}

#' @rdname Ingredients-class
#' @section Getters:
#'
#' `units(ingredients)` obtain the units of each ingredient.
#' @export
units <- function(x) {
  x@units
}

##### coersion #####

#' @keywords internal
#' @noRd
.as_data_frame_ingredients <- function(x) {
  data.frame(
    names = x@names,
    amounts = x@amounts,
    units = x@units
  )
}

#' @rdname Ingredients-class
#' @section Getters:
#'
#' `as.data.frame(ingredients)` converts ingredients to a `data.frame`.
setMethod("as.data.frame", "Ingredients", .as_data_frame_ingredients)
