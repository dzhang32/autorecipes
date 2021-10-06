#' @include RecipeBook-class.R
NULL

##### show #####

#' @keywords internal
#' @noRd
.show_RecipeBook <- function(object) {
    print(object@recipes)
    return(invisible())
}

#' @rdname RecipeBook-class
#' @section Displaying:
#'
#' `show(recipebook)` prints the `recipes` as a `tibble::tibble()`.
#' @importMethodsFrom methods show
setMethod("show", "RecipeBook", .show_RecipeBook)

##### getters #####

#' @rdname RecipeBook-class
#' @section Getters:
#'
#' `names(recipebook)` obtains the names of the recipes.
setMethod("names", "RecipeBook", function(x) x@recipes[["names"]])

#' @rdname RecipeBook-class
#' @section Getters:
#'
#' `recipes(recipebook)` obtains the `recipes` as a `tibble::tibble()`.
recipes <- function(object) {
    return(object@recipes)
}

#' @rdname RecipeBook-class
#' @section Getters:
#'
#' `meal_plan(recipebook)` obtains the `meal_plan` as a `tibble::tibble()`.
meal_plan <- function(object) {
    if (nrow(object@meal_plan) == 0) {
        message("No meal plan found, have you run create_meal_plan()?")
        return(invisible())
    }

    return(object@meal_plan)
}

##### setters #####

#' for internal use in `create_meal_plan()`
#' @keywords internal
#' @noRd
`meal_plan<-` <- function(object, value) {
    if (nrow(object@meal_plan) != 0) {
        message("overwriting existing meal plan")
    }

    object@meal_plan <- value
    validObject(object)
    object
}
