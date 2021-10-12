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
#' @export
setMethod("names", "RecipeBook", function(x) x@recipes[["names"]])

#' @rdname RecipeBook-class
#' @section Getters:
#'
#' `length(recipebook)` obtains the number of recipes.
#' @export
setMethod("length", "RecipeBook", function(x) nrow(x@recipes))

#' @rdname RecipeBook-class
#' @section Getters:
#'
#' `recipes(recipebook)` obtains the `recipes` as a `tibble::tibble()`.
#' @export
recipes <- function(object) {
    return(object@recipes)
}

#' @rdname RecipeBook-class
#' @section Getters:
#'
#' `meal_plan(recipebook)` obtains the `meal_plan` as a `tibble::tibble()`.
#' @export
meal_plan <- function(object) {
    if (nrow(object@meal_plan) == 0) {
        stop("No meal plan found, have you run create_meal_plan()?")
    }

    object@meal_plan %>% dplyr::left_join(
        object@recipes,
        by = c("recipe_index" = "index")
    )
}

##### setters #####

#' @rdname RecipeBook-class
#' @section Setters:
#'
#' `meal_plan(recipebook) <- meal_plan` stores the meal plan inside a
#' `RecipeBook-class` object.
#' @export
`meal_plan<-` <- function(object, value) {
    if (nrow(object@meal_plan) != 0) {
        message("Overwriting existing meal plan")
    }

    object@meal_plan <- value
    validObject(object)
    object
}

#' @keywords internal
#' @noRd
`last_eaten<-` <- function(object, value) {
    object@recipes[["last_eaten"]][value] <- lubridate::today()
    validObject(object)
    object
}
