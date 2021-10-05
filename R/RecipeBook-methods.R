#' @include RecipeBook-class.R
NULL

##### show #####

#' @keywords internal
#' @noRd
.show_RecipeBook <- function(object) {
    print(object@recipes)
}

#' @rdname RecipeBook-class
#' @section Displaying:
#'
#' `show(recipebook)` displays the `recipes` slot as a `tibble::tibble()`.
#'
#' @param object RecipeBook object to display.
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
recipes <- function(x) {
    x@recipes
}

#' @rdname RecipeBook-class
#' @section Getters:
#'
#' `meal_plan(recipebook)` obtains the `meal_plan` as a `tibble::tibble()`.
meal_plan <- function(x) {
    if (nrow(x@meal_plan) == 0) {
        message("No meal plan found, have you run create_meal_plan()?")
        return(invisible())
    }

    x@meal_plan
}

#' @rdname RecipeBook-class
#' @section Getters:
#'
#' `favourites(recipebook)` obtains the `recipes` marked as a favourite.
favourites <- function(x) {
    if (sum(x@recipes[["fav"]]) == 0) {
        message("No favourite recipes set, have you run `favorites<-`?")
        return(invisible())
    }
    x@recipes[x@recipes[["fav"]], ]
}

##### setters #####

#' @rdname RecipeBook-class
#' @section Setters:
#'
#' `favourites(recipebook) <- fav_indexes` set your favourite `recipes`.
`favourites<-` <- function(x, value) {
    if (is.logical(value)) {
        x@recipes[["fav"]] <- value
    } else if (is.numeric(value)) {
        value <- as.integer(value)
        x@recipes[["fav"]][value] <- TRUE
    } else {
        stop(
            "value must be a logical or integer ",
            "(specifying the indexes of the recipes to favourite)"
        )
    }
}
