#' @include RecipeBook-class.R
NULL

##### show #####

#' @keywords internal
#' @noRd
.show_recipebook <- function(object) {
  print(object@recipes)
  return(invisible())
}

#' @rdname RecipeBook-class
#' @section Displaying:
#'
#'   `show(recipebook)` prints the `recipes` as a `tibble::tibble()`.
#' @importMethodsFrom methods show
setMethod("show", "RecipeBook", .show_recipebook)

##### getters #####

#' @rdname RecipeBook-class
#' @section Getters:
#'
#'   `names(recipebook)` obtains the names of the recipes.
#' @export
setMethod("names", "RecipeBook", function(x) x@recipes[["names"]])

#' @rdname RecipeBook-class
#' @section Getters:
#'
#'   `length(recipebook)` obtains the number of recipes.
#' @export
setMethod("length", "RecipeBook", function(x) nrow(x@recipes))

#' @rdname RecipeBook-class
#' @section Getters:
#'
#'   `recipes(recipebook)` obtains the `recipes` as a `tibble::tibble()`.
#' @export
recipes <- function(object) {
  return(object@recipes)
}

#' @rdname RecipeBook-class
#' @section Getters:
#'
#'   `meal_plan(recipebook)` obtains the `meal_plan` as a `tibble::tibble()`.
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

#' @rdname RecipeBook-class
#' @section Getters:
#'
#'   `shopping_list(recipebook)` obtains the `shopping_list` as a
#'   `tibble::tibble()`.
#' @export
shopping_list <- function(object) {
  if (nrow(object@shopping_list) == 0) {
    stop("No shopping_list found, have you run create_shopping_list()?")
  }

  object@shopping_list
}

#' @rdname RecipeBook-class
#' @section Getters:
#'
#' `favourites(recipebook)` obtains the `recipes` marked as favourites.
#' @export
favourites <- function(x) {
  if (sum(x@recipes[["fav"]]) == 0) {
    stop("No favourite recipes set, have you run `favorites<-`?")
  }
  x@recipes[x@recipes[["fav"]], ]
}

##### setters #####

#' @rdname RecipeBook-class
#' @section Setters:
#'
#'   `recipes(object) <- value` can be used to set the `recipes`.
#' @export
`recipes<-` <- function(object, value) { # nolint
  object@recipes <- value
  validObject(object)
  object
}


#' @keywords internal
#' @noRd
`meal_plan<-` <- function(object, value) { # nolint
  if (nrow(object@meal_plan) != 0) {
    message("Overwriting existing meal plan")
  }

  object@meal_plan <- value
  validObject(object)
  object
}

#' @keywords internal
#' @noRd
`shopping_list<-` <- function(object, value) { # nolint
  if (nrow(object@shopping_list) != 0) {
    message("Overwriting existing shopping list")
  }

  object@shopping_list <- value
  validObject(object)
  object
}

#' @keywords internal
#' @noRd
`last_eaten<-` <- function(object, value) { # nolint
  object@recipes[["last_eaten"]][value] <- lubridate::today()
  validObject(object)
  object
}
