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
  validObject(x)
}
