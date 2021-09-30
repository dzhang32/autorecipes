#' @include allGenerics.R RecipeBook-class.R
NULL

##### show #####

#' @keywords internal
#' @noRd
.show_RecipeBook <- function(object) {
    print("TBA")
}

#' @importMethodsFrom methods show
setMethod("show", "RecipeBook", .show_RecipeBook)

##### getters #####

#' @describeIn RecipeBook-class obtain names of ingredients
setMethod("names", "RecipeBook", function(x) x@names)
#' @describeIn RecipeBook-class obtain ingredients for each recipe
setMethod("ingredients", "RecipeBook", function(x) x@ingredients)
