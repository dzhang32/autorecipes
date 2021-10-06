##### favourites getter #####

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

##### favourites setter #####

setGeneric(
    "favourites<-",
    function(object, value, ...) standardGeneric("favourites<-")
)

#' @keywords internal
#' @noRd
.set_favourites_numeric <- function(object, value) {
    value <- as.integer(value)
    object@recipes[["fav"]][value] <- TRUE
    validObject(object)
    object
}

#' @rdname RecipeBook-class
#' @section Setters:
#'
#' `favourites(recipebook) <- fav_indexes` set favourite `recipes` by index.
setMethod("favourites<-",
    signature = c(object = "RecipeBook", value = "numeric"),
    .set_favourites_numeric
)

#' @keywords internal
#' @noRd
.set_favourites_logical <- function(object, value) {
    object@recipes[["fav"]] <- value
    validObject(object)
    object
}

#' @rdname RecipeBook-class
#' @section Setters:
#'
#' `favourites(recipebook) <- fav_indexes` set favourite `recipes`.
setMethod("favourites<-",
    signature = c(object = "RecipeBook", value = "logical"),
    .set_favourites_logical
)

#' @keywords internal
#' @noRd
.set_favourites_manual <- function(object, value, con = stdin()) {
    if (value != "manual") {
        stop("If favourites<- is set by character, value must be 'manual'")
    }

    message(
        stringr::str_c(
            seq_along(names(object)), " - ",
            names(object), "\n"
        ),
        "\nPlease select your favourites recipes from the above.",
        "\nEnter the indexes, separated with a ',' - for example '1,2,3' to ",
        "favourite the first three recipes."
    )

    fav_indexes <- readLines(con = con, n = 1L)

    fav_indexes <- fav_indexes %>%
        stringr::str_split(",") %>%
        unlist() %>%
        stringr::str_trim() %>%
        as.integer()

    object@recipes[["fav"]][fav_indexes] <- TRUE
    validObject(object)
    object
}

#' @rdname RecipeBook-class
#' @section Setters:
#'
#' `favourites(recipebook) <- fav_indexes` set favourite `recipes`.
setMethod("favourites<-",
    signature = c(object = "RecipeBook", value = "character"),
    .set_favourites_manual
)
