##### favourites #####

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

##### favourites<- #####

#' @rdname RecipeBook-class
#'
#' @param ... additional arguments passed on the `favourite<-` methods.
#' @param value either a `integer()` specifying the index of the rows to
#'   favourite, an `logical()` to set as the "fav" column, or the `character()`
#'   "manual" to specify setting the favourites manually.
#'
#' @export
setGeneric(
    "favourites<-",
    function(object, ..., value) standardGeneric("favourites<-")
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
#' `favourites(recipebook) <- 1` set favourite `recipes` by index.
#' @export
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
#' `favourites(recipebook) <- TRUE` set favourite `recipes` by logical.
#' @export
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
#' @inheritParams base::readLines
#'
#' @section Setters:
#'
#' `favourites(recipebook) <- "manual"` set favourite `recipes` manually.
#' @export
setMethod("favourites<-",
    signature = c(object = "RecipeBook", value = "character"),
    .set_favourites_manual
)
