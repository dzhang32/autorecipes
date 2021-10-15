##### set_favourites #####

#' Add or remove favourite recipes
#'
#' @param recipebook object of `RecipeBook-class`.
#' @param method `character()` one of 'manual' or 'index'. If 'manual', will
#'   allow users to interactively pick the recipes to add/remove as favourites.
#'   If 'index' will modify the recipes corresponding to `indexes`.
#' @param indexes `numeric()` the indexes of the recipes to change, required if
#'   method is set to 'index'.
#'
#' @name set_favourites
NULL

#' @rdname set_favourites
#' @export
add_favourites <- function(recipebook,
                           method = c("manual", "index"),
                           indexes = NULL) {
  recipebook <- .set_favourites(recipebook, method, indexes, TRUE)

  return(recipebook)
}

#' @rdname set_favourites
#' @export
rm_favourites <- function(recipebook,
                          method = c("manual", "index"),
                          indexes = NULL) {
  recipebook <- .set_favourites(recipebook, method, indexes, FALSE)

  return(recipebook)
}

#' @keywords internal
#' @noRd
.set_favourites <- function(recipebook,
                            method = c("manual", "index"),
                            indexes = NULL,
                            value = TRUE) {
  .check_object(recipebook, "RecipeBook")
  method <- match.arg(method)

  favourite_setter_func <- .dispatch_favourite_setter(method)
  indexes <- favourite_setter_func(recipebook, indexes)

  recipebook@recipes[["fav"]][indexes] <- value

  return(recipebook)
}

#' @keywords internal
#' @noRd
.dispatch_favourite_setter <- function(method) {
  switch(method,
    "manual" = .set_favourites_manual,
    "index" = .set_favourites_index
  )
}

#' @keywords internal
#' @noRd
.set_favourites_manual <- function(recipebook, indexes, con = stdin()) {
  message(
    stringr::str_c(
      seq_along(names(recipebook)), " - ",
      names(recipebook), "\n"
    ),
    "\nPlease select your favourites recipes from the above.",
    "\nEnter the indexes, separated with a ',' - for example '1,2,3' to ",
    "favourite the first three recipes."
  )

  indexes <- readLines(con = con, n = 1L)

  indexes <- indexes %>%
    stringr::str_split(",") %>%
    unlist() %>%
    stringr::str_trim() %>%
    as.integer()

  return(indexes)
}

#' @keywords internal
#' @noRd
.set_favourites_index <- function(recipebook, indexes) {
  if (is.null(indexes)) {
    stop("'index' method requires non-NULL indexes")
  } else if (!is.numeric(indexes)) {
    stop("indexes must be numeric")
  }
  indexes <- as.integer(indexes)
  return(indexes)
}
