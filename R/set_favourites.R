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
                           indexes,
                           method = c("index", "manual")) {
  recipebook <- .set_favourites(recipebook, indexes, method, TRUE)

  return(recipebook)
}

#' @rdname set_favourites
#' @export
rm_favourites <- function(recipebook,
                          indexes,
                          method = c("index", "manual")) {
  recipebook <- .set_favourites(recipebook, indexes, method, FALSE)

  return(recipebook)
}

#' @keywords internal
#' @noRd
.set_favourites <- function(recipebook,
                            indexes,
                            method = c("index", "manual"),
                            value) {
  .check_object(recipebook, "RecipeBook")
  method <- match.arg(method)

  favourite_setter_func <- .dispatch_favourite_setter(method)
  indexes <- favourite_setter_func(recipebook, indexes, value)

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
.set_favourites_manual <- function(recipebook, indexes, value, con = stdin()) {
  fav_options <- recipes(recipebook) %>%
    dplyr::filter(fav != value)

  if (nrow(fav_options) == 0) {
    stop("No recipes found with favourite as ", as.character(value))
  }

  .select_index_message(
    fav_options[["names"]],
    "recipes",
    ifelse(value, "favourite", "unfavourite"),
    "recipebook"
  )

  indexes <- readLines(con = con, n = 1L)
  indexes <- .check_tidy_index_input(indexes, seq_along(fav_options[["names"]]))

  return(indexes)
}

#' @keywords internal
#' @noRd
.set_favourites_index <- function(recipebook, indexes, value) {
  if (is.null(indexes)) {
    stop("'index' method requires non-NULL indexes")
  } else if (!is.numeric(indexes)) {
    stop("indexes must be numeric")
  }
  indexes <- as.integer(indexes)
  return(indexes)
}
