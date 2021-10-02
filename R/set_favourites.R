#' Pick your favourite recipes
#'
#' @inheritParams create_meal_plan
#' @param fav_indexes `integer()` set of indexes that correspond to the recipes
#'   that you want to favourite.
#'
#' @return `tibble::tibble()` containing recipebook with selected favourites.
#' @export
#'
#' @examples
#' set_favourites(recipebook_example, fav_indexes = c(1, 2, 3))
set_favourites <- function(recipebook, fav_indexes = NULL) {
    .valid_recipebook(recipebook)

    if (is.null(fav_indexes)) {
        fav_indexes <- .set_favourites_manual(recipebook)
    }

    .check_fav_indexes(recipebook, fav_indexes)

    recipebook <- recipebook %>% dplyr::mutate(fav = FALSE)
    recipebook[["fav"]][fav_indexes] <- TRUE

    return(recipebook)
}

#' @keywords internal
#' @noRd
.set_favourites_manual <- function(recipebook, con = stdin()) {
    message(
        stringr::str_c(
            seq_along(recipebook[["names"]]), " - ",
            recipebook[["names"]], "\n"
        ),
        "\nPlease select your favourites recipes from the above.",
        "\nEnter the indexes, separated with a ',' - for example '1,2,3' to ",
        "favourite the first three recipes."
    )

    fav_indexes <- readLines(con = con, n = 1L)

    fav_indexes <- fav_indexes %>%
        stringr::str_split(",") %>%
        unlist() %>%
        as.integer()

    return(fav_indexes)
}

#' @keywords internal
#' @noRd
.check_fav_indexes <- function(recipebook, fav_indexes) {
    if (!is.integer(fav_indexes)) {
        warning("fav_indexes must be an integer, coercing")
        fav_indexes <- as.integer(fav_indexes)
    }

    missing <- fav_indexes[!(fav_indexes %in% seq_along(recipebook[["names"]]))]

    if (length(missing) != 0) {
        stop(
            "Entered indexes do not match recipe indexes; ",
            stringr::str_c(missing, collapse = ", ")
        )
    }
}
