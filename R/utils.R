#' Obtain the weekdays of interest
#'
#' @param which_days `numeric()` or `character()`, default `NULL`. If numeric,
#'   must be an index between 1:7. If character, must be one of  `c("Mon",
#'   "Tues", "Wed", "Thurs", "Fri", "Sat", "Sun")`. If NULL, will return all
#'   week days.
#'
#' @return `character()` with selected weekdays
#' @export
#'
#' @examples
#'
#' weekdays()
#' weekdays(1:3)
#' weekdays(c("Mon", "Tues", "Wed"))
weekdays <- function(which_days = NULL) {
  valid_days <- c("Mon", "Tues", "Wed", "Thurs", "Fri", "Sat", "Sun")

  if (is.numeric(which_days)) {
    which_days <- unique(as.integer(which_days))

    if (any(!which_days %in% seq_along(valid_days))) {
      stop("When an numeric, which_days must be one of 1:7")
    }

    chosen_days <- valid_days[which_days]
  } else if (is.character(which_days)) {
    if (!all(which_days %in% valid_days)) {
      stop(
        "When an character, which_days must be one of: ",
        stringr::str_c(valid_days, collapse = ", ")
      )
    }

    chosen_days <- valid_days[valid_days %in% which_days]
  } else if (is.null(which_days)) {
    chosen_days <- valid_days
  }

  return(chosen_days)
}

#' @keywords internal
#' @noRd
.check_object <- function(object, class_name) {
  if (!is(object, class_name)) {
    stop(
      "object is not an instance of ",
      stringr::str_c(class_name, "-class")
    )
  }
  validObject(object)
}

#' @keywords internal
#' @noRd
.select_index_message <- function(object,
                                  what,
                                  to,
                                  in_from) {
  message(
    stringr::str_c(
      seq_along(object), " - ",
      object, "\n"
    ),
    "\nFrom the above, ",
    "please select the ", what, " you would like to ", to,
    " ", ifelse(to %in% c("include", "favourite"), "in", "from"),
    " your ", in_from, ".",
    "\nEnter the selected indexes, ",
    "separated with a ',' - for example '1,2,3' to ",
    "include the first three ingredients."
  )

  return(invisible())
}

#' @keywords internal
#' @noRd
.check_tidy_index_input <- function(input_indexes, valid_indexes) {
  if (length(input_indexes) == 1 && input_indexes == "") {
    stop("No indexes entered")
  }

  input_indexes <- input_indexes %>%
    stringr::str_split(",") %>%
    unlist() %>%
    stringr::str_trim() %>%
    as.integer()

  if (any(is.na(input_indexes))) {
    stop("Entered indexes must be entered as integers")
  }

  invalid_indexes <- input_indexes[!(input_indexes %in% valid_indexes)]

  if (length(invalid_indexes) > 0) {
    stop(
      "Invalid indexes found in input: ",
      stringr::str_c(invalid_indexes, collapse = ", ")
    )
  }

  return(input_indexes)
}
