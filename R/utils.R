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
