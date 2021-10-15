#' @title S4 class containing recipes, their ingredients and meal plans
#'
#' @description
#'
#' A book of recipes from which meal plans and shopping lists can be generated.
#'
#' @slot recipes `tibble::tibble()` storing the names, ingredients and
#'   favourites status of recipes.
#' @slot meal_plan `tibble::tibble()` storing a meal plan, created by `recipes`
#'   using `create_meal_plan()`.
#'
#' @param x a `RecipeBook-class` object.
#' @param object a `RecipeBook-class` object.
#'
#' @name RecipeBook-class
#' @aliases RecipeBook
#' @exportClass RecipeBook
#' @importClassesFrom tibble tbl_df
setClass("RecipeBook",
  slots = c(
    recipes = "tbl_df",
    meal_plan = "tbl_df",
    shopping_list = "tbl_df"
  )
)

##### initialize #####

#' @keywords internal
#' @noRd
initialize_recipebook <- function(.Object, names, ingredients, ...) {
  .Object <- callNextMethod(.Object, ...)
  recipes <- dplyr::tibble(
    index = seq_along(names),
    names = names,
    ingredients = ingredients,
    fav = FALSE,
    last_eaten = lubridate::as_date(vector("character", length(names)))
  )

  .Object@recipes <- recipes
  .Object@meal_plan <- dplyr::tibble()
  .Object@shopping_list <- dplyr::tibble()
  validObject(.Object)

  return(.Object)
}

setMethod("initialize", "RecipeBook", initialize_recipebook)

##### constructor #####

#' @rdname RecipeBook-class
#' @section Constructor:
#'
#' `RecipeBook(names, ingredients)` creates an object of `RecipeBook-class`.
#'
#' @param names `character()` containing the name of of recipes.
#' @param ingredients `character()` or `list()` of `Ingredient-class` objects.
#'   If a `character()`, will be read in using `read_ingredients()`.
#'
#' @export
RecipeBook <- function(names, ingredients) { # nolint
  names <- as.character(names)

  # allow users to put in a character for ingredients
  if (is.character(ingredients)) {
    ingredients <- read_ingredients(ingredients)
  }

  new("RecipeBook", names = names, ingredients = ingredients)
}

#' Read in a list of ingredients from a character vector
#'
#' @param ingredients `character()` containing ingredient info separated by
#'   `delim`. Each element should denote the ingredients coming from a distinct
#'   recipe.
#' @param delim `character()` that separates each ingredient.
#' @param method `character()` used to determine the method of parsing
#'   ingredients, placeholder for future implementations (e.g. with data.frame
#'   input).
#'
#' @return `list()` of elements of `Ingredients-class` objects.
#' @export
#'
#' @examples
#' read_ingredients(
#'   c(
#'     "butter;6 eggs;1 bag coriander;1 punnet mixed baby tomatoes",
#'     "1 tsp garam masala;1 bag baby spinach;1 lemon;1 chilli"
#'   ),
#'   delim = ";"
#' )
read_ingredients <- function(ingredients,
                             delim = ";",
                             method = "auto") {
  method <- match.arg(method)

  read_ingredient_func <- .dispatch_ingredient_reader(method)

  ingredients_tidy <- read_ingredient_func(ingredients, delim)

  return(ingredients_tidy)
}

#' @keywords internal
#' @noRd
.dispatch_ingredient_reader <- function(method) {
  switch(method,
    "auto" = .read_ingredients_auto
  )
}

#' @keywords internal
#' @noRd
.read_ingredients_auto <- function(ingredients, delim) {
  if (!is.null(delim)) {
    ingredients_parsed <- ingredients %>%
      stringr::str_split(delim)
  } else {
    stop("Currently, delim must be entered")
  }

  n_ingred_per_recipe <- ingredients_parsed %>%
    lapply(FUN = function(x) length(x)) %>%
    unlist()

  ingredients_tibble <-
    dplyr::tibble(
      recipe_index = rep(seq_along(ingredients_parsed), n_ingred_per_recipe),
      ingredients = unlist(ingredients_parsed)
    )

  amounts_regex <- "[1-9]/?[0-9]?[0-9]?"
  units_regex <- .all_valid_units(no_na = TRUE, regex = TRUE)

  # extract each individual component
  ingredients_tibble <- ingredients_tibble %>%
    dplyr::mutate(
      amounts = ingredients %>% stringr::str_extract(amounts_regex),
      units = ingredients %>%
        stringr::str_extract(units_regex) %>%
        stringr::str_trim(),
      names = ingredients %>%
        stringr::str_remove(amounts_regex) %>%
        stringr::str_remove(units_regex) %>%
        stringr::str_trim()
    )

  ingredients_tidy <- vector(mode = "list", length = length(ingredients_parsed))
  for (i in seq_along(ingredients_parsed)) {
    recipe_curr <- ingredients_tibble %>% dplyr::filter(recipe_index == i)

    ingredients_tidy[[i]] <- Ingredients(
      names = recipe_curr[["names"]],
      amounts = recipe_curr[["amounts"]],
      units = recipe_curr[["units"]]
    )
  }

  return(ingredients_tidy)
}

##### validator #####

#' @keywords internal
#' @noRd
.valid_recipebook <- function(object) {
  if (.check_recipes_nrow(object)) {
    "object@recipes must have > 0 rows"
  } else if (.check_recipes_colnames(object)) {
    "object@recipes must contain 'names' and 'ingredients' columns"
  } else if (.check_recipes_index(object)) {
    paste(
      "sort(object@recipes[['index']]) should be",
      "equivalent to seq_along(object@recipes[['index']])"
    )
  } else if (.check_recipes_names(object)) {
    "object@recipes[['names']] must be a character"
  } else if (.check_recipes_ingredients(object)) {
    paste(
      "object@recipes[['ingredients']] must be a list",
      "containing Ingredients-class objects"
    )
  } else if (.check_recipes_fav(object)) {
    "object@recipes[['fav']] must be logical"
  } else if (.check_recipes_last_eaten(object)) {
    "object@recipes[['last_eaten']] must of class Date"
  } else if (.check_meal_plan_colnames(object)) {
    "object@meal_plan must have the columns: 'day', 'meal', 'recipe_index'"
  } else if (.check_meal_plan_days(object)) {
    paste(
      "object@meal_plan[['day']] must all be one of:",
      stringr::str_c(weekdays(), collapse = ", ")
    )
  } else if (.check_shopping_list_colnames(object)) {
    paste(
      "object@shopping_list must have the columns: 'names', 'n'"
    )
  } else {
    TRUE
  }
}

setValidity("RecipeBook", .valid_recipebook)

#' @keywords internal
#' @noRd
.check_recipes_nrow <- function(object) {
  return(nrow(object@recipes) == 0)
}

#' @keywords internal
#' @noRd
.check_recipes_colnames <- function(object) {
  return(!all(c("names", "ingredients") %in% colnames(object@recipes)))
}

#' @keywords internal
#' @noRd
.check_recipes_index <- function(object) {
  check_recipes_index <- !identical(
    object@recipes[["index"]],
    seq_along(object@recipes[["index"]])
  )

  return(check_recipes_index)
}

#' @keywords internal
#' @noRd
.check_recipes_names <- function(object) {
  return(!is.character(object@recipes[["names"]]))
}

#' @keywords internal
#' @noRd
.check_recipes_ingredients <- function(object) {
  check_ingredients <- FALSE

  if (!is.list(object@recipes[["ingredients"]])) {
    check_ingredients <- TRUE
  } else {
    all_ingredients <- object@recipes[["ingredients"]] %>%
      lapply(
        FUN = function(x) methods::is(x, "Ingredients")
      ) %>%
      unlist() %>%
      all()

    if (!all_ingredients) {
      check_ingredients <- TRUE
    }
  }

  return(check_ingredients)
}

.check_recipes_fav <- function(object) {
  return(!is.logical(object@recipes[["fav"]]))
}

.check_recipes_last_eaten <- function(object) {
  return(!lubridate::is.Date(object@recipes[["last_eaten"]]))
}

.check_meal_plan_colnames <- function(object) {
  check_meal_plan <- FALSE
  if (nrow(object@meal_plan) > 0) {
    if (!all(
      c("day", "meal", "recipe_index") %in% colnames(object@meal_plan)
    )) {
      check_meal_plan <- TRUE
    }
  }
  return(check_meal_plan)
}

.check_meal_plan_days <- function(object) {
  valid_days <- weekdays()
  return(any(!(object@meal_plan[["day"]] %in% valid_days)))
}

.check_shopping_list_colnames <- function(object) {
  check_shopping_list <- FALSE
  if (nrow(object@shopping_list) > 0) {
    if (!all(
      c("names", "n") %in% colnames(object@shopping_list)
    )) {
      check_shopping_list <- TRUE
    }
  }
  return(check_shopping_list)
}
