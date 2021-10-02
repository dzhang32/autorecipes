#' Set of example recipes
#'
#' A data set containing 26 example recipes originating from recipe boxes that
#' I've ordered from [Riverford](https://www.riverford.co.uk). Riverford are a
#' wonderful UK company producing sustainable, delicious, locally-sourced food -
#' check them out!
#'
#' @format A `tibble::tibble()` with 26 rows and 4 columns:
#' \describe{
#'   \item{title}{`character()` recipe name.}
#'   \item{title_tidy}{`character()` recipe name in computer-friendly format.}
#'   \item{ingredients}{
#'     `character()` quantity and type of ingredient, separated by a ";".
#'     }
#'   \item{instructions}{
#'     `character()` instructions for cooking for each recipe.
#'   }
#' }
#'
#' @source generated using data-raw/recipes_example.R
"recipes_example"

#' An example recipe book
#'
#' A data set that has been created using `autorecipes::create_recipebook()`
#' containing 26 example recipes. The recipes originate from recipe boxes that
#' I've ordered from [Riverford](https://www.riverford.co.uk). Riverford are a
#' wonderful UK company producing sustainable, delicious, locally-sourced food -
#' check them out!
#'
#' @format A `tibble::tibble()` with 26 rows and 2 columns:
#' \describe{
#'   \item{names}{`character()` recipe name.}
#'   \item{ingredients}{
#'     `character()` name, quantity and type of ingredient, stored as a `list()`
#'         of `Ingredients-class`.
#'     }
#' }
#'
#' @source generated using data-raw/recipebook_example.R
"recipebook_example"
