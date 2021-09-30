setClass("Recipe",
         slots = c(
           name = "character",
           ingredients = "character"
         ),
         prototype = list(
           name = NA_character_,
           ingredients = NA_character_
         )
)
