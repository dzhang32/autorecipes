
# Load packages -----------------------------------------------------------

library(autorecipes)

# Main --------------------------------------------------------------------

RecipeBook_example <- RecipeBook(
  names = recipes_example[["title"]],
  ingredients = recipes_example[["ingredients"]]
)

# Save data ---------------------------------------------------------------

usethis::use_data(RecipeBook_example, overwrite = TRUE)
