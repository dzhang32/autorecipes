
# Load packages -----------------------------------------------------------

library(autorecipes)

# Main --------------------------------------------------------------------

recipebook_example <- RecipeBook(
  names = recipes_example[["title"]],
  ingredients = recipes_example[["ingredients"]]
)

# Save data ---------------------------------------------------------------

usethis::use_data(recipebook_example, overwrite = TRUE)
