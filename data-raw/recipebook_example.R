
# Load packages -----------------------------------------------------------

library(autorecipes)

# Main --------------------------------------------------------------------

recipebook_example <-
    create_recipebook(
        names = recipes_example[["title"]],
        ingredients = read_ingredients(recipes_example[["ingredients"]], delim = ";")
    )

# Save data ---------------------------------------------------------------

usethis::use_data(recipebook_example, overwrite = TRUE)
