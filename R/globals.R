# bypass R CMD Check notes, related to tidyverse non-standard evaluation
# https://www.r-bloggers.com/2019/08/no-visible-binding-for-global-variable/
utils::globalVariables(c(
    ".",
    "n",
    "fav",
    "recipe_index"
))
