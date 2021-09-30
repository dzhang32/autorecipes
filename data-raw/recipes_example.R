
# Load packages -----------------------------------------------------------

library(tidyverse)

# Load data ---------------------------------------------------------------

recipes_example <- readr::read_lines(
    here::here("data-raw", "recipes_example.txt")
)

# Functions ---------------------------------------------------------------

tidy_instructions <- function(instruction) {
    ins_order <- instruction %>%
        stringr::str_extract_all("[1-9][0-9]?\\. ") %>%
        unlist() %>%
        as.integer()

    # check we only find 1 of each [1-9]\\. and all are present
    stopifnot(all.equal(ins_order %>% sort(), seq_along(ins_order)))

    ins_text <- instruction %>%
        stringr::str_split("[1-9][0-9]?\\. ") %>%
        unlist() %>%
        stringr::str_replace_all(";", " ") %>%
        stringr::str_trim() %>%
        stringr::str_replace_all("  *", " ")

    ins_text <- ins_text[ins_text != ""]

    stopifnot(identical(length(ins_text), length(ins_order)))

    ins_text <- ins_text %>%
        stringr::str_c(ins_order, ": ", .)

    ins_text_tidy <- ins_text[order(ins_order)] %>%
        stringr::str_c(collapse = ";")

    return(ins_text_tidy)
}

# Main --------------------------------------------------------------------

# merge the ingredients and instruction blocks
recipes_example[recipes_example == ""] <- "&&"

recipes_example <- recipes_example %>%
    stringr::str_c(collapse = ";") %>%
    stringr::str_split("&&") %>%
    unlist()

# result 3 as 3 elements (title, instruction, ingredients) per recipe
stopifnot(length(recipes_example) %% 3 == 0)

recipes_tidy <- dplyr::tibble(
    title = recipes_example[seq_along(recipes_example) %% 3 == 1],
    ingredients = recipes_example[seq_along(recipes_example) %% 3 == 0],
    instructions = recipes_example[seq_along(recipes_example) %% 3 == 2],
)

# general tidying
recipes_tidy <- recipes_tidy %>% dplyr::mutate(
    title = title %>%
        stringr::str_remove_all("^;|;$"),
    title_tidy = title %>%
        stringr::str_to_lower() %>%
        stringr::str_replace_all(" ", "_"),
    ingredients = ingredients %>%
        stringr::str_remove_all("^;|;$") %>%
        stringr::str_remove("^Ingredients;"),
    instructions = instructions %>%
        stringr::str_remove_all("^;|;$"),
    instructions = ifelse(stringr::str_detect(instructions, "^[1-9]\\."),
        instructions,
        stringr::str_c("1. ", instructions)
    )
)

recipes_tidy[["instructions"]] <- recipes_tidy[["instructions"]] %>%
    lapply(tidy_instructions) %>%
    unlist()

recipes_example <- recipes_tidy %>%
    dplyr::select(title, title_tidy, ingredients)

# Save data ---------------------------------------------------------------

usethis::use_data(recipes_example, overwrite = TRUE)
