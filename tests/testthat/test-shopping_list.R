##### create_shopping_list #####

test_that("create_shopping_list output looks correct", {
    test_shopping_list <- create_shopping_list(recipebook_example[1:5, ])
    expect_false(any(duplicated(test_shopping_list[["names"]])))
    expect_true(ncol(test_shopping_list) == 2)
})

##### .extract_ingredients #####

test_that(".extract_ingredients output looks correct", {
    test_ingredients_df <- .extract_ingredients(recipebook_example[1:5, ])
    expect_true(is.data.frame(test_ingredients_df))
})

##### .collapse_ingredients #####

test_that(".collapse_ingredients output looks correct", {
    test_ingredients_collapsed <- .collapse_ingredients(dplyr::tibble(
        names = stringr::str_c("ingred_", c(1, 1, 2, 3))
    ))
    expect_true(is.data.frame(test_ingredients_collapsed))
    expect_true(identical(
        test_ingredients_collapsed[["names"]],
        c("ingred_1", "ingred_2", "ingred_3")
    ))
    expect_true(identical(
        test_ingredients_collapsed[["n"]],
        c(2L, 1L, 1L)
    ))
})
