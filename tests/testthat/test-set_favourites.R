##### set_favourites #####

test_that("set_favourites output looks correct", {
    test_favourites <- set_favourites(recipebook_example, fav_indexes = 1:3)
    expect_true(identical(which(test_favourites[["fav"]]), 1:3))
})

##### .set_favourites_manual #####

test_that(".set_favourites_manual output looks correct", {
    # setup testing to mirror manual input
    # based on https://debruine.github.io/posts/interactive-test/
    input <- file()
    lines <- "1,2,3"
    write(lines, input)
    test_manual <- .set_favourites_manual(recipebook_example[1:5, ], con = input)
    close(input)

    expect_true(identical(test_manual, 1:3))
})
##### .check_fav_indexes #####

test_that(".check_fav_indexes catches user input errors", {
    expect_warning(
        .check_fav_indexes(recipebook_example, c(1, 2, 3)),
        "fav_indexes must be an integer, coercing"
    )
    expect_error(
        .check_fav_indexes(recipebook_example, c(1L, 40L, 2L, 50L)),
        "Entered indexes do not match recipe indexes; 40, 50"
    )
})
