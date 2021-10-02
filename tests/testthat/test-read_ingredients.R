test_ingred <- recipes_example[["ingredients"]]

##### read_ingredients #####

test_that("read_ingredients works correctly", {
    expect_true(identical(
        read_ingredients(test_ingred[1:2], delim = ";"),
        .parse_ingredient_auto(test_ingred[1:2], delim = ";")
    ))
})

test_that("read_ingredients catches input errors", {
    expect_error(
        read_ingredients(test_ingred[1:2], delim = ";", method = "x"),
        "'arg' should be one of"
    )
    expect_error(
        read_ingredients(test_ingred[1:2]),
        "Currently, delim must be entered"
    )
})


##### .parse_ingredient_auto #####

test_that(".parse_ingredient_auto output looks broadly correctly", {
    test_parse_ingred <- .parse_ingredient_auto(test_ingred, delim = ";")

    expect_true(identical(length(test_parse_ingred), length(test_ingred)))
    expect_true(all(
        lapply(test_parse_ingred, function(x) methods::is(x, "Ingredients")) %>%
            unlist()
    ))
})

test_that(".parse_ingredient_auto output specifics", {
    test_parse_ingred <- .parse_ingredient_auto(
        "1 red onion;3 garlic cloves;500g salad potatoes;100ml hot water",
        delim = ";"
    )

    expect_true(length(test_parse_ingred) == 1)
    expect_true(identical(
        test_parse_ingred[[1]]@names,
        c("red onion", "garlic cloves", "salad potatoes", "hot water")
    ))
    expect_true(identical(
        test_parse_ingred[[1]]@amounts,
        c(1, 3, 500, 100)
    ))
    expect_true(identical(
        test_parse_ingred[[1]]@units,
        c(NA_character_, NA_character_, "g", "ml")
    ))
})
