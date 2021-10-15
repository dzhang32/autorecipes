test_recipebook <- recipebook_example

##### add_recipes #####

test_that("add_recipes output looks correct", {
  test_recipebook_base <- test_recipebook
  test_recipes_to_add <- recipes(test_recipebook_base) %>%
    .[11:length(test_recipebook_base), ]
  recipes(test_recipebook_base) <- recipes(test_recipebook_base)[1:10, ]
  expect_true(identical(
    add_recipes(
      test_recipebook_base,
      test_recipes_to_add[["names"]],
      test_recipes_to_add[["ingredients"]]
    ),
    test_recipebook
  ))
})
