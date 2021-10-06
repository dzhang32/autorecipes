##### favourites #####

test_that("favourites getter works when favourites empty", {
  expect_message(
    favourites(RecipeBook_example),
    "No favourite recipes set, have you run `favorites<-`?"
  )
})

##### favourites<- #####

test_that("favourites getter works when favourites empty", {
  expect_message(
    favourites(RecipeBook_example),
    "No favourite recipes set, have you run `favorites<-`?"
  )
})

