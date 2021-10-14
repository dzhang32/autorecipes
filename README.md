
<!-- README.md is generated from README.Rmd. Please edit that file -->

# autorecipes

<!-- badges: start -->

[![GitHub
issues](https://img.shields.io/github/issues/dzhang32/autorecipes)](https://github.com/dzhang32/autorecipes/issues)
[![GitHub
pulls](https://img.shields.io/github/issues-pr/dzhang32/autorecipes)](https://github.com/dzhang32/autorecipes/pulls)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/dzhang32/autorecipes/workflows/R-CMD-check/badge.svg)](https://github.com/dzhang32/autorecipes/actions)
[![Codecov test
coverage](https://codecov.io/gh/dzhang32/autorecipes/branch/master/graph/badge.svg)](https://codecov.io/gh/dzhang32/autorecipes?branch=master)
<!-- badges: end -->

`autorecipes` automates your recipe planning by taking care of:

1.  Creating and storing a database of recipes.
2.  Generating a weekly meal plan.
3.  Deriving your shopping list of ingredients.

## Installation instructions

And the development version from
[GitHub](https://github.com/dzhang32/autorecipes) with:

``` r
devtools::install_github("dzhang32/autorecipes")
```

## Usage

Below is a basic illustration of how to use `autorecipes`. For a more
comprehensive overview, please read the
[vignette](https://dzhang32.github.io/autorecipes/articles/autorecipes.html).

``` r
library("autorecipes")
#> 
#> Attaching package: 'autorecipes'
#> The following objects are masked from 'package:base':
#> 
#>     units, weekdays
# autorecipes contains an example set of recipes in recipebook_example
# recipes stored in RecipeBook-class objects can be retrieved using recipes()
head(recipes(recipebook_example))
#> # A tibble: 6 × 5
#>   index names                                            ingredients fav   last_eaten
#>   <int> <chr>                                            <list>      <lgl> <date>    
#> 1     1 Akoori Eggs with Spinach & Potato                <Ingrdnts>  FALSE NA        
#> 2     2 Crispy Duck with Fava Beans & Caramelised Onions <Ingrdnts>  FALSE NA        
#> 3     3 Chicken with Rosemary Plumns                     <Ingrdnts>  FALSE NA        
#> 4     4 Chickpea & Sweet Potato Masala                   <Ingrdnts>  FALSE NA        
#> 5     5 Gaucho Steaks with Chimichurri Salad             <Ingrdnts>  FALSE NA        
#> 6     6 Chipotle Bean Taco Salad                         <Ingrdnts>  FALSE NA
# create a meal plan for your lunch and dinner across all weekdays
recipebook <- create_meal_plan(recipebook_example)
meal_plan(recipebook)
#> # A tibble: 14 × 7
#>    day   meal   recipe_index names                  ingredients fav   last_eaten
#>    <fct> <fct>         <int> <chr>                  <list>      <lgl> <date>    
#>  1 Mon   Lunch            25 Korean Beef & Kimchi … <Ingrdnts>  FALSE 2021-10-14
#>  2 Mon   Dinner           16 Creamy Pollock, Samph… <Ingrdnts>  FALSE 2021-10-14
#>  3 Tues  Lunch             3 Chicken with Rosemary… <Ingrdnts>  FALSE 2021-10-14
#>  4 Tues  Dinner            7 Steak Tagliata & Roas… <Ingrdnts>  FALSE 2021-10-14
#>  5 Wed   Lunch             2 Crispy Duck with Fava… <Ingrdnts>  FALSE 2021-10-14
#>  6 Wed   Dinner           11 Pink Grapefruit, Mint… <Ingrdnts>  FALSE 2021-10-14
#>  7 Thurs Lunch             1 Akoori Eggs with Spin… <Ingrdnts>  FALSE 2021-10-14
#>  8 Thurs Dinner           13 Creamy Chicken & Much… <Ingrdnts>  FALSE 2021-10-14
#>  9 Fri   Lunch            24 Mexican Enchiladas     <Ingrdnts>  FALSE 2021-10-14
#> 10 Fri   Dinner            9 Squash, Corn & Tomati… <Ingrdnts>  FALSE 2021-10-14
#> 11 Sat   Lunch            26 Afghan Spliced Squash  <Ingrdnts>  FALSE 2021-10-14
#> 12 Sat   Dinner            4 Chickpea & Sweet Pota… <Ingrdnts>  FALSE 2021-10-14
#> 13 Sun   Lunch            19 Beef & Spinach Lasagne <Ingrdnts>  FALSE 2021-10-14
#> 14 Sun   Dinner           12 Honey & Sesame Buffal… <Ingrdnts>  FALSE 2021-10-14
```

## Code of Conduct

Please note that the `autorecipes` project is released with a
[Contributor Code of
Conduct](http://bioconductor.org/about/code-of-conduct/). By
contributing to this project, you agree to abide by its terms.

## Development tools

-   Continuous code testing is possible thanks to [GitHub
    actions](https://www.tidyverse.org/blog/2020/04/usethis-1-6-0/)
    through *[usethis](https://CRAN.R-project.org/package=usethis)*,
    *[remotes](https://CRAN.R-project.org/package=remotes)*, and
    *[rcmdcheck](https://CRAN.R-project.org/package=rcmdcheck)*
    customized to use [Bioconductor’s docker
    containers](https://www.bioconductor.org/help/docker/) and
    *[BiocCheck](https://bioconductor.org/packages/3.14/BiocCheck)*.
-   Code coverage assessment is possible thanks to
    [codecov](https://codecov.io/gh) and
    *[covr](https://CRAN.R-project.org/package=covr)*.
-   The [documentation website](http://dzhang32.github.io/autorecipes)
    is automatically updated thanks to
    *[pkgdown](https://CRAN.R-project.org/package=pkgdown)*.
-   The code is styled automatically thanks to
    *[styler](https://CRAN.R-project.org/package=styler)*.
-   The documentation is formatted thanks to
    *[devtools](https://CRAN.R-project.org/package=devtools)* and
    *[roxygen2](https://CRAN.R-project.org/package=roxygen2)*.

For more details, check the `dev` directory.

This package was developed using
*[biocthis](https://bioconductor.org/packages/3.14/biocthis)*.
