
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

As an unadventurous, by-the-recipe cook, I had accumulated a set of \~25
go-to meals that were cycled through weekly depending on mood, time and
what I’d had the week before. Over time, this process became repetitive.
`autorecipes` was born from this repetition; it takes care of:

1.  Creating and storing a database of recipes
2.  Customizing weekly meal plans
3.  Generating shopping lists of ingredients

## Installation

``` r
devtools::install_github("dzhang32/autorecipes")
```

## Usage

The below example gives a taste of `autorecipes`. If you would like to
sink your teeth into a more detailed overview, please head over to the
[vignette](https://dzhang32.github.io/autorecipes/articles/autorecipes.html).

``` r
library("autorecipes")

# autorecipes contains an example set of recipes in recipebook_example
# recipes stored in RecipeBook-class objects can be retrieved using recipes()
head(recipes(recipebook_example))

# create a meal plan for your lunch and dinner across all weekdays
recipebook <- create_meal_plan(recipebook_example)
meal_plan(recipebook)
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
    customized to use [Bioconductor”s docker
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
