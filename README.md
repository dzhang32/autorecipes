
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

The goal of `autorecipes` is to automate my weekly recipe planning (and
practice using OOP in `R`).

`autorecipes` achieves this by taking care of:

1.  Creating and storing a database of recipes
2.  Customizing weekly meal plans
3.  Generating shopping lists of ingredients

## Installation

`autorecipes` exists purely for personal use and is no longer under
active development. If you’d like to install the development version
from GitHub you can use the following:

``` r
devtools::install_github("dzhang32/autorecipes")
```

## Usage

The below example gives a taste of `autorecipes`. If you would like to
sink your teeth into a more detailed tutorial, head over to the
[vignette](https://dzhang32.github.io/autorecipes/articles/autorecipes.html).

``` r
library("autorecipes")
#> 
#> Attaching package: 'autorecipes'
#> The following objects are masked from 'package:base':
#> 
#>     units, weekdays

# autorecipes contains an example set of recipes
head(recipes(recipebook_example))
#> # A tibble: 6 × 5
#>   index names                                       ingredients fav   last_eaten
#>   <int> <chr>                                       <list>      <lgl> <date>    
#> 1     1 Akoori Eggs with Spinach & Potato           <Ingrdnts>  FALSE NA        
#> 2     2 Crispy Duck with Fava Beans & Caramelised … <Ingrdnts>  FALSE NA        
#> 3     3 Chicken with Rosemary Plumns                <Ingrdnts>  FALSE NA        
#> 4     4 Chickpea & Sweet Potato Masala              <Ingrdnts>  FALSE NA        
#> 5     5 Gaucho Steaks with Chimichurri Salad        <Ingrdnts>  FALSE NA        
#> 6     6 Chipotle Bean Taco Salad                    <Ingrdnts>  FALSE NA

# create a meal plan for lunch and dinner across all weekdays
recipebook <- create_meal_plan(recipebook_example)

head(meal_plan(recipebook))
#> # A tibble: 6 × 7
#>   day   meal   recipe_index names                   ingredients fav   last_eaten
#>   <fct> <fct>         <int> <chr>                   <list>      <lgl> <date>    
#> 1 Mon   Lunch            11 Pink Grapefruit, Mint,… <Ingrdnts>  FALSE 2022-03-19
#> 2 Mon   Dinner            6 Chipotle Bean Taco Sal… <Ingrdnts>  FALSE 2022-03-19
#> 3 Tues  Lunch            23 Hoisin Beef Noodles & … <Ingrdnts>  FALSE 2022-03-19
#> 4 Tues  Dinner            2 Crispy Duck with Fava … <Ingrdnts>  FALSE 2022-03-19
#> 5 Wed   Lunch            21 Chickpea Falafels with… <Ingrdnts>  FALSE 2022-03-19
#> 6 Wed   Dinner            3 Chicken with Rosemary … <Ingrdnts>  FALSE 2022-03-19

recipebook <- create_shopping_list(recipebook)

head(shopping_list(recipebook))
#> # A tibble: 6 × 2
#>   names            n
#>   <chr>        <int>
#> 1 garlic clove    11
#> 2 onion            6
#> 3 lime             5
#> 4 ginger           4
#> 5 red onion        4
#> 6 sesame seeds     4
```

## Credits

`autorecipes` was developed using
*[biocthis](https://bioconductor.org/packages/3.14/biocthis)*.
