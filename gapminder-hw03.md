gapminder-hw03
================
Nicole Mak
28/09/2018

**First, let’s load the data and the packages we will use.**

``` r
library(gapminder)
library(tidyverse)
```

    ## -- Attaching packages ------------------------------------ tidyverse 1.2.1 --

    ## v ggplot2 3.0.0     v purrr   0.2.5
    ## v tibble  1.4.2     v dplyr   0.7.6
    ## v tidyr   0.8.1     v stringr 1.3.1
    ## v readr   1.1.1     v forcats 0.3.0

    ## -- Conflicts --------------------------------------- tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(dplyr)
library(ggplot2)
```

# Homework \#3 assignment **Task Menu**:

Get the maximum and minimum of GDP per capita for all continents. Look
at the spread of GDP per capita within the continents.

Compute a trimmed mean of life expectancy for different years. Or a
weighted mean, weighting by population. Just try something other than
the plain vanilla mean.

How is life expectancy changing over time on different continents?

Report the absolute and/or relative abundance of countries with low life
expectancy over time by continent: Compute some measure of worldwide
life expectancy – you decide – a mean or median or some other quantile
or perhaps your current age. Then determine how many countries on each
continent have a life expectancy less than this benchmark, for each
year.

Find countries with interesting stories. Open-ended and, therefore,
hard. Promising but unsuccessful attempts are encouraged. This will
generate interesting questions to follow up on in class.

## First exercise:

Get the maximum and minimum of GDP per capita for all continents. Look
at the spread of GDP per capita within the continents.

``` r
gapminder %>% 
  group_by(continent) %>% 
  summarize(min(gdpPercap), max(gdpPercap))
```

    ## # A tibble: 5 x 3
    ##   continent `min(gdpPercap)` `max(gdpPercap)`
    ##   <fct>                <dbl>            <dbl>
    ## 1 Africa                241.           21951.
    ## 2 Americas             1202.           42952.
    ## 3 Asia                  331           113523.
    ## 4 Europe                974.           49357.
    ## 5 Oceania             10040.           34435.

``` r
#structuring data by continent
#then requesting that desired observation (minimum and maximum) be summarized in a table, by continent
```

## Second exercise

Compute a trimmed mean of life expectancy for different years. Or a
weighted mean, weighting by population. Just try something other than
the plain vanilla mean.

``` r
gapminder %>% 
  group_by(continent, year) %>% 
  summarise(mean_lifeExp_weighted = weighted.mean(lifeExp, pop))
```

    ## # A tibble: 60 x 3
    ## # Groups:   continent [?]
    ##    continent  year mean_lifeExp_weighted
    ##    <fct>     <int>                 <dbl>
    ##  1 Africa     1952                  38.8
    ##  2 Africa     1957                  40.9
    ##  3 Africa     1962                  43.1
    ##  4 Africa     1967                  45.2
    ##  5 Africa     1972                  47.2
    ##  6 Africa     1977                  49.2
    ##  7 Africa     1982                  51.0
    ##  8 Africa     1987                  52.8
    ##  9 Africa     1992                  53.4
    ## 10 Africa     1997                  53.3
    ## # ... with 50 more rows

``` r
#This is a calculation of the weighted mean life expectancy by population for each continent. The means are separated by year so we can observe trends of change in lifeExp over time.
```

## Third exercise

We can use the above information to make some figures which illustrate
how life expectancy changes over time.

How is life expectancy changing over time on different continents?
