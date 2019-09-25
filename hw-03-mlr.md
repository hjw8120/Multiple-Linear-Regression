HW 03: Multiple Linear Regression
================
INSERT NAME
INSERT DATE

``` r
library(tidyverse)
library(broom)
library(knitr) 
```

``` r
# for Question 8
houses <- read_csv("data/KingCountyHouses.csv")
```

### Question 1

### Question 2

### Question 3

### Question 4

### Question 5

### Question 6

### Question 7

### Question 8

``` r
houses <- houses %>%
  filter(bedrooms <= 5 ) %>%
  mutate(floorsCat = as.factor(floors), 
         sqftCent = sqft - mean(sqft), 
         bedroomsCent = bedrooms - mean(bedrooms),
         bathroomsCent = bathrooms-mean(bathrooms),
         logprice = log(price))
```

### Overall (do not delete\!)
