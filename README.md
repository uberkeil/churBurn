---
title: "Chur Burn 2018 Dataset"
author: "Kyle Simpson"
date: "4/10/2022"
output: github_document
---

## An Example of a Data Cleaning & Analysis of Missingness Workflow Using:

``` r
library(tidyverse)
library(tidytext)
library(mice)
library(naniar)
library(UpSetR)
library(ggplot2)
library(VIM)
library(reshape2)
```

### Creating a list architecture

I personally like to keep a relatively uncluttered R environment.  Personally, I find that storing the objects and outputs I produce over the course of an analysis on a list helps me better conceptualize my project.

```r
bench <- list(raw_data = "", 
              chur_tidy = "",
              chur_clean = "",
              temp = list(""),
              miss = "")
```

### Importing raw data file

Likewise, I enjoy the `here` package as it helps facilitate project based programming and maintaining a uniform
file structure across all projects. 

```r
bench$raw_data <- 
  read_csv(file = here("data", "raw", "ChurBurn2018_public.csv")) %>%
  as_tibble()
```

### I'm also a fan of pipes

```r
bench$chur_tidy <-
  bench$raw_data %>%
  dplyr::select(city = starts_with("Where do you"),  #Renaming variable headings using select function.
                nights = starts_with("What nights"), 
                nxtyear = starts_with("Would you buy"),
                safety = starts_with("How safe"),
                gtp = starts_with("Were you aware"),
                love = starts_with("What do you love"),
                dislike = starts_with("What do you dis"),
                improve = starts_with("What could we add or"),
                burner = starts_with("Have you ever"),
                keenfest = starts_with("How about"),
                whatfest = starts_with("What kind of"),
                farfest = starts_with("How far"),
                fdback = starts_with("Any other")) %>%
  distinct() %>% #Timestamp was not included so that duplicates could be removed using distinct function
  rowid_to_column("ID") %>% #Adding a row ID column
  mutate_all(funs(tolower)) %>% #Changing all strings to lower (this is useful for manipulating the `city` entries)
  mutate(across(where(is.character), str_trim)) %>% #Trimming white space
  mutate(city = replace(city, str_detect(city, "well"), "wellington")) %>% #Now all the various spellings of 'wellington' can be collapsed into one fariable.
  mutate_at(vars(ID, safety, keenfest), as.numeric) %>% #Ensuring that all numerical data is of the numeric class
  mutate_if(is.character, as.factor) %>% #...and all characters are now factors.
  mutate(city = fct_collapse(city,  #Collapsing various localities into fewer regions
                                WELL = c("wellington"),
                                AUCK = c("auckland"),
                                CHCH = c("christchurch"), 
                                DNDN = c("dunedin"),
                                TRVL = c("antwerp", "traveling"),
                                GSBN = "gisborne", 
                                TAUR = c("mount maunganui", "tauranga", "te puke"),
                                WEL_REG = c("kapiti", "lower hutt", "porirua"),
                                CENT_NI = c("manawatu", "palmerston north", "stratford"))) %>%
  mutate(welly_TF = fct_collapse(city, #Creating a welly_TF variable.
                                 Y = c("WELL", "WEL_REG"),
                                 N = c("AUCK", "CHCH","DNDN", "TRVL", "GSBN", "TAUR", "CENT_NI"))) %>% # Should this be boolean?
  mutate(welly_TF = ifelse(grepl("WEL", city), TRUE, FALSE)) %>% #Likely got here with too many steps, but won't trim the fat just yet.
  mutate(nights = factor(nights, labels = c('BOTH', 'FRI', 'SAT'))) %>% #Renaming levels of factor `nights`
  mutate(num_nights = factor(ifelse(nights != "BOTH", "1", "2"))) %>% #Creating a number of nights in attendence variable.
  mutate(nxtyear = factor(nxtyear, levels = c( # Collapsing data by interest irrespective of actual nights attended. 
    "one night was enough for me",  # There was likely a way to get here via grepl or some similar process...
    "one night was enough for me;i don't mind either way", # ...but this is the approach for now.
    "i went one night, but would go for two nights next year;one night was enough for me",
    "i went one night, but would go for two nights next year;i don't mind either way",
    "i don't mind either way",
    "i went two nights and would do two nights again next year;i don't mind either way", 
    "i went one night, but would go for two nights next year",
    "i went two nights and would do two nights again next year",
    "i went one night, but would go for two nights next year;i could probably do a third night",
    "i went two nights and would do two nights again next year;i could probably do a third night",
    "i could probably do a third night"), 
    labels = c('A','B','C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K'))) %>% #New Labels for the items of this variable.
  mutate(nxtyear = fct_collapse(nxtyear,  ## Collapsing how many nights next year survey item by interest in double pass for next year.
                                   A = c("A"), B = c("B", "C", "D", "E", "F"), C = c("G", "H"), D = c("I", "J", "K"))) %>%
  mutate(burner = factor(burner, labels = c(
    'Never', 'Once', 'Few', 'Veteran'))) %>% #USE THIS to experiment with label/level recall
  mutate(burner_v2 = factor(burner, labels = c(
    'A', 'B', 'C', 'D'))) %>%
  mutate(whatfest = factor(whatfest, levels = c( #short hand labels for the whatfest variable.
    "a weekend doof (camping, bass music, dancing)",
    "a long weekend festie (camping, music, dancing, workshops, talks)",
    "a longer larger festival (camping, music, dancing, workshops, performances, multiple stages/zones)"),
    labels = c(
      'weekend doof', 'weekend festival', 'long festival'))) %>%
  mutate(farfest = as.numeric(factor(farfest, levels = c( #Changing this variable to a likert 1-7 scale
    "up to 30 mins",
    "up to 1 hr",
    "up to 1.5 hrs",
    "up to 2.0 hrs",
    "up to 3.0 hrs", 
    "up to 4.0 hrs", 
    "any distance"),
    labels = c('1', '2', '3', '4', '5', '6', '7')))) %>% # This will need a write up, and a 'code book' definitely needs to be created.
  mutate(gtp = as.numeric(factor(gtp, labels = c(1, 2, 3, 4)))) %>% #Changing the gtp variable to a 1:4 scale.
  select(ID, starts_with("city"), welly_TF, starts_with("nights"), starts_with("num"), starts_with("burner"), #Reordering variables in the df/tibble
         starts_with("nxtyear"), starts_with("gtp"), starts_with("safety"), 
         starts_with("keen"), starts_with("far"), starts_with("what"), love, dislike, improve, fdback) %>%
  mutate_if(is.character, as.factor) #Making sure any factors coerced to characters are factors again.
```

If one so desired, they could export the tidy (in terms of formatting) but not yet clean (because of missingness) dataset from the R environment
```r
bench$chur_clean %>% #this is purely for demonstration purposes.
  write_csv(here("data","clean", "chur2018_clean.csv")) #this could simply be an object in the r enviroinment
```

## Missingness

### Summary Statistics of Missingess

``` r
round(mean(complete.cases(bench$chur_tidy)),2) # Percentage of complete surveys
#> 0.39
```

``` r
sum(complete.cases(bench$chur_tidy)) # Total number of complete cases
#> 53
```

``` r
round(sum(is.na(bench$chur_tidy))/prod(dim(bench$chur_tidy)), 2) # Total percent of missingess overall
#> 0.06
```

``` r
sapply(bench$chur_tidy, function(x) sum(is.na(x))) # Sum of each individual item's missingness
#> ID       city       welly_TF   nights    num_nights   burner   nxtyear    gtp     
#> 0        0          0          0         0            0        1          0          
#> safety   keenfest   farfest    whatfest  love         dislike  improve    fdback 
#> 0        1          1          1         9            34       25         68 
```

``` r
sapply(bench$chur_tidy, function(x) mean(is.na(x))) %>%
  round(digits = 2) # Percentage of each individual item's missingness
#> ID       city       welly_TF   nights    num_nights   burner   nxtyear    gtp     
#> 0.00     0.00       0.00       0.00      0.00         0.00     0.01       0.00    
#> safety   keenfest   farfest    whatfest  love         dislike  improve    fdback   
#> 0.00     0.01       0.01       0.01      0.07         0.25     0.19       0.50 
```

### Graphs of Missingness

### Graphing Patterns of Missingness

``` r
bench$chur_tidy %>% # Graphing patterns of missingness
  as_shadow_upset() %>%
  upset(nsets = 7)
```
#### Figure 1: Pattern of Missingness

<p align="center">
<img src='https://raw.githubusercontent.com/uberkeil/churBurn/master/figures/naniar_graph.png'>
</p>


### Graphing Map of Missingness

``` r
ggplot_missing <- function(x){ # Map of missingness function
  x %>% is.na %>%
    melt %>%
    ggplot(data = .,
           aes(x = Var2, 
               y = Var1)) +
    geom_raster(aes(fill = value))+
    scale_fill_grey(name = "",
                    labels = c("Present", "Missing"))+
    theme_minimal()+
    theme(axis.text.x = element_text(angle=45, vjust = 0.7)) +
    labs(x = "Variables in Dataset",
         y = "Rows / Observations")
}

ggplot_missing(bench$chur_tidy) ## Map of missingness
```
#### Figure 2: Map of Missingness

<p align ="center">
<img src='https://raw.githubusercontent.com/uberkeil/churBurn/master/figures/missingMap.png'>
</p>

### Little's MCAR Test & Multivariate Imputation by Chained Equations (mice)

We can see that the open ended questions have the greatest amount of missingness, and are likely to have 
been left blank in many cases due to respondents having an aversion to open ended questions.  Moreover, 
barring reducing every open ended question to a continuous variable (e.g. via sentiment analysis) there is 
no way that imputation can be applied to these missing values.

For this reason, a subset containing only categorical and continuous variables was be created.

```r
bench$miss <- # Creating a df for purposes of analysis of missingness and imputation
  bench$chur_tidy %>% # Should probably hang this on this list.
  select(ID, city, num_nights, burner, nxtyear:whatfest) %>%
  mutate(whatfest = as.numeric(factor(whatfest, labels = c('1', '2', '3'))))

#> A tibble: 135 × 10
#>    ID city  num_nights burner  nxtyear   gtp safety keenfest farfest whatfest
#> <dbl> <fct> <fct>      <fct>   <fct>   <dbl>  <dbl>    <dbl>   <dbl>    <dbl>
#>     1 WELL  2          Never   D           1      7        7       7        2
#>     2 WELL  2          Once    C           4      7        7       7        3
#>     3 AUCK  2          Once    C           2      6        7       6        3
#>     4 TAUR  2          Veteran D           2      7        7       7        3
#>     5 TRVL  1          Never   A           4      7        7       6        1
#>     6 WELL  1          Never   D           2      7        7       2        2
#>     7 WELL  1          Veteran A           4      5       NA       3        2
#>     8 WELL  1          Veteran C           4      7        7       4        1
#>     9 WELL  2          Once    C           4      7        7       7        2
#>    10 WELL  2          Veteran C           4      6        7       7        2
#> … with 125 more rows 
```

Applying Little's MCAR test produces a significant result (p < .05) meaning that the missing data is
not missing completely at random. 

```r
mcar_test(bench$miss) #MCAR Test for Analysis of Missingess
#> A tibble: 1 × 4
#>  statistic    df p.value missing.patterns
#>      <dbl> <dbl>   <dbl>            <int>
#>      35.0     17 0.00619                3
```

But when we look at the missing items for ID = 46, we can see that the respondent likely  
left these items blank given that there was no distance that they'd be willing to travel
or a form of festival that they'd be interested in attending. This is an important consideration
for survey design, in that the item should have accounted for this. 

```r
bench$miss[46, 8:10]
#> A tibble: 1 × 3
#> keenfest farfest whatfest
#>    <dbl>   <dbl>    <dbl>
#>        1      NA       NA
```
In this case however, because we have rescaled the items in question, we can simply attribute the 
lowest value in each case. 

```r
bench$miss$whatfest[46] <- 1
bench$miss$farfest[46] <- 1
```

Performing Little's MCAR test again produces a non-significant result (p = .184) meaning that we can presume
that the rest of the missingness in this dataset is MCAR.

```r
mcar_test(bench$miss) # Second application of Little's MCAR test after manual imputations of missing values.
#> A tibble: 1 × 4
#>  statistic    df p.value missing.patterns
#>      <dbl> <dbl>   <dbl>            <int>
#>       12.6     9   0.184                2
```

Multivariate imputation can now be applied to the data set via weighted predictive mean matching...

```r
bench$temp$mi.temp <- 
  mice(bench$miss, m = 5, maxit = 50, meth = 'midastouch', seed = 500)

summary(bench$temp$mi.temp)
```

...which produces a value of '4' for the missing value. 

```r
bench$temp$mi.temp$imp$keenfest #eyeballing imputed data as generated by mice
#>  1 2 3 4 5
#>  4 5 4 4 4
```

This is imputed onto the temporary `miss` dataset, which is then merged with the openended portion
of the orignal datset using a merging join.  The resulting object `chur_clean` is written as a .csv
in our `clean_data` directory.

```r
bench$miss <- as.tibble(complete(bench$temp$mi.temp, 1)) 

bench$chur_clean <- #Merging OE questions with the rest of the dataset
  left_join(bench$miss, select(bench$chur_tidy, ID, welly_TF, nights, love:fdback), by = "ID") %>%
  select(ID, city, welly_TF, nights, num_nights, nxtyear, burner,  gtp:whatfest, love:fdback)

bench$chur_clean %>% #this is purely for demonstration purposes.
  write_csv(here("data", "clean", "chur2018_clean.csv")) #this could simply be an object in the r enviroinment
```