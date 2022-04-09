## Analysis of Missingness
# A lot of the write up on this script suggests maybe that a markdown/report could be in the works?

library(tidyverse)
library(mice)
library(naniar)
library(UpSetR)
library(ggplot2)
library(VIM)
library(reshape2)

# Graphing patterns of missingness
bench$CHURvey$chur_wide %>% 
  as_shadow_upset() %>%
  upset(nsets = 7)

# An additional visual representation of missingness
ggplot_missing <- function(x){
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

ggplot_missing(bench$CHURvey$chur_wide) ## Map of missingness

mean(complete.cases(bench$CHURvey$chur_wide)) # Percentage of complete surveys
sum(complete.cases(bench$CHURvey$chur_wide)) # Total number of complete cases
sum(is.na(bench$CHURvey$chur_wide))/prod(dim(bench$CHURvey$chur_wide)) # Total percent of missingess overall
sapply(bench$CHURvey$chur_wide, function(x) sum(is.na(x))) # Sum of each individual item's missingness
sapply(bench$CHURvey$chur_wide, function(x) mean(is.na(x))) %>%
  round(digits = 2) # Percentage of each individual item's missingness

miss_test <- # Creating a df for purposes of analysis of missingness and imputation
  bench$CHURvey$chur_wide %>% # Should probably hang this on this list.
  select(ID, city, num_nights, burner_v2:whatfest) %>%
  mutate(whatfest = as.numeric(factor(whatfest, labels = c('1', '2', '3'))))

mcar_test(miss_test) #MCAR Test for Analysis of Missingess

  # Little's MCAR test
  ## First LittleMCAR test failed indicating that data was not MCAR suggesting that imputation could not be 
  ## performed.  Individuals responses were analyzed to see if patterns of missingness in the data could be inferred.
  
  ## sure enough...
  
  ## Respondent 46: missing data on whatkind and farfest having answered 1 for keenfest.  Very possible that 
  ## that these were left blank on account of the individual not being keen on a Chur Festival at all and them
  ## believing that answering these questions did not apply.

miss_test$whatfest[46] <- 1
miss_test$farfest[46] <- 1

## Once these new items were added to the dataset, a second Little MCAR test was performed, whereby non-significant
## p value was obtained, indicating that data was MCAR and that we could proceded with missing imputation. 
## Although the values for Whatfest and Farfest were low, it does not necessarily follow that the keenfest score for 
## this individual would be low as well (i.e. there is a high likelyhood of randomness pertaining to the missingness of 
## this item as indicated by a non-significant Little's MCAR test result).  

mcar_test(miss_test) # Second application of Little's MCAR test after manual imputations of missing values.

# Imputation of Missing Data using mice package.
mi.temp <- 
  mice(miss_test, m = 5, maxit = 50, meth = 'pmm', seed = 500)
summary(mi.temp)
mi.temp$imp$keenfest

miss_test <- complete(mi.temp, 1) # Chosen as a conservative value, which is also consistent with outputs of other imputation methods (e.g. Bayes)

