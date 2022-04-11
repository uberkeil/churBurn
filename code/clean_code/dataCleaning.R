#Importing and cleaning
# Note: Does not include analysis of missingness (see missingness.R)

library(tidyverse)
library(readr)
library(tidytext)
library(forcats)
library(here)

# Creating the template for lists that will be populated with various outputs/objects
bench <- list(raw_data = "", 
              chur_tidy = "",
              chur_clean = "",
              temp = list(""),
              miss = "")

## Importing raw data file
bench$raw_data <- 
  read_csv(file = here("data", "ChurBurn2018_public.csv")) %>%
  as_tibble()

# Cleaning the dataset via many pipes and functions of the tidyverse
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

bench$chur_tidy %>% #this is purely for demonstration purposes.
  write_csv(here("data", "chur2018_tidy.csv")) #this could simply be an object in the r enviroinment


