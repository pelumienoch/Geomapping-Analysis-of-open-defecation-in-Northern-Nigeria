pacman::p_load(
  rio,          # File import
  here,         # File locator
  tidyverse,    # data management + ggplot2 graphics, 
  stringr,      # manipulate text strings 
  purrr,        # loop over objects in a tidy way
  gtsummary,    # summary statistics and tests 
  broom,        # tidy up results from regressions
  lmtest,       # likelihood-ratio tests
  parameters,   # alternative to tidy up results from regressions
  see,          # alternative to visualise forest plots
  broom.helpers # for tidying up regression tables
)


#### I want to performa a regression analysis

OD_REG <- import(here("Dataset1.xlsx"))      # I am importing my dataset

## define variables of interest 
explanatory_vars <-(c("Age","Settlement","Average Income/Month",
                      "What is the highest degree or level of school you have completed?",
                      "Marital status", "Occupation"))

## drop rows with missing information for variables of interest 
OD_REG <- OD_REG %>% 
  drop_na(any_of(c(explanatory_vars)))

univ_lack <- OD_REG %>% 
  dplyr::select(explanatory_vars,`Lack of toilet facilities in houses`) %>% ## select variables of interest
  
  tbl_uvregression(                         ## produce univariate table
    method = glm,                           ## define regression want to run (generalised linear model)
    y = `Lack of toilet facilities in houses`,                            ## define outcome variable
    method.args = list(family = binomial),  ## define what type of glm want to run (logistic)
    exponentiate = TRUE                     ## exponentiate to produce odds ratios (rather than log odds)
  )
### error return ooo!!!! says i should install broom.helpers
install.packages("broom.helpers")
#### another error return, need me to convert the Y variable to 0 and 1

## convert dichotomous variables to 0/1 
OD_REGG <- OD_REG %>%  
  mutate(across(                                      
    .cols = all_of(c("Lack of toilet facilities in houses")),  ## for each column listed and "outcome"
    .fns = ~case_when(                              
      . %in% c("Yes")   ~ 1,           ## recode male, yes and death to 1
      . %in% c("No") ~ 0,           ## female, no and recover to 0
      TRUE                            ~ NA_real_)    ## otherwise set to missing
  )
  )

### Now let's try our regression again

univ_lack <- OD_REG %>% 
  dplyr::select(explanatory_vars,`Lack of toilet facilities in houses`) %>% ## select variables of interest
  
  tbl_uvregression(                         ## produce univariate table
    method = glm,                           ## define regression want to run (generalised linear model)
    y = `Lack of toilet facilities in houses`,                            ## define outcome variable
    method.args = list(family = binomial),  ## define what type of glm want to run (logistic)
    exponentiate = TRUE                     ## exponentiate to produce odds ratios (rather than log odds)
  )

## view univariate results table 
univ_tab
