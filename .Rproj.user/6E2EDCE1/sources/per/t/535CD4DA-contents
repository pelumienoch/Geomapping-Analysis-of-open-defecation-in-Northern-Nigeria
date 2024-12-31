pacman::p_load(
  rio,          # File import
  here,         # File locator
  skimr,        # get overview of data
  tidyverse,    # data management + ggplot2 graphics, 
  gtsummary,    # summary statistics and tests
  forcats,
  rstatix,      # statistics
  corrr,        # correlation analayis for numeric variables
  janitor,      # adding totals and percents to tables
  flextable,     # converting tables to HTML
  apyramid,     # a package dedicated to creating age pyramids
  stringr,
  dplyr
)

OD_TAB <- import(here("Dataset1.xlsx"))      # I am importing my dataset

### To perform CHI-SQUARE table for the lack of toilet facilites
Tchi <- OD_TAB %>% 
  select(Age,Gender,Settlement,Household2,`Average Income/Month`,
         `What is the highest degree or level of school you have completed?`,
         `Marital status`, Occupation, `Lack of toilet facilities in houses`) %>% # keep only columns of interest
  tbl_summary(by = `Lack of toilet facilities in houses`,                                               # stratify entire table by toilet
    statistic = list( all_continuous() ~ "{mean} ({sd})",        # stats and format for continuous columns
                     all_categorical() ~ "{n} ({p}%)"),   # stats and format for categorical columns
   digits = all_continuous() ~ 1,                              # rounding for continuous columns
    type   = all_categorical() ~ "categorical",                 # force all categorical levels to display
    label  = list(                                              # display labels for column names
      #toilet  ~ "Lack of toilet facilities in houses",                           
      Age ~ "Age group",
      Gender    ~ "Gender",
      Settlement      ~ "Settlement",
      Household2  ~ "Household",
      `Average Income/Month` ~ "Average Income/Month",
      `What is the highest degree or level of school you have completed?` ~ "Educational level",
      `Marital status` ~ "Marital status",
      Occupation ~ "Occupation"),
    missing_text = "Missing"                                    # how missing values should display
  ) %>%
  add_p(
    test = list(
      all_categorical() ~ "chisq.test"  # Use chi-square test for all categorical variables
    )
  )                                               

### Time to convert our tibble into dataframe so as to use flextable to generate a word document type table
Tchi <- as.data.frame(Tchi)
Tchi  <- flextable(Tchi) %>%
  save_as_docx(Tchi, path = "Chi square table lack of toilet.docx")

names(OD_TAB)

### I keep getting error o, the bug is most likely because of the data type of lack of toilet facilities

skim(OD_TAB)

# Convert grouping variable to factor (if not already)
OD_TAB <- OD_TAB %>%
  mutate(`Lack of toilet facilities in houses` = as.factor(`Lack of toilet facilities in houses`)) %>%
  rename(toilet = `Lack of toilet facilities in houses`)
### I have changed the data type of the lack of toilet, now i want to go back to the code and try


### stll getting the error, i realized that after using the select function, i didnt add the toilet or lack of toilet variable inside the select function,
## so it was difficult to perform any table summary when toilet is not part of the new selected subset
# So i added toilet to the select function and re-run the test, and boooooom it worked




### To perform CHI-SQUARE table for the `Poor understanding of effect of open defecation`
PoorCHI <- OD_TAB %>% 
  select(Age,Gender,Settlement,Household2,`Average Income/Month`,
         `What is the highest degree or level of school you have completed?`,
         `Marital status`, Occupation,`Poor understanding of effect of open defecation`) %>% # keep only columns of interest
  tbl_summary(by = `Poor understanding of effect of open defecation`,                                               # stratify entire table by toilet
              statistic = list( #all_continuous() ~ "{mean} ({sd})",        # stats and format for continuous columns
                                all_categorical() ~ "{n} ({p}%)"),   # stats and format for categorical columns
              #digits = all_continuous() ~ 1,                              # rounding for continuous columns
              type   = all_categorical() ~ "categorical",                 # force all categorical levels to display
              label  = list(                                              # display labels for column names
                #`Poor understanding of effect of open defecation`  ~ "Poor understanding of effect of open defecation",                           
                Age ~ "Age group",
                Gender    ~ "Gender",
                Settlement      ~ "Settlement",
                Household2  ~ "Household",
                `Average Income/Month` ~ "Average Income/Month",
                `What is the highest degree or level of school you have completed?` ~ "Educational level",
                `Marital status` ~ "Marital status",
                Occupation ~ "Occupation"),
              missing_text = "Missing"                                    # how missing values should display
  ) %>%
  add_p(test = all_categorical() ~ "chisq.test")                                             

### Time to convert our tibble into dataframe so as to use flextable to generate a word document type table
PoorCHI <- as.data.frame(PoorCHI)
PoorCHI  <- flextable(PoorCHI) %>%
  save_as_docx(PoorCHI, path = "Chi square poor understanding.docx")



### To perform CHI-SQUARE table for the Cultural practice`
CultureCHI <- OD_TAB %>% 
  select(Age,Gender,Settlement,Household2,`Average Income/Month`,
         `What is the highest degree or level of school you have completed?`,
         `Marital status`,Occupation, `Cultural practice`) %>% # keep only columns of interest
  tbl_summary(by = `Cultural practice`,                                               # stratify entire table by toilet
              statistic = list( #all_continuous() ~ "{mean} ({sd})",        # stats and format for continuous columns
                all_categorical() ~ "{n} ({p}%)"),   # stats and format for categorical columns
              #digits = all_continuous() ~ 1,                              # rounding for continuous columns
              type   = all_categorical() ~ "categorical",                 # force all categorical levels to display
              label  = list(                                              # display labels for column names
                #Cultural practice``  ~ "Cultural practice`",                           
                Age ~ "Age group",
                Gender    ~ "Gender",
                Settlement      ~ "Settlement",
                Household2  ~ "Household",
                `Average Income/Month` ~ "Average Income/Month",
                `What is the highest degree or level of school you have completed?` ~ "Educational level",
                `Marital status` ~ "Marital status",
                Occupation ~ "Occupation"),
              missing_text = "Missing"                                    # how missing values should display
  ) %>%
  add_p(test = all_categorical() ~ "chisq.test")                                             

### Time to convert our tibble into dataframe so as to use flextable to generate a word document type table
CultureCHI <- as.data.frame(CultureCHI)
CultureCHI  <- flextable(CultureCHI) %>%
  save_as_docx(CultureCHI, path = "Chi square Cultural practices.docx")




### To perform CHI-SQUARE table for the Religious practice
ReligiousCHI <- OD_TAB %>% 
  select(Age,Gender,Settlement,Household2,`Average Income/Month`,
         `What is the highest degree or level of school you have completed?`,
         `Marital status`,Occupation,`Religious practice`) %>% # keep only columns of interest
  tbl_summary(by = `Religious practice`,                                               # stratify entire table by toilet
              statistic = list( #all_continuous() ~ "{mean} ({sd})",        # stats and format for continuous columns
                all_categorical() ~ "{n} ({p}%)"),   # stats and format for categorical columns
              #digits = all_continuous() ~ 1,                              # rounding for continuous columns
              type   = all_categorical() ~ "categorical",                 # force all categorical levels to display
              label  = list(                                              # display labels for column names
                #Cultural practice``  ~ "Cultural practice`",                           
                Age ~ "Age group",
                Gender    ~ "Gender",
                Settlement      ~ "Settlement",
                Household2  ~ "Household",
                `Average Income/Month` ~ "Average Income/Month",
                `What is the highest degree or level of school you have completed?` ~ "Educational level",
                `Marital status` ~ "Marital status",
                Occupation ~ "Occupation"),
              missing_text = "Missing"                                    # how missing values should display
  ) %>%
  add_p(test = all_categorical() ~ "chisq.test")                                             

### Time to convert our tibble into dataframe so as to use flextable to generate a word document type table
ReligiousCHI <- as.data.frame(ReligiousCHI)
ReligiousCHI  <- flextable(ReligiousCHI) %>%
  save_as_docx(ReligiousCHI, path = "Chi square Religious practice.docx")


### To perform CHI-SQUARE table for the `Nonchalant attitude`
NaCHI <- OD_TAB %>% 
  select(Age,Gender,Settlement,Household2,`Average Income/Month`,
         `What is the highest degree or level of school you have completed?`,
         `Marital status`,Occupation,`Nonchalant attitude`) %>% # keep only columns of interest
  tbl_summary(by = `Nonchalant attitude`,                                               # stratify entire table by toilet
              statistic = list( #all_continuous() ~ "{mean} ({sd})",        # stats and format for continuous columns
                all_categorical() ~ "{n} ({p}%)"),   # stats and format for categorical columns
              #digits = all_continuous() ~ 1,                              # rounding for continuous columns
              type   = all_categorical() ~ "categorical",                 # force all categorical levels to display
              label  = list(                                              # display labels for column names
                #Cultural practice``  ~ "Cultural practice`",                           
                Age ~ "Age group",
                Gender    ~ "Gender",
                Settlement      ~ "Settlement",
                Household2  ~ "Household",
                `Average Income/Month` ~ "Average Income/Month",
                `What is the highest degree or level of school you have completed?` ~ "Educational level",
                `Marital status` ~ "Marital status",
                Occupation ~ "Occupation"),
              missing_text = "Missing"                                    # how missing values should display
  ) %>%
  add_p(test = all_categorical() ~ "chisq.test")                                             

### Time to convert our tibble into dataframe so as to use flextable to generate a word document type table
NaCHI <- as.data.frame(NaCHI)
NaCHI  <- flextable(NaCHI) %>%
  save_as_docx(NaCHI, path = "Chi square Nonchallant.docx")



### To perform CHI-SQUARE table for the `Peer group pressure`
PGPCHI <- OD_TAB %>% 
  select(Age,Gender,Settlement,Household2,`Average Income/Month`,
         `What is the highest degree or level of school you have completed?`,
         `Marital status`,Occupation,`Peer group pressure`) %>% # keep only columns of interest
  tbl_summary(by = `Peer group pressure`,                                               # stratify entire table by toilet
              statistic = list( #all_continuous() ~ "{mean} ({sd})",        # stats and format for continuous columns
                all_categorical() ~ "{n} ({p}%)"),   # stats and format for categorical columns
              #digits = all_continuous() ~ 1,                              # rounding for continuous columns
              type   = all_categorical() ~ "categorical",                 # force all categorical levels to display
              label  = list(                                              # display labels for column names
                #Cultural practice``  ~ "Cultural practice`",                           
                Age ~ "Age group",
                Gender    ~ "Gender",
                Settlement      ~ "Settlement",
                Household2  ~ "Household",
                `Average Income/Month` ~ "Average Income/Month",
                `What is the highest degree or level of school you have completed?` ~ "Educational level",
                `Marital status` ~ "Marital status",
                Occupation ~ "Occupation"),
              missing_text = "Missing"                                    # how missing values should display
  ) %>%
  add_p(test = all_categorical() ~ "chisq.test")                                             

### Time to convert our tibble into dataframe so as to use flextable to generate a word document type table
PGPCHI <- as.data.frame(PGPCHI)
PGPCHI  <- flextable(PGPCHI) %>%
  save_as_docx(PGPCHI, path = "Chi square peer group.docx")



### To perform CHI-SQUARE table for the TABOO
TABOOCHI <- OD_TAB %>% 
  select(Age,Gender,Settlement,Household2,`Average Income/Month`,
         `What is the highest degree or level of school you have completed?`,
         `Marital status`,Occupation,`Is there taboo attach to the use of toilet`) %>% # keep only columns of interest
  tbl_summary(by = `Is there taboo attach to the use of toilet`,                                               # stratify entire table by toilet
              statistic = list( #all_continuous() ~ "{mean} ({sd})",        # stats and format for continuous columns
                all_categorical() ~ "{n} ({p}%)"),   # stats and format for categorical columns
              #digits = all_continuous() ~ 1,                              # rounding for continuous columns
              type   = all_categorical() ~ "categorical",                 # force all categorical levels to display
              label  = list(                                              # display labels for column names
                #Cultural practice``  ~ "Cultural practice`",                           
                Age ~ "Age group",
                Gender    ~ "Gender",
                Settlement      ~ "Settlement",
                Household2  ~ "Household",
                `Average Income/Month` ~ "Average Income/Month",
                `What is the highest degree or level of school you have completed?` ~ "Educational level",
                `Marital status` ~ "Marital status",
                Occupation ~ "Occupation"),
              missing_text = "Missing"                                    # how missing values should display
  ) %>%
  add_p(test = all_categorical() ~ "chisq.test")                                             

### Time to convert our tibble into dataframe so as to use flextable to generate a word document type table
TABOOCHI <- as.data.frame(TABOOCHI)
TABOCHI  <- flextable(TABOOCHI) %>%
  save_as_docx(TABOOCHI, path = "Chi square Taboo .docx")

