pacman::p_load(
  rio,        # importing data  
  here,       # relative file pathways  
  janitor,    # data cleaning and tables
  lubridate,  # working with dates
  matchmaker, # dictionary-based cleaning
  epikit,     # age_categories() function
  tidyverse,   # data management and visualization
  gtsummary
)

here()
SS <- import(here("Dr Ogunnaike excel sheet.xlsx"))

### now, lets view the dataset
head(SS)  # this shows only the first 6 rows by default
head(SS, 10)   # this shows the firt 10 rows
tail(SS)
### lets skim the data, that is, to provide a summary of the dataset
pacman::p_load(skimr)

skimr::skim(SS)

### lets check the names of the column
names(SS)
SS$VAR00001 <- as.factor(SS$VAR00001)

###########################
clean_SS <- SS %>%
  
  # remove column
  select(-c(eduuse,edulogis,VAR00005,
            VAR00006, VAR00007, VAR00008, VAR00010, VAR00011, overall)) %>% 
 
  # de-duplicate
  #distinct() %>%

  mutate(
    VAR00001 = recode(VAR00001, "0" = "10-19years", "1" = "20-29years", "2" = "30-39years", "3" = "40-49years",
                      "4" = "50-59years", "5" = "60-69years"),
    gender = recode(gender, "1" = "Male", "2" = "Female"),
    ethnicity = recode(ethnicity, "1" = "Yoruba", "2" = "Ibo", "3" = "Hausa", "4" = "Others"),
    religion = recode(religion, "1" = "Christianity", "2" = "Islam", "3" = "Traditionalist", "4" = "Others"),
    education = recode(education, "1" = "Primary", "2" = "Secondary", "3" = "Tertiary"),
    eduoffather = recode(eduoffather, "1" = "Primary", "2" = "Secondary", "3" = "Tertiary"),
    eduofmother = recode(eduofmother, "1" = "Primary", "2" = "Secondary", "3" = "Tertiary"),
    marital = recode(marital, "1" = "Single", "2" = "Married",  "3" = "Separated", "4" = "Widowed"),
    familytype = recode(familytype, "1" = "Monogamous", "2" = "Polygamous"),
    source = recode(source, "1" = "self", "2" = "HMO", "3" = "LSHS", "4" = "NHIS"),
    typeof = recode(typeof, "1" = "HbSS", "2" = "HbSC", "3" = "HbSB", "4" = "Others"),
    doyou = recode(doyou, "1" = "Yes", "2" = "No"),
    presence = recode(presence, "1" = "Yes", "2" = "No"),
    presence_A = recode(presence_A, "1" = "Yes", "2" = "No")
    
  
    )
clean_SS$average <- SS$average
table(clean_SS$average, useNA = "always")  # print table of all unique values, including missing 
###

clean_SS <- clean_SS %>% 
  mutate(stroke = 0)

# Convert chronic from NA to 0 
clean_SS <- clean_SS %>% 
  mutate(chronic =replace(chronic, chronic > NA, 0))

clean_SS <- edit(clean_SS)


### I want to pull out only section A
sectionA <- clean_SS%>%
select(c(age: source))
                   
pacman::p_load(
  rio,          # File import
  here,         # File locator
  skimr,        # get overview of data
  tidyverse,    # data management + ggplot2 graphics 
  gtsummary,    # summary statistics and tests
  rstatix,      # summary statistics and statistical tests
  janitor,      # adding totals and percents to tables
  scales,       # easily convert proportions to percents
  flextable,
  apyramid,
  GGally
)

### SECTION A: SOCIODEMOGRAPHIC FACTOR ####
secA<-sectionA %>% 
  select(-c(qualification, occupation, average)) %>%  # keep only the columns of interest
  tbl_summary()    ### creating a table summary

secA <- as.data.frame(secA) #### we must convert the sdf object into data frame for us to use flextable

secAtable  <- flextable(secA) %>%
  save_as_docx(secA, path = "socio demograhic table.docx")

### lets do the table for clinical history

clean_SS <- edit(clean_SS)
clean_SS$ifyes <- as.numeric(clean_SS$ifyes)
clinicalH <- clean_SS %>%
  select(typeof:specify)

skim(clinicalH)
clinicalH <- edit(clinicalH)

CliH<-clinicalH %>% 
  select(typeof: presence) %>%  # keep only the columns of interest
  tbl_summary()    ### creating a table summary

CliH <- as.data.frame(CliH) #### we must convert the sdf object into data frame for us to use flextable

ClinHist  <- flextable(CliH) %>%
  save_as_docx(CliH, path = "Clinical History.docx")


### I wanna create an age-sex pyramid

# begin ggplot
ggplot(mapping = aes(x = age, fill = gender)) +
  
  # female histogram
  geom_histogram(data = clean_SS %>% filter(gender == "Female"),
                 breaks = seq(0,85,5),
                 colour = "white") +
  
  # male histogram (values converted to negative)
  geom_histogram(data = clean_SS %>% filter(gender == "Male"),
                 breaks = seq(0,85,5),
                 mapping = aes(y = ..count..*(-1)),
                 colour = "white") +
  
  # flip the X and Y axes
  coord_flip() +
  
  # adjust counts-axis scale
  scale_y_continuous(limits = c(-20, 40),
                     breaks = seq(-20,40,10),
                     labels = abs(seq(-20, 40, 10)))


## to create a barchart of sickle cell type
ggplot(clinicalH, aes(x = typeof)) +
  geom_bar(fill = "skyblue") +
  labs(title = "Type of sickle cell disease", x = "Sickle cell", y = "Count")


library(dplyr)
library(ggplot2)
library(ggcharts)

sickle <- tibble::tribble(
  ~type, ~percentage,
  "HBSc", 14,
  "HBSS", 86,
)



chart <- sickle %>%
  bar_chart(type, percentage) %>%
  print()

chart +
  geom_text(aes(x = type, y = percentage, label = percentage, vertical = TRUE))

### To plot boxplot of pcv
clinicalH2 <- clinicalH %>%
rename(Type     = typeof,
              PCV =      ifyes)

# A) Boxplot of steady pcv overall
ggplot(data = clinicalH2)+  
  geom_boxplot(mapping = aes(y = PCV))+   # only y axis mapped (not x)
  labs(title = "Boxplot of the steady PCV"
       )

# B) Box plot by group
ggplot(data = clinicalH2, mapping = aes(y = PCV, x = Type, fill = Type)) + 
  geom_boxplot()+                     
  theme(legend.position = "left")+   # remove legend (redundant)
  labs(title = "Boxplot based on the type of sickle cell disease",
       xlab = "Sickle cell disease",
       ylab = "PCV%")    


# A) Boxplot of steady pcv overall
ggplot(data = clinicalH)+  
  geom_boxplot(mapping = aes(y = ifyes))+   # only y axis mapped (not x)
  labs(title = "Boxplot of the steady PCV"
  )

# B) Box plot by group
ggplot(data = clinicalH, mapping = aes(y = ifyes, x = typeof, fill = typeof)) + 
  geom_boxplot()+                     
  theme(legend.position = "left")+   # remove legend (redundant)
  labs(title = "Boxplot based on the type of sickle cell disease",
       xlab = "Sickle cell disease",
       ylab = "PCV%")    

clinicalH2 <- edit(clinicalH2)
clinicalH2$valueofPVC <- as.numeric(clinicalH2$valueofPVC)

CliH2<-clinicalH2 %>% 
  select(Type: presence) %>%  # keep only the columns of interest
  tbl_summary()    ### creating a table summary

CliH2 <- as.data.frame(CliH2) #### we must convert the sdf object into data frame for us to use flextable

ClinHist  <- flextable(CliH2) %>%
  save_as_docx(CliH2, path = "Clinical History2.docx")

### want to perform a combinationa analysis
# Convert chronic from NA to 0 
clinicalH2 <- clinicalH2 %>% 
  mutate(chronic =replace(chronic, chronic > NA, 0),
        (neuropathy =replace(neuropathy, neuropathy > NA, 0))
        
         )
names(clinicalH2)
# create column with the symptoms named, separated by semicolons
c1<- clinicalH2 %>%
  mutate(stroke = ifelse(stroke == "1", "stroke", NA),
         chronic = ifelse(chronic == "1", "chronic", NA),
         neuropathy = ifelse(neuropathy == "1", "neuropathy", NA),
         avascular = ifelse(avascular == "1", "avascular", NA),
         chroniculcer = ifelse(chroniculcer == "1", "chroniculcer", NA))


c1<- c1 %>% 
  unite(col = "Presenceofcomplication",
        c(stroke, chronic, neuropathy, avascular, chroniculcer), 
        sep = "; ",
        remove = TRUE,
        na.rm = TRUE) %>% 
  mutate(
    # make a copy of all_symptoms column, but of class "list" (which is required to use ggupset() in next step)
    Presenceofcomplication_list = as.list(strsplit(Presenceofcomplication, "; "))
  )

pacman::p_load(
  tidyverse,     # data management and visualization
  UpSetR,        # special package for combination plots
  ggupset)       # special package for combination plots



ggplot(
  data = c1,
  mapping = aes(x = Presenceofcomplication_list)) +
  geom_bar() +
  scale_x_upset(
    reverse = FALSE,
    n_intersections = 10,
    sets = c("stroke", "chronic", "neuropathy", "avascular", "chroniculcer"))+
  labs(
    title = "presence of complications",
    subtitle = "The frequent combinations complications among the particpants",
    caption = "Caption here.",
    x = "Sources combination",
    y = "Frequency in dataset")


clinicalH2["chronic"][is.na(clinicalH2["chronic"])] <- 0
clinicalH2["neuropathy"][is.na(clinicalH2["neuropathy"])] <- 0
clinicalH2["avascular"][is.na(clinicalH2["avascular"])] <- 0
clinicalH2["chroniculcer"][is.na(clinicalH2["chroniculcer"])] <- 0

c2<- clinicalH2 %>%
  mutate(stroke = ifelse(stroke == "1", 1, 0),
         chronic = ifelse(chronic == "1", 1, 0),
         neuropathy = ifelse(neuropathy == "1", 1, 0),
         avascular = ifelse(avascular == "1", 1, 0),
         chroniculcer = ifelse(chroniculcer == "1", 1, 0))


c2 %>% 
  UpSetR::upset(
    sets = c("stroke", "chronic", "neuropathy", "avascular", "chroniculcer"),
    order.by = "freq",
    sets.bar.color = c("blue", "red", "yellow", "darkgreen", "orange"), # optional colors
    empty.intersections = "on",
    # nsets = 3,
    nintersects = 5,   # number of bars to plot, in this case, plot the top 10
    number.angles = 0,
    point.size = 3.5,
    line.size = 2, 
    mainbar.y.label = "complication combination",
    sets.x.label = "presence of complications among participants")

export(clean_SS, here("clean_ss.xlsx"))
clean_SS <- edit(clean_SS)

### time to perform the health related quality of life
clean_ss2 <- import(here("SCD dataset.xlsx"))

names(clean_ss2)

# Recode 1 to "A" and 2 to "B" in multiple columns
clean_ss2 <- clean_ss2 %>%
  mutate(across(c(1, 2, 20,22,34,36), ~ recode(.x, "1" = "100", "2" = "75", "3" = "50", "4" = "25", "5" = "0")))

# Recode 1 to "A" and 2 to "B" in multiple columns
clean_ss2 <- clean_ss2 %>%
  mutate(across(c(3, 4, 5,6,7, 8, 9, 10, 11,12 ), ~ recode(.x, "1" = "0", "2" = "50", "3" = "100")))

