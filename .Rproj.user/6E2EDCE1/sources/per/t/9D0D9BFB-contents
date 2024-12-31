pacman::p_load(
  rio,        # importing data  
  here,       # relative file pathways  
  janitor,    # data cleaning and tables
  lubridate,  # working with dates
  matchmaker, # dictionary-based cleaning
  epikit,     # age_categories() function
  tidyverse,   # data management and visualization
  gtsummary,
  flextable,
  skimr,
  rstatix
)

here()
DrI <- import(here("Dataset2.xlsx"))
Drii <- import(here("Dataset2.xlsx"))

### now, lets view the dataset
head(DrI)  # this shows only the first 6 rows by default
head(DrI, 10)   # this shows the firt 10 rows
### Lets do some skimming
skim(DrI)

DrI <- DrI %>%
  mutate(across(.cols = c(Age:PresenceofComplicationse.g.NeuropathyRetinopathy), .fns = as.factor))


skim(DrI)

#### Time to create a befiting table
DrISectA <- DrI %>%
  select(Age:PresenceofComplicationse.g.NeuropathyRetinopathy)%>%
  tbl_summary()

DrISectA <- as.data.frame(DrISectA)

DrISectA <- flextable(DrISectA) %>%
  save_as_docx(DrISectA, path = "Sociodemographic factors.docx")




  

DrIXx <- DrI %>%
  group_by(Gender) %>%
  summarise(across(.cols = c(DOM1, DOM2, DOM3, DOM4), 
                   .fns = list("mean" = mean, "sd" = sd), 
                   na.rm = TRUE))

DrIXxx <- DrI %>%
  group_by(MonthlyIncome) %>%
  summarise(across(.cols = c(DOM1, DOM2, DOM3, DOM4), 
                   .fns = list("mean" = mean, "sd" = sd), 
                   na.rm = TRUE))


DrIXxxx <- DrI %>%
  group_by(LevelofEducation) %>%
  summarise(across(.cols = c(DOM1, DOM2, DOM3, DOM4), 
                   .fns = list("mean" = mean, "sd" = sd), 
                   na.rm = TRUE))


DrIX <- flextable(DrIX) %>%
  save_as_docx(DrIX, path = "Age on QOL.docx")

DrIXx <- flextable(DrIXx) %>%
  save_as_docx(DrIXx, path = "gender on QOL.docx")

DrIXxx <- flextable(DrIXxx) %>%
  save_as_docx(DrIXxx, path = "monthly income on QOL.docx")

DrIXxxx <- flextable(DrIXxxx) %>%
  save_as_docx(DrIXxxx, path = "level of education on QOL.docx")



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
  flextable     # converting tables to HTML
)


#### Summary statstics of the health related quality of life, WHOQOL_BREF
WHOQOL <- DrI %>% 
  get_summary_stats(
    DOM1, DOM2, DOM3, DOM4,  # columns to calculate for
    type = "common")                    # summary stats to return
WHOQOLL  <- flextable(WHOQOL) %>%
  save_as_docx(WHOQOL, path = " Descriptive of WHOQOL.docx")


names(DrI)
DrII <- DrI %>%
  mutate( Howwouldyourateyouroverallhealth = recode(Howwouldyourateyouroverallhealth, "1" = "Very poor", "2" = "Poor", "3" = "Fair",
                    "4" = "Good", "5" = "Very Good"))



DrII %>% 
  count(Howwouldyourateyouroverallhealth) %>%
  mutate(pct = n / sum(n) * 100) %>%  # Calculate percentage
  ggplot() +
  geom_col(mapping = aes(x = fct_relevel(Howwouldyourateyouroverallhealth, c("Very poor", "Poor", "Fair",
                                                                             "Good", "Very Good" )), y = n)) +
  geom_text(aes(x = Howwouldyourateyouroverallhealth, label = paste0(round(pct, 1), "%"),  # Add percentage labels
                y = n), 
            position = position_stack(vjust = 0.5)) + # Position labels in the middle of bars
  labs(
    title = "How would you rate your overall health",
    x = "Overall Health",
    y = "Percentages"
    #color = "Age"
  )

### I want to create a boxplot of multiple columns

boxplot(DrI$DOM1, DrI$DOM2, DrI$DOM3, DrI$DOM4, ylab = "Score", 
        names=c("Physical health", "Psychological", "Social Relationship", "Environmental"),show.names=TRUE,
        col=c('red', 'darkgreen','skyblue', 'purple'))

legend("bottom", inset = c(0.01, 0.01),
       c("VAR","No VAR" ), fill=c("seagreen3","powderblue" ), box.col = "transparent", bg = "transparent", cex=0.8)
stripchart(DrI$DOM1, DrI$DOM2, DrI$DOM3, DrI$DOM4, 
           method = "jitter", 
           vertical = TRUE,
           pch = 1, add = TRUE, seed = 1, width = .3, col = "BLACK")



### Another method

library(ggplot2)

# Assuming your data is in a data frame called `DrI`

pacman::p_load(tidyr)
# Melt the data into a long format
DrI_longe <- tidyr::pivot_longer(Drii, cols = c(DOM1, DOM2, DOM3, DOM4), names_to = "Domain", values_to = "Score") %>%
  # switch order of levels and also relabel them
  mutate(labels=c("DOM1"= "Physical health","DOM2" = "Psychological","DOM3" = "Social Relationship","DOM4" = "Environmental"))

# Create the boxplot with jitter points and statistical summaries
ggplot(DrI_long, aes(x = Domain, y = Score)) +
  geom_boxplot(fill = c("red", "darkgreen", "skyblue", "purple")) +
  geom_jitter(alpha = 0.5) +  # Add jitter points for individual data points
  stat_summary(fun = mean, geom = "point", shape = 18, size = 4, color = "black") +  # Add mean points
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2) +  # Add confidence intervals
  labs(x = "Domain", y = "Score")



### Another method
ggplot(DrI_long, aes(x = Domain, y = Score)) +
  geom_boxplot(fill = c("red", "darkgreen", "skyblue", "purple")) +
  geom_jitter(alpha = 0.5) +
  #stat_summary(fun = mean, geom = "point", shape = 18, size = 4, color = "black") +
  #stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2) +
  geom_text(data = . %>% group_by(Domain) %>% summarise(mean = mean(Score, na.rm = TRUE), sd = sd(Score, na.rm = TRUE)), 
            aes(x = Domain, y = mean, label = paste0("Mean: ", round(mean, 2), "\nSD: ", round(sd, 2))), vjust = -6) +
  labs(x = "Domain", y = "Score")


DrIXY <- DrI %>%
  group_by(Age) %>%
  summarise(across(.cols = c(DOM1, DOM2, DOM3, DOM4),
                   .fns = list(mean = mean, sd = sd),
                   na.rm = TRUE)) %>%
  pivot_longer(cols = -Age, names_to = "variable", values_to = "value") #%>%
  anova_test(value ~ Age)




