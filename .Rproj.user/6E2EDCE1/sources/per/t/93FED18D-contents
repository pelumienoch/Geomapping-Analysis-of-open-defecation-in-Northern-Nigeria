pacman::p_load(
  rio,          # File import
  here,         # File locator
  skimr,        # get overview of data
  tidyverse,    # data management + ggplot2 graphics, 
  gtsummary,    # summary statistics and tests
  rstatix,      # statistics
  corrr,        # correlation analayis for numeric variables
  janitor,      # adding totals and percents to tables
  flextable     # converting tables to HTML
)

full <- import(here("FULL.xlsx"))
names(full)

#### I want to create wide .docx table of the descripitve of MCS and PCS grouped by depression
FULL <- full %>%
  group_by(Depression) %>%
  rstatix::get_summary_stats(MCS2, PCS2, type = "common")
FULL  <- flextable(FULL) %>%
  save_as_docx(FULL, path = " depression VS QOL.docx")


#### I want to create a kruskal wallis table of MCS and Depression
MCSDEP <- full %>% 
  select(MCS2, Depression) %>%                       # keep variables of interest
  tbl_summary(                                         # produce summary table
    statistic = MCS2 ~ "{median} ({p25}, {p75})", # specify what statistic to show (default, so could remove)
    by = Depression) %>%                                  # specify the grouping variable
  add_p(MCS2 ~ "kruskal.test")                    # specify what test to perform

MCSDEP <- as.data.frame(MCSDEP)

MCSDEP  <- flextable(MCSDEP) %>%
  save_as_docx(MCSDEP, path = " depression VS MCS.docx")


PCSDEP <- full %>% 
  select(PCS2, Depression) %>%                       # keep variables of interest
  tbl_summary(                                         # produce summary table
    statistic = PCS2 ~ "{median} ({p25}, {p75})", # specify what statistic to show (default, so could remove)
    by = Depression) %>%                                  # specify the grouping variable
  add_p(PCS2 ~ "kruskal.test")                    # specify what test to perform

PCSDEP <- as.data.frame(PCSDEP)

PCSDEP  <- flextable(PCSDEP) %>%
  save_as_docx(PCSDEP, path = " depression VS PCS.docx")





#### I am creating a stacked plots of type of SCD and depression, showing the percentage

full %>% 
  count(typeof, Depression) %>% 
  group_by(Depression) %>%  # Group by outcome to calculate percentages within each outcome
  mutate(pct = n / sum(n) * 100) %>%  # Calculate percentage
  ggplot() +
  geom_col(mapping = aes(x = Depression, fill = typeof, y = n)) +
  geom_text(aes(x = Depression, label = paste0(round(pct, 1), "%"),  # Add percentage labels
                y = n, group = typeof), 
            position = position_stack(vjust = 0.5))  # Position labels in the middle of bars


#### I am creating a bar plot plots of depression, showing the percentage

full %>% 
  count(Depression) %>% 
  mutate(pct = n / sum(n) * 100) %>%  # Calculate percentage
  ggplot() +
  geom_col(mapping = aes(x = Depression, y = n)) +
  geom_text(aes(x = Depression, label = paste0(round(pct, 1), "%"),  # Add percentage labels
                y = n), 
            position = position_stack(vjust = 0.5))  # Position labels in the middle of bars

names(full)


#### Summary statstics of the health related quality of life, SF-36
HRQOL <- full %>% 
  get_summary_stats(
    PF2, RP2, REP2, EF2, EWB2, SF2, PAIN2, GH2, PCS2, MCS2,  # columns to calculate for
    type = "common")                    # summary stats to return
HRQOL  <- flextable(HRQOL) %>%
  save_as_docx(HRQOL, path = " Descriptive of HRQOL.docx")


#### Normality test for the MCS and PCS
Normtest <- full %>% 
  head(500) %>%            # first 500 rows of case linelist, for example only
  shapiro_test(PCS2, MCS2)
Normtest  <- flextable(Normtest) %>%
  save_as_docx(Normtest, path = " Normality of HRQOL.docx")
