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

install.packages("apyramid")     # the installation didnt work with pacman, so i am gonna single it out and install
od_data <- import(here("Dataset1.xlsx"))      # I am importing my dataet

skim(od_data)           # time to have a scope of the dataset

od_data %>% 
  tabyl(Age,Gender)      # i want to explore the age and gender variables

### To draw an age-sex pyramid
apyramid::age_pyramid(data = od_data,
                      age_group = "Age",
                      split_by = "Gender")
#### ???? returns an error, told me age must be a factor variable, so i must convert age to factor

od_data$Age <- as.factor(od_data$Age)
od_data$Gender <- as.factor(od_data$Gender)

#### !!!! lets run our age-sex pyramid again
apyramid::age_pyramid(data = od_data,
                      age_group = "Age",
                      split_by = "Gender")


#### I want to do a more sophisticated age-sex pyramid, showing proportion, colour and extra aesthetics

apyramid::age_pyramid(
  data = od_data,
  age_group = "Age",
  split_by = "Gender",
  proportional = TRUE,              # show percents, not counts
  show_midpoint = FALSE,            # remove bar mid-point line
  #pal = c("orange", "purple")      # can specify alt. colors here (but not labels)
)+                 
  
  # additional ggplot commands
  theme_minimal()+                               # simplfy background
  scale_fill_manual(                             # specify colors AND labels
    values = c("orange", "purple"),              
    labels = c("m" = "Male", "f" = "Female"))+
  labs(y = "Percent of all cases",              # note x and y labs are switched
       x = "Age categories",                          
       fill = "Gender", 
       #caption = "My data source and caption here",
       title = "Age-sex pyramid of the participants",
       subtitle = "Open defecation in rural and urban communities of Northern Nigeria")+
  theme(
    legend.position = "bottom",                          # legend to bottom
    axis.text = element_text(size = 10, face = "bold"),  # fonts/sizes
    axis.title = element_text(size = 12, face = "bold"))


### I need to clean the data


### I want to pull out only section A
sectionA <- od_data%>%
  select(c(Age:`Please specify`))

### to do a sociodemgraphic table

### SECTION A: SOCIODEMOGRAPHIC FACTOR ####
secA<-sectionA %>% 
  select(-c(`Address/ location of the respondent`,`Location GPS Point`, 
            `_Location GPS Point_latitude`,`_Location GPS Point_longitude`,`_Location GPS Point_altitude`,
            `_Location GPS Point_precision`,Ethnicity)) %>%  # keep only the columns of interest
  tbl_summary()    ### creating a table summary

secA <- as.data.frame(secA) #### we must convert the sdf object into data frame for us to use flextable

secAtable  <- flextable(secA) %>%
  save_as_docx(secA, path = "socio demograhic table.docx")




#### now, i am creating a big likert chart
### lemme convert my states to factor
od_data$State <- as.factor(od_data$State)


### We must create a pivot wider object and change the direction of the responses
melted <- od_data %>% 
  pivot_longer(
    cols = `Diarrhoeal disease`:`Anaemia`,
    names_to = "Question",
    values_to = "Response") %>% 
  mutate(
    
    direction = case_when(
      Response %in% c("Disagree","Strongly Disagree", "Undecided")  ~ "Negative",
      Response %in% c("Agree", "Strongly Agree") ~ "Positive",
      TRUE                                 ~ "Unknown"),
    
    State = fct_relevel(State, "Niger", "Plateau", "Gombe", "Kano", "Borno", "Jigawa"),
    
    # must reverse 'Very Poor' and 'Poor' for ordering to work
    Response = fct_relevel(Response, "Strongly Agree", "Agree", "Undecided", "Strongly Disagree", "Disagree")) 

# get largest value for scale limits
melted_max <- melted %>% 
  count(State, Question) %>% # get counts
  pull(n) %>%                 # column 'n'
  max(na.rm=T)                # get max




##### lets make the plot
ggplot()+
  
  # bar graph of the "negative" responses 
  geom_bar(
    data = melted %>% filter(direction == "Negative"),
    mapping = aes(
      x = State,
      y = ..count..*(-1),    # counts inverted to negative
      fill = Response),
    color = "black",
    closed = "left",
    position = "stack")+
  
  # bar graph of the "positive responses
  geom_bar(
    data = melted %>% filter(direction == "Positive"),
    mapping = aes(
      x = State,
      fill = Response),
    colour = "black",
    closed = "left",
    position = "stack")+
  
  # flip the X and Y axes
  coord_flip()+
  
  # Black vertical line at 0
  geom_hline(yintercept = 0, color = "black", size=1)+
  
  # convert labels to all positive numbers
  scale_y_continuous(
    
    # limits of the x-axis scale
    limits = c(-ceiling(melted_max/10)*11,    # seq from neg to pos by 10, edges rounded outward to nearest 5
               ceiling(melted_max/10)*10),   
    
    # values of the x-axis scale
    breaks = seq(from = -ceiling(melted_max/10)*100,
                 to = ceiling(melted_max/10)*100,
                 by = 100),
    
    # labels of the x-axis scale
    labels = abs(unique(c(seq(-ceiling(melted_max/10)*100, 0, 100),
                          seq(0, ceiling(melted_max/10)*100, 100))))) + 

  
  # color scales manually assigned 
  scale_fill_manual(
    values = c("Strongly Agree"  = "green4", # assigns colors
               "Agree"      = "green3",
               "Undecided"      = "yellow",
               "Disagree"       = 'pink',
               "Strongly Disagree" = "red3"),
    breaks = c("Strongly Agree", "Agree", "Undecided", "Disagree", "Strongly Disagree"))+ # orders the legend
  
  
  
  # facet the entire plot so each question is a sub-plot
  facet_wrap( ~ Question, ncol = 3)+
  
  # labels, titles, caption
  labs(
    title = str_glue("Perceived Health Effects of Open Defecation\nn = {nrow(od_data)}"),
    x = "Respondent State",
    y = "Number of responses",
    fill = "")+
  
  # display adjustments 
  theme_minimal()+
  theme(axis.text = element_text(size = 7),
        axis.title = element_text(size = 14, face = "bold"),
        strip.text = element_text(size = 9, face = "bold"),  # facet sub-titles
        plot.title = element_text(size = 20, face = "bold"),
        panel.background = element_rect(fill = NA, color = "black")) # black box around each facet