pacman::p_load(
  rio,           # to import data
  here,          # to locate files
  tidyverse,     # to clean, handle, and plot the data (includes ggplot2 package)
  sf,            # to manage spatial data using a Simple Feature format
  tmap,          # to produce simple maps, works for both interactive and static maps
  janitor,       # to clean column names
  OpenStreetMap, # to add OSM basemap in ggplot map
  spdep          # spatial statistics
)

od_data <- import(here("Dataset1.xlsx"))      # I am importing my dataset
geo_data <- od_data %>%
  select(c(`Address/ location of the respondent`,`Location GPS Point`, 
                     `_Location GPS Point_latitude`,`_Location GPS Point_longitude`,`_Location GPS Point_altitude`))


# Create sf object
geo_sf <- geo_data %>%
  sf::st_as_sf(coords = c("_Location GPS Point_longitude", "_Location GPS Point_latitude"), crs = 4326)

### I want to import my shapefiles for country and states
Country <- sf::read_sf(here("Nigeria", "Country.shp"))
Country2 <- sf::read_sf(here("Nigeria", "Country.shx"))

State <- sf::read_sf(here("Nigeria", "State.shp"))
State2 <- sf::read_sf(here("Nigeria", "State.shx"))

# State2 level clean to select out some states out of the 36 states
State2 <- State2 %>%
  filter(NAME %in% c("Borno", "Gombe", "Jigawa", "Kano", "Plateau", "Niger")) # filter to keep certain areas

### setting the tmap mode
tmap_mode("view") # choose either "view" or "plot"
### I want to plot the geo points from the geo_sf object i created earlier on the state shapefiles
# Just the cases (points)
tm_shape(geo_sf) + tm_dots(size=0.01, col='blue')

##### I am creating the state shape map
# Just the state boundaries (polygons)
st_crs(State) <- 4326  # Assign WGS84 CRS if missing

st_geometry_type(Country)

# Create the tmap plot


tm_shape(State) +               # State boundaries shapefile
  tm_polygons(col = "#F7F7F7")+    # show polygons in light grey
  tm_borders(col = "#000000",      # show borders with color and line weight
             lwd = 2) +
  tm_text("NAME")            # tHIS is the column that bears the name of the states to display for each polygon


# Same as above, but with zoom from bounding box
tm_shape(State,
         bbox = c(-13.3, 8.43,    # corner
                  -13.2, 8.5)) +  # corner
  tm_polygons(col = "#F7F7F7") +
  tm_borders(col = "#000000", lwd = 2) +
  tm_text("NAME")




# All together for all the 36 states
tm_shape(State) +               # State boundaries shapefile
  tm_polygons(col = "#F7F7F7")+    # show polygons in light grey
  tm_borders(col = "#000000",      # show borders with color and line weight
             lwd = 2) +
  tm_text("NAME")+           # tHIS is the column that bears the name of the states to display for each polygon
  tm_shape(geo_sf) +
  tm_dots(size=0.08, col='blue', alpha = 1.0) +
  tm_layout(title = "Distribution of participants")   # give title to map


# All together for only 6 states
tm_shape(State2) +               # State boundaries shapefile
  tm_polygons(col = "#F7F7F7")+    # show polygons in light grey
  tm_borders(col = "#000000",      # show borders with color and line weight
             lwd = 2) +
  tm_text("NAME")+           # tHIS is the column that bears the name of the states to display for each polygon
  tm_shape(geo_sf) +
  tm_dots(size=0.4, col='red', alpha = 0.5) +
  tm_layout(title = "Distribution of participants")   # give title to map




# I want to try something for the factors that causes open defection, All together for only 6 states

# Create sf object
od_data_sf <- od_data %>%
  sf::st_as_sf(coords = c("_Location GPS Point_longitude", "_Location GPS Point_latitude"), crs = 4326)

# Count cases by health facility
Toilet_sf <- od_data_sf %>%   # begin with linelist including nearest clinic data
  as.data.frame() %>%                # convert from shapefile to dataframe
  count(`Lack of toilet facilities in houses`,              # count rows by "name" (of clinic)
        name = "case_n") %>%         # assign new counts column as "case_n"
  arrange(desc(case_n))              # arrange in descending order

Toilet_sf                         # print to console

print(Country$geometry)
###### Now I want to plot a view of the places where toilet were factors
tmap_mode("view")   # set tmap mode to interactive  

# Closest health facility to each case
od_data_sf <- od_data_sf %>%                  # begin with linelist shapefile  
  st_join(State, join = st_nearest_feature) #%>%   # data from nearest clinic joined to case data 
 # select(case_id, osm_id, name, amenity) %>%       # keep columns of interest, including id, name, type, and geometry of healthcare facility
  #rename("nearest_clinic" = "name")                # re-name for clarity
# plot the cases and clinic points 
tm_shape(od_data_sf) +            # plot cases
  tm_dots(size=0.1,                  # cases colored by nearest clinic
          col="Lack of toilet facilities in houses") +    
  tm_shape(State) + 
                     # plot clinic facilities in large black dots
  tm_dots(size=0.1, col='black', alpha = 0.8) +
  tm_polygons(col = "#F7F7F7")+    # show polygons in light grey
  tm_borders(col = "#000000",      # show borders with color and line weight
             lwd = 2) +
  tm_text("NAME") +                   # overlay with name of facility
  tm_view(set.view = c(7.491302, 9.072264, 6.440416), # adjust zoom (center coords, zoom)
        set.zoom.limits = c(2,9))+
  tm_layout(title = "Cases, colored by nearest clinic")






# Make new dataframe containing counts of cases by state
od_case <- od_data %>%          # begin with data with new case cols
  as_tibble() %>%                      # convert to tibble for better display
  group_by(State, `Lack of toilet facilities in houses` = "Yes") %>% # group by state 
  summarise(cases = n()) %>%           # summarize and count rows
  arrange(desc(cases))                  # arrange in descending order



# Make new dataframe containing counts of cases by State, i need to create a column for total
  od_casee <- od_data %>%
  as_tibble() %>%  # Convert to tibble for better display
  group_by(State, `Lack of toilet facilities in houses`) %>%  # Group by State and Yes/No
  summarise(Cases = n(), .groups = "drop") %>%  # Summarize and count rows
  group_by(State) %>%  # Group again by State to calculate totals
  mutate(Total = sum(Cases)) %>%  # Calculate the total for each State
  ungroup() %>%  # Remove grouping for a clean dataframe
  filter(`Lack of toilet facilities in houses` == "Yes") %>%  # Keep only rows where it's "Yes"
  arrange(desc(Total), State) %>% # Arrange by total cases and then by State

  ungroup() %>%  # Remove grouping for a clean dataframe
  arrange(desc(Total), State)  # Arrange by total cases and then by State

od_casee
nigeria_pop <- import(here("NIG_POP.xlsx"))
# Add population data and calculate cases per 10K population
od_caseee <- od_casee %>% 
  left_join(nigeria_pop,                             # add columns from pop dataset
            by = c("State" = "State")) %>%  # join based on common values across these two columns
  select(names(od_casee), Total) %>%                 # keep only important columns, including total population
  mutate(case_10kpop = round(Cases/Total * 10000, 3)) # make new column with case rate per 10000, rounded to 3 decimals

od_caseee                                                # print to console for viewing


od_caseee3_sf <- od_caseee %>%                 # begin with cases & rate by state
  left_join(od_data_sf, by="State") %>%    # join to shapefile data by common column
  select(State,Cases, Total, case_10kpop,geometry) %>%            # keep only certain columns of interest
        
                                 # keep geometry so polygons can be plotted
 # drop_na(objectid) %>%                       # drop any empty rows
  st_as_sf()                                  # convert to shapefile

State3 <- State2 %>%
  #janitor:: clean_names() %>%
  rename(State = NAME) %>%
  select(-c(Cases))


od_caseee3_sf <- od_caseee %>%                 # begin with cases & rate by state
  left_join(State3, by= "State") %>%    # join to shapefile data by common column
  select(State,Cases,Total, case_10kpop,geometry) %>%            # keep only certain columns of interest
  
  # keep geometry so polygons can be plotted
  # drop_na(objectid) %>%                       # drop any empty rows
  st_as_sf()                                  # convert to shapefile
st_as_sf(od_caseee3_sf)
##### ERROR ALERT, I cant successfully join these two dataframe
### Debugging th eproblem
names(od_caseee)
names(State3)
str(od_caseee)
str(State3)


# Examine the intermediate result after the left_join to see if Cases is presen
temp <- od_caseee %>% left_join(State3, by = "State")
names(temp)

# Adjust pipeline
od_caseee3_sf <- od_caseee %>%
  left_join(State3, by = "State") %>%
  {if ("Cases" %in% names(.)) . else stop("Column 'Cases' is missing")} %>% # Early check for `Cases`
  select(State,Total,case_10kpop,geometry)

## The problem was that, there were two Cases, one from od_caseee and State3, So i have gone back to line 177 to 180 to remove the Cases from States3


# tmap mode
tmap_mode("plot")               # view static map

# plot polygons
tm_shape(od_caseee3_sf) + 
  tm_polygons("Cases") +  # color by number of cases column
  tm_text("State") +  # name display
  tm_layout(title = "Lack of toilet facilities as a factor for Open Defecation")


#### FINALLY, I have solve the problem, we have our geomapping for "Lack of toilet facilities" as a factor for open defecation

### I want to do another factor, poor understanding of effect of open defecation

# Make new dataframe containing counts of cases by State, i need to create a column for total
poor_under <- od_data %>%
  as_tibble() %>%  # Convert to tibble for better display
  group_by(State, `Poor understanding of effect of open defecation`) %>%  # Group by State and Yes/No
  summarise(Cases = n(), .groups = "drop") %>%  # Summarize and count rows
  group_by(State) %>%  # Group again by State to calculate totals
  mutate(Total = sum(Cases)) %>%  # Calculate the total for each State
  ungroup() %>%  # Remove grouping for a clean dataframe
  filter(`Poor understanding of effect of open defecation` == "Yes") %>%  # Keep only rows where it's "Yes"
  arrange(desc(Total), State) %>% # Arrange by total cases and then by State
  
  ungroup() %>%  # Remove grouping for a clean dataframe
  arrange(desc(Total), State)  # Arrange by total cases and then by State


# Add population data and calculate cases per 10K population
poor_underr <- poor_under %>% 
  left_join(nigeria_pop,                             # add columns from pop dataset
            by = c("State" = "State")) %>%  # join based on common values across these two columns
  select(names(poor_under), Total) %>%                 # keep only important columns, including total population
  mutate(case_10kpop = round(Cases/Total * 10000, 3)) # make new column with case rate per 10000, rounded to 3 decimals

poor_underr                                                # print to console for viewing


poor_underr_sf <- poor_underr %>%                 # begin with cases & rate by state
  left_join(State3, by= "State") %>%    # join to shapefile data by common column
  select(State,Cases,Total, case_10kpop,geometry) %>%            # keep only certain columns of interest
  
  # keep geometry so polygons can be plotted
  # drop_na(objectid) %>%                       # drop any empty rows
  st_as_sf()                                  # convert to shapefile

# tmap mode
tmap_mode("plot")               # view static map

# plot polygons
tm_shape(poor_underr_sf) + 
  tm_polygons("Cases") +  # color by number of cases column
  tm_text("State") +  # name display
  tm_layout(title = "Poor understanding of effect of open defecation")



### I want to do another factor, Cultural practices as a factor of open defecation

# Make new dataframe containing counts of cases by State, i need to create a column for total
cultural <- od_data %>%
  as_tibble() %>%  # Convert to tibble for better display
  group_by(State, `Cultural practice`) %>%  # Group by State and Yes/No
  summarise(Cases = n(), .groups = "drop") %>%  # Summarize and count rows
  group_by(State) %>%  # Group again by State to calculate totals
  mutate(Total = sum(Cases)) %>%  # Calculate the total for each State
  ungroup() %>%  # Remove grouping for a clean dataframe
  filter(`Cultural practice` == "Yes") %>%  # Keep only rows where it's "Yes"
  arrange(desc(Total), State) %>% # Arrange by total cases and then by State
  
  ungroup() %>%  # Remove grouping for a clean dataframe
  arrange(desc(Total), State)  # Arrange by total cases and then by State

# Add population data and calculate cases per 10K population
culturall <- cultural %>% 
  left_join(nigeria_pop,                             # add columns from pop dataset
            by = c("State" = "State")) %>%  # join based on common values across these two columns
  select(names(cultural), Total) %>%                 # keep only important columns, including total population
  mutate(case_10kpop = round(Cases/Total * 10000, 3)) # make new column with case rate per 10000, rounded to 3 decimals

culturall                                                # print to console for viewing


culturall_sf <- culturall %>%                 # begin with cases & rate by state
  left_join(State3, by= "State") %>%    # join to shapefile data by common column
  select(State,Cases,Total, case_10kpop,geometry) %>%            # keep only certain columns of interest
  
  # keep geometry so polygons can be plotted
  # drop_na(objectid) %>%                       # drop any empty rows
  st_as_sf()                                  # convert to shapefile

# tmap mode
tmap_mode("plot")               # view static map

# plot polygons
tm_shape(culturall_sf) + 
  tm_polygons("Cases") +  # color by number of cases column
  tm_text("State") +  # name display
  tm_layout(title = "Cultural practice as a factor for open defecation")



### I want to do another factor, Religious practice as a factor of open defecation

# Make new dataframe containing counts of cases by State, i need to create a column for total
Religion <- od_data %>%
  as_tibble() %>%  # Convert to tibble for better display
  group_by(State, `Religious practice`) %>%  # Group by State and Yes/No
  summarise(Cases = n(), .groups = "drop") %>%  # Summarize and count rows
  group_by(State) %>%  # Group again by State to calculate totals
  mutate(Total = sum(Cases)) %>%  # Calculate the total for each State
  ungroup() %>%  # Remove grouping for a clean dataframe
  filter(`Religious practice` == "Yes") %>%  # Keep only rows where it's "Yes"
  arrange(desc(Total), State) %>% # Arrange by total cases and then by State
  
  ungroup() %>%  # Remove grouping for a clean dataframe
  arrange(desc(Total), State)  # Arrange by total cases and then by State

# Add population data and calculate cases per 10K population
Religionn <- Religion %>% 
  left_join(nigeria_pop,                             # add columns from pop dataset
            by = c("State" = "State")) %>%  # join based on common values across these two columns
  select(names(Religion), Total) %>%                 # keep only important columns, including total population
  mutate(case_10kpop = round(Cases/Total * 10000, 3)) # make new column with case rate per 10000, rounded to 3 decimals

Religionn                                                # print to console for viewing


Religionn_sf <- Religionn %>%                 # begin with cases & rate by state
  left_join(State3, by= "State") %>%    # join to shapefile data by common column
  select(State,Cases,Total, case_10kpop,geometry) %>%            # keep only certain columns of interest
  
  # keep geometry so polygons can be plotted
  # drop_na(objectid) %>%                       # drop any empty rows
  st_as_sf()                                  # convert to shapefile

# tmap mode
tmap_mode("plot")               # view static map

# plot polygons
tm_shape(Religionn_sf) + 
  tm_polygons("Cases") +  # color by number of cases column
  tm_text("State") +  # name display
  tm_layout(title = "Religious practice")



### I want to do another factor, Nonchalant attitude as a factor of open defecation

# Make new dataframe containing counts of cases by State, i need to create a column for total
Nonchalant <- od_data %>%
  as_tibble() %>%  # Convert to tibble for better display
  group_by(State, `Nonchalant attitude`) %>%  # Group by State and Yes/No
  summarise(Cases = n(), .groups = "drop") %>%  # Summarize and count rows
  group_by(State) %>%  # Group again by State to calculate totals
  mutate(Total = sum(Cases)) %>%  # Calculate the total for each State
  ungroup() %>%  # Remove grouping for a clean dataframe
  filter(`Nonchalant attitude` == "Yes") %>%  # Keep only rows where it's "Yes"
  arrange(desc(Total), State) %>% # Arrange by total cases and then by State
  
  ungroup() %>%  # Remove grouping for a clean dataframe
  arrange(desc(Total), State)  # Arrange by total cases and then by State

# Add population data and calculate cases per 10K population
Nonchalantt <- Nonchalant %>% 
  left_join(nigeria_pop,                             # add columns from pop dataset
            by = c("State" = "State")) %>%  # join based on common values across these two columns
  select(names(Nonchalant), Total) %>%                 # keep only important columns, including total population
  mutate(case_10kpop = round(Cases/Total * 10000, 3)) # make new column with case rate per 10000, rounded to 3 decimals

Nonchalantt                                                # print to console for viewing


Nonchalantt_sf <- Nonchalantt %>%                 # begin with cases & rate by state
  left_join(State3, by= "State") %>%    # join to shapefile data by common column
  select(State,Cases,Total, case_10kpop,geometry) %>%            # keep only certain columns of interest
  
  # keep geometry so polygons can be plotted
  # drop_na(objectid) %>%                       # drop any empty rows
  st_as_sf()                                  # convert to shapefile

# tmap mode
tmap_mode("plot")               # view static map

# plot polygons
tm_shape(Nonchalantt_sf) + 
  tm_polygons("Cases") +  # color by number of cases column
  tm_text("State") +  # name display
  tm_layout(title = "Nonchalant attitude")


### I want to do another factor, Peer group pressure as a factor of open defecation

# Make new dataframe containing counts of cases by State, i need to create a column for total
Peer <- od_data %>%
  as_tibble() %>%  # Convert to tibble for better display
  group_by(State, `Peer group pressure`) %>%  # Group by State and Yes/No
  summarise(Cases = n(), .groups = "drop") %>%  # Summarize and count rows
  group_by(State) %>%  # Group again by State to calculate totals
  mutate(Total = sum(Cases)) %>%  # Calculate the total for each State
  ungroup() %>%  # Remove grouping for a clean dataframe
  filter(`Peer group pressure` == "Yes") %>%  # Keep only rows where it's "Yes"
  arrange(desc(Total), State) %>% # Arrange by total cases and then by State
  
  ungroup() %>%  # Remove grouping for a clean dataframe
  arrange(desc(Total), State)  # Arrange by total cases and then by State

# Add population data and calculate cases per 10K population
Peerr <- Peer %>% 
  left_join(nigeria_pop,                             # add columns from pop dataset
            by = c("State" = "State")) %>%  # join based on common values across these two columns
  select(names(Peer), Total) %>%                 # keep only important columns, including total population
  mutate(case_10kpop = round(Cases/Total * 10000, 3)) # make new column with case rate per 10000, rounded to 3 decimals

Peerr                                                # print to console for viewing


Peerr_sf <- Peerr %>%                 # begin with cases & rate by state
  left_join(State3, by= "State") %>%    # join to shapefile data by common column
  select(State,Cases,Total, case_10kpop,geometry) %>%            # keep only certain columns of interest
  
  # keep geometry so polygons can be plotted
  # drop_na(objectid) %>%                       # drop any empty rows
  st_as_sf()                                  # convert to shapefile

# tmap mode
tmap_mode("plot")               # view static map

# plot polygons
tm_shape(Peerr_sf) + 
  tm_polygons("Cases") +  # color by number of cases column
  tm_text("State") +  # name display
  tm_layout(title = "Peer group pressure")


### I want to do another factor, taboo attach to the use of toilet as a factor of open defecation

# Make new dataframe containing counts of cases by State, i need to create a column for total
Taboo <- od_data %>%
  as_tibble() %>%  # Convert to tibble for better display
  group_by(State, `Is there taboo attach to the use of toilet`) %>%  # Group by State and Yes/No
  summarise(Cases = n(), .groups = "drop") %>%  # Summarize and count rows
  group_by(State) %>%  # Group again by State to calculate totals
  mutate(Total = sum(Cases)) %>%  # Calculate the total for each State
  ungroup() %>%  # Remove grouping for a clean dataframe
  filter(`Is there taboo attach to the use of toilet` == "Yes") %>%  # Keep only rows where it's "Yes"
  arrange(desc(Total), State) %>% # Arrange by total cases and then by State
  
  ungroup() %>%  # Remove grouping for a clean dataframe
  arrange(desc(Total), State)  # Arrange by total cases and then by State

# Add population data and calculate cases per 10K population
Tabooo <- Taboo %>% 
  left_join(nigeria_pop,                             # add columns from pop dataset
            by = c("State" = "State")) %>%  # join based on common values across these two columns
  select(names(Taboo), Total) %>%                 # keep only important columns, including total population
  mutate(case_10kpop = round(Cases/Total * 10000, 3)) # make new column with case rate per 10000, rounded to 3 decimals

Tabooo                                               # print to console for viewing


Tabooo_sf <- Tabooo %>%                 # begin with cases & rate by state
  left_join(State3, by= "State") %>%    # join to shapefile data by common column
  select(State,Cases,Total, case_10kpop,geometry) %>%            # keep only certain columns of interest
  
  # keep geometry so polygons can be plotted
  # drop_na(objectid) %>%                       # drop any empty rows
  st_as_sf()                                  # convert to shapefile

# tmap mode
tmap_mode("plot")               # view static map

# plot polygons
tm_shape(Tabooo_sf) + 
  tm_polygons("Cases") +  # color by number of cases column
  tm_text("State") +  # name display
  tm_layout(title = "Taboo attach to the use of toilet")

