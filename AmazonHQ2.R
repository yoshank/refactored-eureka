#==============
# LOAD PACKAGES
#==============

library(rvest)
library(tidyverse)
library(stringr)
library(ggmap)


#=======
# SCRAPE
#=======
html.amz_cities <- read_html("https://www.cbsnews.com/news/amazons-hq2-cities-second-headquarters-these-cities-are-contenders/")


df.amz_cities <- html.amz_cities %>%
  html_nodes("table") %>%
  .[[1]] %>% 
  html_table()


# inspect
df.amz_cities %>% head()

#====================
# CHANGE COLUMN NAMES
#====================

# inspect initial column names
colnames(df.amz_cities)

# assign new column names
colnames(df.amz_cities) <- c("metro_area", 'state', 'population_tot', 'bachelors_degree_pct')

# inspect
df.amz_cities %>% head()

#==============================================
# REMOVE FIRST ROW
# - when we scraped the data, the column names
#   on the table were read in as the first row
#   of data.
# - Therefore, we need to remove the first row
#==============================================

df.amz_cities <- df.amz_cities %>% filter(row_number() != 1)


#===================================================================================
# MODIFY VARIABLES
# - both bachelors_degree_pct and population_tot were scraped as character variables
#    but we need them in numeric format
# - we will use techniques to parse/coerce these variable from char to numeric
#===================================================================================

#--------------------------------
# PARSE AS NUMBER: population_tot
#--------------------------------

df.amz_cities <- mutate(df.amz_cities, population_tot = parse_number(population_tot))


# check
typeof(df.amz_cities$population_tot)

# inspect
df.amz_cities %>% head()


#-----------------------------
# COERCE: bachelors_degree_pct
#-----------------------------

df.amz_cities <- mutate(df.amz_cities, bachelors_degree_pct = as.numeric(bachelors_degree_pct))

#=============================================================
# CREATE VARIABLE: city
# - here, we're using the stringr function str_extract() to
#   extract the primary city name from the metro_area variable
# - to do this, we're using a regex to pull out the city name
#   prior to the first '-' character
#=============================================================

df.amz_cities <- df.amz_cities %>% mutate(city = str_extract(metro_area, "^[^-]*"))

#=========================================
# GEOCODE
# - here, we're getting the lat/long data
#=========================================

data.geo <- geocode(df.amz_cities$city)

#inspect

data.geo %>% head()
data.geo

#========================================
# RECOMBINE: merge geo data to data frame
#========================================

df.amz_cities <- cbind(df.amz_cities, data.geo)
df.amz_cities

#==============================================================
# RENAME VARIABLE: lon -> long
# - we'll rename lon to lon, just because 'long' is consistent
#   with the name for longitude in other data sources
#   that we will use
#==============================================================

df.amz_cities <- rename(df.amz_cities, long = lon)


# get column names names
df.amz_cities %>% names()


# inspect

df.amz_cities %>% head()





#================================================
# GET USA MAP
# - this is the map of the USA states, upon which
#   we will plot our city data points
#================================================

map.states <- map_data("state")

#====================================
# PLOT
# - here, we're actually creating the 
#   data visualizations with ggplot()
#====================================


#------------------------------------------------
# FIRST ITERATION
# - this is just a 'first pass' to check that
#   everything looks good before we take the time
#   to format it
#------------------------------------------------
ggplot() +
  geom_polygon(data = map.states, aes(x = long, y = lat, group = group)) +
  geom_point(data = df.amz_cities, aes(x = long, y = lat, size = population_tot, color = bachelors_degree_pct))



#--------------------------------------------------
# FINALIZED VERSION (FORMATTED)
# - this is the 'finalized' version with all of the
#   detailed formatting
#--------------------------------------------------

ggplot() +
  geom_polygon(data = map.states, aes(x = long, y = lat, group = group)) +
  geom_point(data = df.amz_cities, aes(x = long, y = lat, size = population_tot, color = bachelors_degree_pct*.01), alpha = .5) +
  geom_point(data = df.amz_cities, aes(x = long, y = lat, size = population_tot, color = bachelors_degree_pct*.01), shape = 1) +
  coord_map(projection = "albers", lat0 = 30, lat1 = 40, xlim = c(-121,-73), ylim = c(25,51)) +
  scale_color_gradient2(low = "red", mid = "yellow", high = "green", midpoint = .41, labels = scales::percent_format()) +
  scale_size_continuous(range = c(.9, 11),  breaks = c(2000000, 10000000, 20000000),labels = scales::comma_format()) +
  guides(color = guide_legend(reverse = T, override.aes = list(alpha = 1, size = 4) )) +
  labs(color = "Bachelor's Degree\nPercent"
       ,size = "Total Population\n(metro area)"
       ,title = "Possible cities for new Amazon Headquarters"
       ,subtitle = "Based on population & percent of people with college degrees") +
  theme(text = element_text(colour = "#444444", family = "Gill Sans")
        ,panel.background = element_blank()
        ,axis.title = element_blank()
        ,axis.ticks = element_blank()
        ,axis.text = element_blank()
        ,plot.title = element_text(size = 28)
        ,plot.subtitle = element_text(size = 12)
        ,legend.key = element_rect(fill = "white")
  ) 

