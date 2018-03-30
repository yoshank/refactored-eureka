#=============
# LOAD PACKGES
#=============

library(tidyverse)
library(rvest)
library(stringr)


#=============
# READ WEBPAGE
#=============
html.public_debt_by_country <- read_html('https://en.wikipedia.org/wiki/List_of_countries_by_public_debt')


#==================
# SCRAPE TABLE DATA
#==================
df.public_debt_by_country <- html.public_debt_by_country %>% 
  html_nodes('table') %>% 
  .[[1]] %>% 
  html_table()

# Equivalent of html_table (html_nodes(html.public_debt_by_country, 'table')[[1]])

#===============
# RENAME COLUMNS
#===============
names(df.public_debt_by_country)

colnames(df.public_debt_by_country) <- c('country'
                                         ,'debt_as_pct_of_gdp'
                                         , 'measure_year'
                                         , 'gross_debt_as_pct_of_gdp_IMF'
                                         , 'net_debt_as_pct_of_gdp_IMF'
                                         , 'measure_year_IMF'
                                         , 'region')


#========================================================
# DROP EXTRA COLUMNS
# - The Wikipedia table has a couple of different sources
#   for very similar data.
# - We will drop the IMF data
#========================================================

df.public_debt_by_country %>% 
  select(-gross_debt_as_pct_of_gdp_IMF
         , -net_debt_as_pct_of_gdp_IMF
         , -measure_year_IMF
  ) ->
  df.public_debt_by_country

#=====================================================
# REMOVE RECORD FOR 'World'
# - The original data has a summary record for 'World'
# - we will remove this
#=====================================================
df.public_debt_by_country <- df.public_debt_by_country %>% filter(country != 'World')

#============================
# COERCE data.frame TO tibble
#============================
df.public_debt_by_country <-  df.public_debt_by_country %>% as_tibble()


#=============
# GET MAP DATA
#=============
df.map <- map_data('world')


# INSPECT map data
df.map %>% glimpse()
df.map %>% names()


# INSPECT debt data
df.public_debt_by_country %>% glimpse()

#=============================
# RENAME 'region' to 'country'
#=============================
df.map %>%
  rename(country = region) ->
  df.map

df.map %>% glimpse()

#===============================
# IDENTIFY JOIN MISMATCHES
# - we will use an anti-join to 
#   identify mis-matches between
#   the country variable on our 
#   two different datasets
#===============================
anti_join(df.public_debt_by_country
          ,df.map
          ,by = 'country'
)



Version 0.6-10 of NIMBLE released

Posted: 29 Mar 2018 02:20 PM PDT

(This article was first published on R – NIMBLE, and kindly contributed to R-bloggers)
We’ve released the newest version of NIMBLE on CRAN and on our website. Version 0.6-10 primarily contains updates to the NIMBLE internals that may speed up building and compilation of models and algorithms, as well as a few bug fixes.

Changes include:
  
  some steps of model and algorithm building and compilation are faster;
compiled execution with multivariate distributions or function arguments may be faster;
data can now be provided as a numeric data frame rather than a matrix;
to run WAIC, a user now must set ‘enableWAIC’ to TRUE, either in NIMBLE’s options or as an argument to buildMCMC();
if ‘enableWAIC’ is TRUE, buildMCMC() will now check to make sure that the nodes monitored by the MCMC algorithm will lead to a valid WAIC calculation; and
the use of identityMatrix() is deprecated in favor of diag().
Please see the NEWS file in the installed package for more details

To leave a comment for the author, please follow the link and comment on their blog: R – NIMBLE.
R-bloggers.com offers daily e-mail updates about R news and tutorials on topics such as: Data science, Big Data, R jobs, visualization (ggplot2, Boxplots, maps, animation), programming (RStudio, Sweave, LaTeX, SQL, Eclipse, git, hadoop, Web Scraping) statistics (regression, PCA, time series, trading) and more...

This posting includes an audio/video/photo media file: Download Now

How to map public debt data with ggplot2

Posted: 29 Mar 2018 08:30 AM PDT

(This article was first published on r-bloggers | SHARP SIGHT, and kindly contributed to R-bloggers)



You’ve heard me say it a thousand times: to master data science, you need to practice.

You need to “practice small” by practicing individual techniques and functions. But you also need to “practice big” by working on larger projects.

To get some practice, my recommendation is to find reasonably sized datasets online and plot them.

Wikipedia is a nearly-endless source of good datasets. The great thing about Wikipedia is that many of the datasets are small and well contained. They are also fairly clean, with just enough messiness to make them a bit of a challenge.

As a quick example, this week, we’ll plot some economic data.

Plotting public debt using ggplot2
Here, we’re going to scrape some data from Wikipedia and plot it using ggplot2.

It sounds easy enough, but make note of the complexity here. We’re actually using several dozen techniques in mildly complex ways to get this done.

If you’re a beginner, take a careful look at this. You need to break this down, identify the tools that are being used, and make sure that you know them. (Hint: 80% of this is ggplot2 and dplyr … make sure you master them.)

If you’re at an intermediate level and you now most of the R data science toolkit, take a look at how everything is being put together.

Don’t just run the code. Study it. You’ll become a better data scientist by studying other people’s code.

#=============
# LOAD PACKGES
#=============

library(tidyverse)
library(rvest)
library(stringr)


#=============
# READ WEBPAGE
#=============
html.public_debt_by_country <- read_html('https://en.wikipedia.org/wiki/List_of_countries_by_public_debt')


#==================
# SCRAPE TABLE DATA
#==================
df.public_debt_by_country <- html.public_debt_by_country %>% 
  html_nodes('table') %>% 
  .[[1]] %>% 
  html_table()


#===============
# RENAME COLUMNS
#===============
names(df.public_debt_by_country)

colnames(df.public_debt_by_country) <- c('country'
                                         ,'debt_as_pct_of_gdp'
                                         , 'measure_year'
                                         , 'gross_debt_as_pct_of_gdp_IMF'
                                         , 'net_debt_as_pct_of_gdp_IMF'
                                         , 'measure_year_IMF'
                                         , 'region'
)


#========================================================
# DROP EXTRA COLUMNS
# - The Wikipedia table has a couple of different sources
#   for very similar data.
# - We will drop the IMF data
#========================================================

df.public_debt_by_country %>% 
  select(-gross_debt_as_pct_of_gdp_IMF
         , -net_debt_as_pct_of_gdp_IMF
         , -measure_year_IMF
  ) ->
  df.public_debt_by_country


#=====================================================
# REMOVE RECORD FOR 'World'
# - The original data has a summary record for 'World'
# - we will remove this
#=====================================================
df.public_debt_by_country <- df.public_debt_by_country %>% filter(country != 'World')


#============================
# COERCE data.frame TO tibble
#============================
df.public_debt_by_country <-  df.public_debt_by_country %>% as_tibble()


#=============
# GET MAP DATA
#=============
df.map <- map_data('world')


# INSPECT map data
df.map %>% glimpse()
df.map %>% names()


# INSPECT debt data
df.public_debt_by_country %>% glimpse()


#=============================
# RENAME 'region' to 'country'
#=============================
df.map %>%
  rename(country = region) ->
  df.map


df.map %>% glimpse()


#===============================
# IDENTIFY JOIN MISMATCHES
# - we will use an anti-join to 
#   identify mis-matches between
#   the country variable on our 
#   two different datasets
#===============================
anti_join(df.public_debt_by_country
          ,df.map
          ,by = 'country'
)


# country debt_as_pct_of_gdp measure_year
# 1                Antigua and Barbuda               89.0         2012
# 2                              Burma                 NA           NA
# 3         People's Republic of China               20.1         2016
# 4  Congo, Democratic Republic of the               18.2         2016
# 5             Congo, Republic of the               49.3         2016
# 6                      Cote d'Ivoire               50.9         2016
# 7                        Gambia, The                 NA           NA
# 8                          Gibraltar                7.5         2008
# 9                          Hong Kong               38.4         2016
# 10                      Korea, North                 NA           NA
# 11                      Korea, South               46.1         2016
# 12             Saint Kitts and Nevis               83.0         2013
# 13  Saint Vincent and the Grenadines               67.0         2013
# 14               Trinidad and Tobago               61.0         2016
# 15                            Tuvalu               41.1         2013
# 16                    United Kingdom               92.2         2016
# 17                     United States               73.8         2016
# 18                             World               64.0         2012


#============================================
# GET COUNTRY NAMES FROM df.map
# - these will be the new names that we will
#   use when we re-code the names in 
#   df.public_debt_by_country
#============================================
df.map %>% 
  group_by(country) %>% 
  summarise() %>% 
  print(n = Inf)

# RECODE
df.public_debt_by_country %>% 
  mutate(country = recode(country
                          ,`Antigua and Barbuda` = 'Antigua'
                          ,`Burma` = 'Myanmar'
                          ,`People's Republic of China` = 'China'
                          ,`Congo, Democratic Republic of the` = 'Democratic Republic of the Congo'
                          ,`Congo, Republic of the` = 'Republic of Congo'
                          ,`Cote d'Ivoire` = 'Ivory Coast'
                          ,`Gambia, The` = 'Gambia'
                          #,`Gibraltar` = ''
                          #,`Hong Kong` = ''
                          ,`Korea, North` = 'North Korea'
                          ,`Korea, South` = 'South Korea'
                          ,`Saint Kitts and Nevis` = 'Saint Kitts'
                          ,`Saint Vincent and the Grenadines` = 'Saint Vincent'
                          ,`Trinidad and Tobago` = 'Trinidad'
                          #,`Tuvalu` = ''
                          ,`United Kingdom` = 'UK'
                          ,`United States` = 'USA'
  )
  ) ->
  df.public_debt_by_country

#===========================
# RE-INSPECT JOIN MISMATCHES
# note: these last 3 are OK
#===========================
anti_join(df.public_debt_by_country
          ,df.map
          ,by = 'country'
)

#=====
# JOIN
#=====
df.map_public_debt <- left_join(df.map
                                ,df.public_debt_by_country
                                ,by = 'country'
)


# INSPECT
df.map_public_debt %>% glimpse()


#============================
# PLOT
# - this is just a basic plot
#============================
df.map_public_debt %>% 
  ggplot(aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = debt_as_pct_of_gdp))


#=============
# CREATE THEME
#=============
theme.map <- theme(
  text = element_text(family = 'Helvetica Neue', color = '#444444')
  ,panel.background = element_rect(fill = '#CCCCCC')
  ,plot.background = element_rect(fill = '#CCCCCC')
  ,legend.background = element_rect(fill = '#CCCCCC')
  ,panel.grid = element_blank()
  ,plot.title = element_text(size = 18, face = 'bold')
  ,plot.subtitle = element_text(size = 12)
  ,legend.key = element_blank()
  ,axis.text = element_blank()
  ,axis.ticks = element_blank()
  ,axis.title = element_blank()
)


#========================
# CREATE WORLD PLOT / MAP
#========================
plot.debt_to_gdp_map <- df.map_public_debt %>% 
  ggplot(aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = debt_as_pct_of_gdp)) +
  theme.map +
  labs(title = str_c('Countries in the developed world are'
                     ,'\ncarrying high levels of debt compared to GDP'
  )
  ,fill = str_c('Net public debt','\nas a % of GDP')
  ) +
  scale_fill_gradientn(colors = c('#009933', '#ffff00', 'orange', '#e60000')
                       ,values = scales::rescale(c(30, 50, 70, 100, 200))
  )

#------
# WORLD
#------
plot.debt_to_gdp_map


#-------
# EUROPE
#-------
plot.debt_to_gdp_map + 
  coord_cartesian(xlim = c(-15, 50), ylim = c(30, 75)) +
  labs(title = "European countries have high levels of public debt"
       ,subtitle = str_c('In particular, countries in southern Europe - including Portugal, Italy,'
                         ,'\nGreece, and Spain - have high levels of public debt.'
       )
  )

