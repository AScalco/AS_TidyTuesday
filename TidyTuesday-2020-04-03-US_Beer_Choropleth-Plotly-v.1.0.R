##########################
### TidyTuesday 2020.03.31
##########################
### Plotly version #######

# PACKAGES ----------------------------------------------------------------

# For DS
library(tidyverse)
# To make maps and the choropleth
library(plotly)

# DATA RETRIEVE -----------------------------------------------------------

# TidyTuesday (tt) main repo
tt.git.url <- "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020"

# 31st March datasets
#brewing.materials <- read_csv(paste(tt.git.url, "2020-03-31/brewing_materials.csv", sep = "/"))
#beer.taxed <- read_csv(paste(tt.git.url, "2020-03-31/beer_taxed.csv", sep = "/"))
#brewer.size <- read_csv(paste(tt.git.url, "2020-03-31/brewer_size.csv", sep = "/"))
beer.states <- read_csv(paste(tt.git.url, "2020-03-31/beer_states.csv", sep = "/"))


# DATA WRANGLING and EDA ----------------------------------------------------------

#### Inspect data integrity ####
glimpse(beer.states)
head(beer.states,  10)

# Evaluate NAs presence
beer.states %>%
  summarise_each(., function(x) sum(is.na(x)))
# Result: Barrels has 19 NAs
# Inspect the rows with NAs
beer.states[which(is.na(beer.states$barrels)), ]


#### Arrange data ####

## Main data to plot
# Overall barrels production per state per each years considering all 3 types of production/use
beer.states %>%
  filter(state != "total") %>%
  group_by(state, year) %>%
  summarise(barrels.per.state = sum(barrels, na.rm = TRUE)) %>%
  # Add this to summarise the mean production per state over the period 2008-2019  
  group_by(state) %>%
  summarise(mean.barrels = mean(barrels.per.state)) -> df
# Inspect distribution of mean production
hist(df$mean.barrels)

## supplementary (hover) info
beer.states %>%
  filter(state != "total") %>%
  group_by(state, type) %>%
  summarise(mean.barrels = mean(barrels)) -> supplementary.df
# Inspect new df
supplementary.df
# Change the format using "spread()"
supplementary.df.spread <- spread(supplementary.df, key = "type", value = "mean.barrels")
supplementary.df.spread
# Append the info to the df used for the map using inner_join()
df.full <- df %>%
  inner_join(supplementary.df.spread, by = "state")
df.full
# Add info to show when mouse is hover a state
df.full$hover <- with(df.full, paste(state, '<br>',
                                     "Bottles and Cans:", round(`Bottles and Cans`, 2), '<br>',
                                     "Kegs and Barrels:", round(`Kegs and Barrels`, 2), '<br>',
                                     "On Premises:", round(`On Premises`, 2), "<br>"))
# Inspect final df
head(df.full, 20)

  
# PLOTLY ------------------------------------------------------------------

# Specify map projection/options
g <- list(
  scope = 'usa', # Here define what maps to plot
  projection = list(type = 'albers usa'),
  showlakes = TRUE,
  lakecolor = toRGB('white')
)

# Color state boundaries border
l <- list(color = toRGB("black"), width = 2)

# Render the plot
plot_ly(df, z = df.full$mean.barrels, text = df.full$hover, locations = df.full$state, type = "choropleth",
        locationmode = "USA-states", color = df.full$mean.barrels, colors = "Oranges",
        marker = list(line = l), colorbar = list(title = "Mean production")) %>%
  layout(title = "Mean production of barrels of beers by state\nbetween the period 2008-2019\n
         (hover for breakdown by type of production/use",
         
         geo = g,
         width = 800, height = 500 )
