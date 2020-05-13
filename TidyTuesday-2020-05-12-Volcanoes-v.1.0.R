# TidyTuesday 2020-05-12
# Volcano dataset

'Weekly objective: create a nice ensemble of plots and combine them using the patchwork package '

# PACKAGES ----------------------------------------------------------------

library(tidyverse)
# To get the maps
library(maps)
# For nice labels over ggplots
library(ggrepel)
# To combine plots into a single graphic
library(patchwork)
# For enhanched texts description
library(ggtext)
# For additional themes
library(hrbrthemes)
# DATA --------------------------------------------------------------------

volcano <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-12/volcano.csv')
# Not used
# eruptions <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-12/eruptions.csv')
# events <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-12/events.csv')
# tree_rings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-12/tree_rings.csv')
# sulfur <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-12/sulfur.csv')

glimpse(volcano)
# glimpse(eruptions)
# glimpse(events)
# glimpse(tree_rings)
# glimpse(sulfur)


# Set of custom colors for plots
cust_col <- c("Russett"="#7A4419", "Maize"="#E7BB41", "Kappel"="#44BBA4", "Onyx" = "#393E41", "Flame"="#E4572E")
# Generate palettes
cust_pal5 <- grDevices::colorRampPalette(cust_col)(5)
cust_pal10 <- grDevices::colorRampPalette(cust_col)(10)

# WRANGLE/EDA -------------------------------------------------------------

#### Italian volcanoes data ####

# Select only Italian volcanoes from dataset
volcano_italy <- volcano %>%
  filter(country == "Italy") %>%
  # Convert col about rocks into factor
  mutate_at(vars(major_rock_1:minor_rock_5), as.factor)

#### Volcanoes map ####

# Map of Italy (retrieved with ggplot2 map_data() and maps package )
map_italy <- map_data("world", region="Italy")
head(map_italy)

# Collate the name of the volcano together with the last eruption year
volcano_italy <- volcano_italy %>%
  mutate(name_n_eruption = glue::glue("{volcano_name}\n({last_eruption_year})")) 

# Plot the map of Italian volcanoes
volcano_italy_mapped <- volcano_italy %>% 
  ggplot() +
  # Design map of Italy
  geom_polygon(data=map_italy,aes(x=long, y=lat, group=group), fill=cust_pal5[2], alpha=0.5) +
  # Add point for each volcano
  geom_point(aes(x=longitude, y=latitude)) +
  # Add names and last eruption date
  geom_text_repel(aes(x=longitude, y=latitude, label=name_n_eruption),
                  # Move text and adjust arrows
                  nudge_y=1, nudge_x=runif(1,0,0.25),
                  # Color text and arrows, set arrows shape
                  colour=cust_pal10[4], segment.color=cust_pal10[4], arrow=arrow(type="closed", angle=10, length=unit(0.20, "inches"))) +
  # Add size of population around 30km
  geom_point(aes(x=longitude, y=latitude, size=population_within_30_km), alpha=0.5, color=cust_pal5[1]) +
  # Override scientific notation for population size, use comma instead
  scale_size_continuous(labels=scales::comma) + #, breaks=seq(0, ceiling(max(volcano_italy$population_within_30_km)), 500000)) 
  # Set theme 
  theme_ft_rc() + # theme from hrbrthemes 
  # Add title andlabels
  labs(title="Map of Italian Volcanoes - Volcano's name (last eruption year)", subtitle="Area around vulcanoes indicates population within 30km",
       x="Longitude", y="Latitude", size="Population within 30km") +
  # Make some adjustments to nicely fit the hrbrthemes with the custom colors chosen
  theme(legend.position="bottom",
        text = element_text(color=cust_pal5[2]),
        plot.subtitle = element_text(color=cust_pal5[2]),
        legend.title = element_text(color=cust_pal5[2]),
        plot.title = element_text(color=cust_pal5[2]))

# Inspect plot
volcano_italy_mapped

#### Volcano maj/min rocks heatmap ####

# Reshape data for heatmap - only MAJOR rock -
# volcano_italy_majrock_heatmap <- volcano_italy %>%
#   # Select var of interest
#   select(volcano_name, major_rock_1:major_rock_5) %>%
#   # Reshape to get the 5 major rocks associated with volcano name
#   pivot_longer(-volcano_name, names_to="rock_n", values_to="major_rock") %>%
#   # Mark NA where any rock is recorded (if it starts with a letter is not NA)
#   # NB: The original dataset marked "no stone" using a whitespace
#   mutate(major_rock = case_when(str_detect(major_rock, "[a-z]") ~ major_rock)) %>%
#   # Mark presence/absence of each rock per volcano
#   mutate(presence = case_when(is.na(major_rock) ~ 0,
#             TRUE ~ 1)) %>%
#   # Convert into factors
#   mutate_at(vars(presence, rock_n, major_rock), as.factor)

# Reshape data for the heatmap - reshape both MAJOR and MINOR rocks in one go -
volcano_italy_majrock_heatmap <- volcano_italy %>%
  # Select only var of interest
  select(volcano_name, major_rock_1:major_rock_5, minor_rock_1:minor_rock_5) %>%
  # Reshape to get the 5 major and 5 minor rocks tied with volcano name
  pivot_longer(-volcano_name, names_to="rock_type", values_to="rock_name") %>%
  # Mark NA where no rock is recorded (note: if it starts with a letter then is not missing)
  # NB: The original dataset marked "no stone" using a whitespace
  mutate(rock_name = case_when(str_detect(rock_name, "[a-z]") ~ rock_name)) %>%
  # Mark presence/absence of each rock per volcano with 0/1
  mutate(presence = case_when(is.na(rock_name) ~ 0,
                              TRUE ~ 1)) %>%
  # Divide rock_type into major and minor
  # NB: The correct way for this would be to classify cases using case_when()
  # but the fast way -in this case- is to trim the string and convert into fact
  mutate(rock_type = str_extract(rock_type, "major|minor")) %>%
  # Convert into factors
  mutate_at(vars(presence, rock_type, rock_name), as.factor) 

# Inspect new dataset
volcano_italy_majrock_heatmap
volcano_italy_majrock_heatmap[is.na(volcano_italy_majrock_heatmap$rock_name),]
summary(volcano_italy_majrock_heatmap)
levels(volcano_italy_majrock_heatmap$rock_name)

# Plot heatmap of major/minor rock types 
rock_heatmap <- volcano_italy_majrock_heatmap %>%
  # Exclude NAs
  filter(!is.na(rock_name)) %>%
  # Plot with volcano name against rock_name, fill by rock_type
  ggplot(aes( x=rock_name, y=fct_rev(volcano_name), fill=rock_type)) +
  # Geom tiles for heatmaps
  geom_tile(width=0.75, height=0.75, show.legend=F) +
  # Fix x:y ratio of plot
  #coord_fixed(ratio=0.5) +
  # Set theme
  theme_ft_rc() +  # theme from hrbrthemes
  # Make some adjustments to nicely fit the hrbrthemes with the custom colors chosen
  theme(axis.text.x = element_text(angle=-45, hjust=0),
        # NB: to use ggtext functions you must specify ggplot texts as element_markdown()
        plot.title = element_markdown(color=cust_pal5[2]),
        text = element_text(color=cust_pal5[2]),
        plot.subtitle = element_text(color=cust_pal5[2]),
        legend.title = element_text(color=cust_pal5[2]))  +
  # Use custom palette colors to fill bars
  scale_fill_manual(values = cust_pal10[c(1,7)]) +
  # Specify title and labels (use ggtext to make a legend in the title)
  labs(title = "**<span style='color:#7A4419'>Major</span>** and
     **<span style='color:#3C6762'>Minor</span>** Rock Types associated with Italian Volcanoes",
     x="", y="") 

# Inspect plot
rock_heatmap

# Check color hastag no for ggtext
cust_pal10[7]; cust_pal10[1] 

##### Elevation plot ####

# Types of volcanoes (in Italy)
levels(factor(volcano_italy$primary_volcano_type))

# Plot volcanoes elevation and types using a bar chart 
volcano_elevation <- volcano_italy %>%
  ggplot(aes(y=fct_rev(volcano_name), x=elevation, fill=primary_volcano_type)) +
  geom_col() +
  # Add a text to specify the type of volcano
  geom_text(inherit.aes=F, aes(y=fct_rev(volcano_name), x=-750, label=primary_volcano_type), colour=cust_pal10[4], hjust=0, size=3) +
  # Use custom palette colors
  scale_fill_manual(values=cust_pal10) +
  # Specify title and labels
  labs(title="Italian Volcanoes Elevation (m) and Type", x="", y="")  +
  theme_ft_rc() + # theme from hrbrthemes
  # Make some adjustments to nicely fit the hrbrthemes with the custom colors chosen
  theme(legend.position="none",
        text = element_text(color=cust_pal5[2]),
        plot.subtitle = element_text(color=cust_pal5[2]),
        legend.title = element_text(color=cust_pal5[2]),
        plot.title = element_text(color=cust_pal5[2])) +
  # Re-scale to give space to fit geom_text with the volcano type and define the breaks
  scale_x_continuous(limits=c(-750, 4000), breaks=seq(0,4000,500))


# Inspect plot
volcano_elevation

# PATCHWORK ---------------------------------------------------------------

# Combine plots together
volcano_italy_patchwork <- volcano_italy_mapped +  rock_heatmap / volcano_elevation

# Save combined plots
ggsave(plot=volcano_italy_patchwork,
       filename="Italian_Volcanoes_patchwork_theme_ft_rc.png",
       dpi=150,
       height=300, width=550, units="mm")
