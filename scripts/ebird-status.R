library(dplyr)
library(ebirdst)
library(exactextractr)
library(forcats)
library(ggplot2)
library(readr)
library(sf)
library(stringr)
library(terra)
library(tidyr)

# get an ebirdst access key https://ebird.org/st/request
# store it using set_ebirdst_access_key("XXXXXXXXX")
# run the following to test the access key
ebirdst_download_status("scamyz1", pattern = "abundance_median_27km",
                        force = TRUE)

# Status species ---

# explore ebirdst_runs


# Downloading data ---

# look at the files in a dry run

# download all data for Scarlet Myzomela

# confirm pre-downloaded data package


# Applications: trajectories ---

# load proportion of population cubes

# load Queensland boundary
qld <- read_sf("data/gis-data.gpkg", "regions") %>%
  filter(state_code == "AU-QLD") %>%
  st_transform(crs = crs(pop1))

# summarize with Queensland

# combine

# plot
ggplot(trajectories, aes(x = week, y = prop_pop, color = species)) +
  geom_line() +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Week",
       y = "% of population",
       title = "Weekly proportion of population trajectory",
       color = NULL) +
  theme_bw() +
  theme(legend.position = "bottom")

# Applications: Regional statistics ---

# load species names and combine

# extract seasonal proportion of population

# load protected areas data and project
protected <- read_sf("data/capad2022.gpkg") %>%
  filter(STATE == "QLD") %>%
  st_combine() %>%
  st_transform(crs = crs(prop_population))

# extract proportion of popultation within protected areas

# plot
ggplot(percent_protected) +
  aes(x = fct_reorder(common_name, percent_population),
      y = percent_population) +
  geom_col(aes(fill = percent_population)) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_viridis_b() +
  labs(x = NULL, y = "Percent of population in protected areas") +
  coord_flip() +
  theme_bw() +
  theme(legend.position = "none")

# Applications: Prioritization ---

# calculate mean proportion of population

# plot
par(mar = c(0.25, 0.25, 0.25, 0.25))
crs <- "+proj=laea +lon_0=146.95 +lat_0=-19.15 +datum=WGS84 +units=m +no_defs"
r_plot <- sqrt(importance) %>%
  project(crs, method = "near") %>%
  trim()
plot(r_plot, axes = FALSE)

# calculate proportion protected

# identify selected sites

# plot protected areas and selected sites
par(mar = c(0.25, 0.25, 0.25, 0.25))

# high importance
r_plot <- project(selected_sites, crs, method = "near") %>%
  trim()
protected_proj <- st_transform(protected_qld, crs = crs) %>%
  st_geometry()
plot(r_plot, axes = FALSE, legend = FALSE)

# existing
plot(st_simplify(protected_proj),
     col = "#00000088", border = NA,
     add = TRUE)

# calc percent protected by selected sites

# plot the data
ggplot(comparison) +
  aes(x = fct_reorder(common_name, percent_population),
      y = percent_population,
      group = network_type,
      fill = network_type) +
  geom_col(position = "dodge") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_brewer(palette = "Set1") +
  labs(x = NULL,
       y = "Percent of population in protected areas",
       fill = NULL) +
  coord_flip() +
  theme(legend.position = "bottom")
