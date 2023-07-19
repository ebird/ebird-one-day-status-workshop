library(tidyverse)
library(viridis)
library(terra)
library(sf)

checklists <- read_tsv("~/data/ebird/ebd_sampling_relApr-2023/ebd_sampling_relApr-2023.txt.gz",
                       col_select = c("SAMPLING EVENT IDENTIFIER", "OBSERVATION DATE",
                                      "LATITUDE", "LONGITUDE", "COUNTRY CODE",
                                      "DURATION MINUTES")) %>%
  janitor::clean_names()
checklists$year <- year(checklists$observation_date)
checklists$month <- month(checklists$observation_date)
checklists$in_latinamerica <- checklists$country_code %in%
  c("BZ", "GT", "SV", "HN", "NI", "CR", "PA",
    "CO", "VE", "SR", "GY", "GF", "PE", "EC", "BR", "CL", "AR", "UY")

# cumulative effort in years
total_minutes <- sum(checklists$duration_minutes, na.rm = TRUE)
total_minutes / 60 / 24/ 365

# monthly checklists
checklists_per_month_la <- checklists %>%
  filter(year >= 2003, in_latinamerica) %>%
  count(year, month, name = "latin_america")
checklists_per_month <- checklists %>%
  filter(year >= 2003) %>%
  count(year, month, name = "global") %>%
  left_join(checklists_per_month_la, by = c("year", "month")) %>%
  mutate(latin_america = coalesce(latin_america, 0L))
checklists_per_month %>%
  transmute(month = paste0(year, "-",
                           str_pad(month, width = 2, pad = "0"), "-01"),
            global = round(global / 1000),
            latin_america = round(latin_america / 1000)) %>%
  filter(month <= "2023-04-01") %>%
  write_csv("data-raw/monthly-checklists.csv")


# global effort map ----

proj_crs <- "+proj=wag4"
cell_res <- 25000

r <- project(rast(), proj_crs, res  = cell_res)
r_checklists <- checklists %>%
  select(lon = longitude, lat = latitude) %>%
  vect(crs = "epsg:4326") %>%
  project(proj_crs) %>%
  rasterize(r, fun = "sum",
            filename = "data-raw/global-effort_n-checklists.tif",
            gdal = c("COMPRESS=DEFLATE"))

# load map data
f_gpkg <- "data/gis-data.gpkg"
ne_bbox <- read_sf(f_gpkg, "ne_bbox") %>%
  st_transform(crs = proj_crs) %>%
  st_geometry()
ne_graticules <- read_sf(f_gpkg, "ne_graticules") %>%
  st_transform(crs = proj_crs) %>%
  st_geometry()
ne_land <- read_sf(f_gpkg, "ne_land") %>%
  st_transform(crs = proj_crs) %>%
  st_geometry()
ne_country_lines <- read_sf(f_gpkg, "ne_country_lines") %>%
  st_transform(crs = proj_crs) %>%
  st_geometry()

png("images/global-effort_n-checklists.png", width = 2400, height = 1200)
par(mfrow = c(1, 1), mar = c(6, 0, 0, 0), bg = "#ffffff")

# land background
plot(ne_bbox, col = "#ffffff", border = "black", lwd = 3,
     xaxt = "n", yaxt = "n", bty = "n")
plot(ne_graticules, col = "#888888", lwd = 0.5, add = TRUE)
plot(ne_land, col = "#dddddd", border = "#888888", lwd = 1, add = TRUE)

# data
pal <- viridis(25)
mx <- global(r_checklists, max, na.rm = TRUE)$max %>% ceiling()
brks <- 10^seq(0, log10(mx), length.out = length(pal) + 1)
lbl_brks <- seq(0, log10(mx), length.out = 5)
lbl_lbl <- signif(10^lbl_brks, 3) %>%
  round() %>%
  format(big.mark = ",") %>%
  trimws()
plot(r_checklists, col = pal, breaks = brks,
     maxpixels = ncell(r_checklists),
     legend = FALSE, axes = FALSE, add = TRUE)

# lines
plot(ne_country_lines, col = "#ffffff", lwd = 1.5, add = TRUE)
# bounding box
plot(ne_bbox, col = NA, border = "black", lwd = 3, add = TRUE)

# legend
fields::image.plot(zlim = range(lbl_brks), legend.only = TRUE, col = pal,
                   smallplot = c(0.35, 0.65, 0.03, 0.05),
                   horizontal = TRUE,
                   axis.args = list(at = lbl_brks, labels = lbl_lbl,
                                    fg = "black", col.axis = "black",
                                    cex.axis = 2, lwd.ticks = 2),
                   legend.args = list(text = "eBird checklists per 25km grid cell",
                                      side = 3, col = "black",
                                      cex = 3, line = 1))
dev.off()
