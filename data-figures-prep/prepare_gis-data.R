library(dplyr)
library(rnaturalearth)
library(sf)
library(wdpar)
sf_use_s2(FALSE)

# file to save spatial data
gpkg_file <- "data/gis-data.gpkg"
dir.create(dirname(gpkg_file), showWarnings = FALSE, recursive = TRUE)

# political boundaries
# land border with lakes removed
ne_land <- ne_download(scale = 50, category = "cultural",
                       type = "admin_0_countries_lakes",
                       returnclass = "sf") %>%
  st_set_precision(1e6) %>%
  st_union() %>%
  st_make_valid()
# bounding box
ne_bbox <- ne_download(scale = 50, category = "physical",
                       type = "wgs84_bounding_box",
                       returnclass = "sf") %>%
  st_make_valid()
# 15 degree graticules
ne_graticules <- ne_download(scale = 50, category = "physical",
                             type = "graticules_15",
                             returnclass = "sf") %>%
  st_make_valid()
# country lines
ne_country_lines <- ne_download(scale = 50, category = "cultural",
                                type = "admin_0_boundary_lines_land",
                                returnclass = "sf") %>%
  st_geometry()
ne_states <- ne_download(scale = 10, category = "cultural",
                         type = "admin_1_states_provinces",
                         returnclass = "sf") %>%
  filter(iso_a2 %in% c("CL", "BR")) %>%
  select(country = admin, country_code = iso_a2,
         state = name, state_code = iso_3166_2) %>%
  st_make_valid()
# chile proteced areas
protected_areas <- read_sf("data-raw/chile-protected-areas/snaspe.shp") %>%
  st_transform(crs = 4326) %>%
  st_make_valid() %>%
  select(objectid, nombre = Nombre, tipo_snasp = Tipo_Snasp,
         region = Region, cod_region = Cod_Region)

# save all layers to a geopackage
unlink(gpkg_file)
write_sf(ne_land, gpkg_file, "ne_land")
write_sf(ne_bbox, gpkg_file, "ne_bbox")
write_sf(ne_graticules, gpkg_file, "ne_graticules")
write_sf(ne_states, gpkg_file, "ne_states")
write_sf(ne_country_lines, gpkg_file, "ne_country_lines")
write_sf(protected_areas, gpkg_file, "protected_areas")
