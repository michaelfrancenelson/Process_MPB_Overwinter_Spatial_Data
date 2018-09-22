source("Globals.R")

require(sp)
require(rgdal)
require(raster)
require(rgeos)


# US States:  ----------------------------------
us_states = spTransform(readOGR(paste0(spatial_data_input_dir, states_layer), states_layer), proj4_master)
head(us_states@data)
unique(us_states$REGION)

# Include the western 1/3 of SD
ext1 = extent(subset(us_states, STUSPS %in% MPB_states))
ext1@xmax = 0
mpb_states = crop(us_states, ext1)
mpb_states@data = droplevels(mpb_states@data)
rm(ext1, us_states)

# Keep only the needed columns
mpb_states@data = mpb_states@data[, c("STUSPS", "NAME")]
names(mpb_states) = c("abbr", "name")
head(mpb_states)

mpb_usa = SpatialPolygonsDataFrame(gBuffer(mpb_states, width = 0), data = data.frame(1), match.ID = F)
plot(mpb_usa)
# National Forests -----------------------------

# Read in the national forest spatial data, and make sure it is in the right projection
national_forests_polygons = spTransform(readOGR(paste0(spatial_data_input_dir, usfs_layer), usfs_layer), proj4_master)

# Keep only the national forests:
national_forests_polygons = subset(national_forests_polygons, NFSLANDU_1 == "National Forest")

# Keep only relevant columns
national_forests_polygons@data = national_forests_polygons@data[, c("NFFID", "NFSLANDU_2")]
names(national_forests_polygons) = c("code", "name")
head(national_forests_polygons)

# Crop to the states with MPB:
national_forests_in_mpb_states = crop(national_forests_polygons, mpb_states)
# Get rid of the now unused factor levels
national_forests_in_mpb_states@data = droplevels(national_forests_in_mpb_states@data)
head(national_forests_in_mpb_states)

# Create a 1km buffered version (this is kind of slow):
national_forests_in_mpb_states_1km_buffer = gBuffer(national_forests_in_mpb_states, width = 1000, byid = T)

# Save the useful shapes for later reuse

# Polygons of all national forests
writeOGR(
  national_forests_polygons, 
  dsn = paste0(spatial_data_output_dir, "national_forests_polygons"), 
  layer = "national_forests_polygons", driver = "ESRI Shapefile")  

# Polygons of national forests in the states with MPB
writeOGR(
  national_forests_in_mpb_states, 
  dsn = paste0(spatial_data_output_dir, "national_forests_in_mpb_states"), 
  layer = "national_forests_in_mpb_states", driver = "ESRI Shapefile")  

# Buffered polygons of national forests in the states with MPB  
writeOGR(
  national_forests_in_mpb_states_1km_buffer,
  dsn = paste0(spatial_data_output_dir, "national_forests_in_mpb_states_1km_buffer"),
  layer = "national_forests_in_mpb_states_1km_buffer", driver = "ESRI Shapefile")  

# Borders of states with MPB
writeOGR(
  mpb_states,
  dsn = paste0(spatial_data_output_dir, "mpb_states"),
  layer = "mpb_states", driver = "ESRI Shapefile")  
writeOGR(
  mpb_usa,
  dsn = paste0(spatial_data_output_dir, "mpb_usa"),
  layer = "mpb_usa", driver = "ESRI Shapefile")  


# Case Study area polygons ---------------------
national_forests_polygons = readOGR( 
  dsn = paste0(spatial_data_output_dir, "national_forests_polygons"), 
  layer = "national_forests_polygons")

# Combine araphao, white river, and medicine bow into the same study site,
# and simplify the other study forest names:

case_study_site_names = data.frame(
  case_study = c(
  "Colorado", 
  "Colorado",
  "Colorado",
  "Colorado",
  "Beaverhead",
  "Beaverhead",
  "Black Hills",
  "Colville"
), 
forest = c(
  "Roosevelt", 
  "Arapaho", 
  "Medicine Bow", 
  "White River",
  "Beaverhead", 
  "Deerlodge", 
  "Black Hills", 
  "Colville"
), stringsAsFactors = F)


case_study_site_polygons = subset(national_forests_polygons, name %in% paste0(case_study_site_names$forest, " National Forest"))
case_study_site_polygons$forest_name = as.character(case_study_site_polygons$name)
case_study_site_polygons$forest_name = gsub(" National Forest", "", case_study_site_polygons$name)
case_study_site_polygons$forest_num = as.numeric(factor(case_study_site_polygons$name))

case_study_site_names$case_study
match(x = case_study_site_polygons$forest_name, case_study_site_names$forest)
case_study_site_polygons$study_name = case_study_site_names$case_study[match(x = case_study_site_polygons$forest_name, case_study_site_names$forest)]
case_study_site_polygons$study_num = as.numeric(factor(case_study_site_polygons$study_name))


plot(case_study_site_polygons)
sort(national_forests_polygons$name)

writeOGR(
  case_study_site_polygons, 
  dsn = paste0(spatial_data_output_dir, "case_study_site_polygons"), 
  layer = "case_study_site_polygons", driver = "ESRI Shapefile", overwrite = T)  


# studyForestsSPDF = subset(nationalForestsSPDF, FOREST %in% forestNames$oldName)
# studyForestsSPDF$FOREST = mapvalues(studyForestsSPDF$FOREST, from = forestNames$oldName, to = forestNames$newName)
# 

# Pine species polygons -----------------------

load(paste0(spatial_data_input_dir, "western_us_mpb/western_us_mpb.Rd"))
load(paste0(spatial_data_input_dir, "national_forests_mpb/national_forests_all.Rd"))

# For the western US study sites we need the pine ranges.
# Combine ponderosa and contorta
pine_range_polygons = SpatialPolygonsDataFrame(gUnion(
  spTransform(readOGR(dsn = paste0(spatial_data_input_dir, "species_range_maps/Pinus_ponderosa/data/commondata/data0"), "pinupond"), proj4_master),
  spTransform(readOGR(dsn = paste0(spatial_data_input_dir, "species_range_maps/Pinus_contorta/data/commondata/data0"), "pinucont"), proj4_master)),
  data = data.frame(pine = 1))

# Cropped to the western states of interest
pine_range_polygons_mpb_states = crop(pine_range_polygons, western_us_mpb)  

# Cropped to the national forests of interests
pine_range_polygons_national_forests = crop(pine_range_polygons, national_forests_in_mpb_states_1km_buffer)

writeOGR(
  pine_range_polygons,
  dsn = paste0(spatial_data_output_dir, "pine_range_polygons"),
  layer = "pine_range_polygons", driver = "ESRI Shapefile")  

writeOGR(
  pine_range_polygons_mpb_states,
  dsn = paste0(spatial_data_output_dir, "pine_range_polygons_mpb_states"),
  layer = "pine_range_polygons_mpb_states", driver = "ESRI Shapefile")  

writeOGR(
  pine_range_polygons_national_forests,
  dsn = paste0(spatial_data_output_dir, "pine_range_polygons_national_forests"),
  layer = "pine_range_polygons_national_forests", driver = "ESRI Shapefile")  






# Create a spdf with a single feature with the union of all the national forests:
# (similar procedure to below with the buffered pine area kill below)
# 
# require(maptools)
# polys = as(national_forests_mpb_buffered, "SpatialPolygons") 
# length(polys)
# national_forests_all = gUnaryUnion(unionSpatialPolygons(polys, IDs = 1:length(polys)))
# bbb = gUnaryUnion(national_forests_all)
# bbb
# ccc = gIntersection(bbb, national_forests_mpb_buffered)
# 
# aaa
# plot(aaa)
# plot(bbb)
# ?unionSpatialPolygons
# 
# n_polygons = 0
# for(i in 1:length(national_forests_mpb@polygons)){
#   print(i)
#   n_polygons = n_polygons + length(national_forests_mpb@polygons[[i]]@Polygons)
# }
# 
# polygon_list = vector(mode = "list", length = n_polygons)
# 
# index = 1
# str(national_forests_mpb@polygons[[i]]@Polygons, 0)
# str(national_forests_mpb_buffered@polygons[[i]]@Polygons, 0)
# for(i in 1:length(national_forests_mpb@polygons)){
#   for(p in national_forests_mpb@polygons[[i]]@Polygons) {
#     polygon_list[[index]] = p 
#     print(index);index = index + 1
#   }
# }
# 
# # A buffer with a very small width will join adjacent polygons and it's faster tuan gUnaryUnion()
# national_forests_all = gBuffer(SpatialPolygons(list(Polygons(polygon_list, "pike_kill")), proj4string = CRS(proj4_master)), width = 100)
# str(national_forests_all@polygons[[1]]@Polygons, 0)
# str(aaa@polygons[[1]]@Polygons, 0)
# aaa = gUnaryUnion(national_forests_all)  
# 