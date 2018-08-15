source("Globals.R")
require(sp)
require(raster)
require(rgdal)
require(rgeos)
require(data.table)

# read data ---------------
# Study area to crop:
study_area = readOGR(paste0(spatial_data_output_dir, "mpb_states"), "mpb_states")

daymet_output_dir = "E:/Data/Daymet/daymet_output/"

daymet_files = paste0(daymet_output_dir, list.files(daymet_output_dir))

tiles = list.files(paste0(daymet_dir, "Tiles"))

# # Make sure all the tile files for every year are present --------------
# 
# tile_dirs = paste0(daymet_dir, "Tiles/", tiles, "/")
# 
# tmax_errors = c()
# tmin_errors = c()
# 
# tmax_tile_miss = c()
# tmin_tile_miss = c()
# tmax_year_miss = c()
# tmin_year_miss = c()
# 
# for (i in 1:length(tiles))
# {
#   tile = tiles[i]
#   
#   tiles_i = list.files(tile_dirs[i])
#   
#   
#   
#   tmax_i = tiles_i[grepl("tmax", tiles_i)]
#   tmin_i = tiles_i[grepl("tmin", tiles_i)]
#   
#   tmax_files_i = paste0(daymet_dir, "Tiles/", tile, "/", tmax_i)
#   tmin_files_i = paste0(daymet_dir, "Tiles/", tile, "/", tmin_i)
#   
#   years_tmax_i = as.numeric(sapply(tmax_i, function(x) strsplit(x, split = "_")[[1]][2]))
#   years_tmin_i = as.numeric(sapply(tmin_i, function(x) strsplit(x, split = "_")[[1]][2]))
#   
#   for (year in daymet_years)
#   {
#     if (!(year %in% years_tmax_i)){
#       print(paste0("tile ", tile, " tmax missing year: ", year))
#      tmax_tile_miss = c(tmax_tile_miss, tile)
#      tmax_year_miss = c(tmax_year_miss, year)
#     }
#   }
#   
#   for (year in daymet_years)
#   {
#     if (!(year %in% years_tmin_i))
#     {
#       print(paste0("tile ", tile, " tmin missing year: ", year))
#      tmin_tile_miss = c(tmin_tile_miss, tile)
#      tmin_year_miss = c(tmin_year_miss, year)
#     }
#   }
#   
#   j = 1
#   tmax_size = file.size(tmax_files_i[j])
#   tmin_size = file.size(tmin_files_i[j])
#   for (j in 1:length(tmax_files_i))
#   {
#     max_j = file.size(tmax_files_i[j])
#     min_j = file.size(tmin_files_i[j])
#     
#     max_dif = tmax_size - max_j
#     min_dif = tmin_size - min_j
#     
#     
#     if(!is.na(max_dif)) if(abs(max_dif) > 76)
#     {
#       print(tmax_files_i[j])
#       print(max_dif)
#       tmax_errors = c(tmax_errors, tmax_files_i[j])
#     }
#     
#     if (!is.na(min_dif)) if(abs(min_dif) > 76)
#     {
#       tmin_errors = c(tmin_errors, tmin_files_i[j])
#       print(tmin_files_i[j])
#       print(min_dif)
#     }
#   }
#   
# }
# tmin_errors
# tmax_errors
# tmax_tile_miss
# tmax_year_miss
# cbind(tmax_tile_miss, tmax_year_miss)
# cbind(tmin_tile_miss, tmin_year_miss)




# Create list of data tables from rasters ----------------------

n_rows = 0
n_tiles = 0
ext_sa = as(extent(study_area), "SpatialPolygons")
proj4string(ext_sa) = proj4_master
proj4string(study_area)

plot(ext_sa)
plot(study_area, add = T)

surv_tables = vector(mode = "list", length = length(daymet_files))

which(tiles == 12452)
i = 130
for (i in 1:length(tiles))
{
  surv_i = brick(daymet_files[i])
  
  # plot(subset(surv_i, 1))
  
  ext_i = as(extent(surv_i), "SpatialPolygons")
  proj4string(ext_i) = proj4_master
  
  if (!is.null(gIntersection(ext_sa, ext_i)))
  {
    print(paste0(i, ": ", daymet_files[i]))
    plot(ext_i, add = T, col = rgb(0.1, 0.1, 1, 0.5))
    
    msk = (rasterize(study_area, subset(surv_i, 1)))
    
    r_to_p = data.table(rasterToPoints(mask(surv_i, msk)))
    r_to_p = r_to_p[complete.cases(r_to_p), ]
    
    # ggplot(r_to_p, aes(x, y, color = X1981)) + geom_point()
    
    
    names(r_to_p) = c("x", "y", paste0("winter_ending_", daymet_years + 1))
    r_to_p[ , tile := tiles[i]]
    
    r_to_p
    
    n_rows = n_rows + nrow(r_to_p)
    n_tiles = n_tiles + 1
    
    
    surv_tables[[n_tiles]] = r_to_p
  } else
    plot(ext_i, add = T, col = rgb(1, 0.1, 0.2, 0.5))
}




# Create a master data table --------------
surv_dt = data.table(matrix(0, nrow = n_rows, ncol = ncol(r_to_p)))
names(surv_dt) = names(r_to_p)
surv_dt

index = 1

for (i in 1:n_tiles)
{
  
  if (nrow(surv_tables[[i]]) > 0)
  {
    index_next = index + nrow(surv_tables[[i]]) - 1
    print(i)
    surv_dt[index:index_next, ] = surv_tables[[i]]
    index = index_next + 1
  }
}

sub = surv_dt[sample(1:nrow(surv_dt), 25000)]
ggplot(sub, aes(x, y, color = winter_ending_1981)) + geom_point()



# Get rid of the second to last columns, since they have no data
surv_dt = surv_dt[, -which(grepl("2017", names(surv_dt))), with = F]
fwrite(surv_dt, paste0(spatial_data_output_dir, "modeled_survival_daymet_data_table.csv"))


# Create bricks for winter and annual min temps ----------------
pine_mask = raster(paste0(spatial_data_output_dir, "pine_mask.nc"), crs = proj4_master)

# Make a raster stack with the same extent and resolution as the tree kill stack
neighbor_points = nabor::knn(
  surv_dt[, .(x, y)],
  coordinates(pine_mask),
  k = 1
)

# The pine kill rasters are only the US, so there will be a lot of points in the SPDF that fall within Canada, or over the ocean.
exclude_ids = which(neighbor_points$nn.dists > 1000)
pine_mask_neighbor_data_table_rows = data.table(neighbor_points$nn.idx)
names(pine_mask_neighbor_data_table_rows) = "row_id"


templateR = raster(pine_mask)

surv_brick = brick()
for(i in head(daymet_years, -1)){
  # for(i in head(daymet_years, -30)){
  layer_surv = templateR
  print(i)
  
  layer_surv[] =
    surv_dt[pine_mask_neighbor_data_table_rows$row_id, get(paste0("winter_ending_", i + 1))]
  layer_surv[exclude_ids] = NA
  
  # plot(layer_surv)
  
  surv_brick = addLayer(surv_brick, layer_surv)
}

writeRaster(surv_brick, paste0(spatial_data_output_dir, "mpb_overwinter_survival_brick.nc"), format = "CDF", overwrite = T)
plot(subset(surv_brick, 2))

