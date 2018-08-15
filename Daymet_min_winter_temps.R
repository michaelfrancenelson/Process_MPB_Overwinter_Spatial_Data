source("Globals.R")
require(raster)
require(rgdal)
require(rgeos)
require(data.table)

# read data ---------------
# Study area to crop:
study_area = readOGR(paste0(spatial_data_output_dir, "mpb_states"), "mpb_states")

winter_min_dir = "E:/Data/Daymet/winter_tmin_java/"
annual_min_dir = "E:/Data/Daymet/annual_tmin_java/"

winter_min_files = paste0(winter_min_dir, list.files(winter_min_dir))
annual_min_files = paste0(winter_min_dir, list.files(winter_min_dir))

tiles = list.files(paste0(daymet_dir, "Tiles"))

# Make sure all the tile files for every year are present --------------

for (i in 1:length(tiles))
{
  tile = tiles[i]
  tile_dirs = paste0(daymet_dir, "Tiles/", tiles, "/")
  
  tiles_i = list.files(tile_dirs[i])
  
  tmax_i = tiles_i[grepl("tmax", tiles_i)]
  tmin_i = tiles_i[grepl("tmax", tiles_i)]
  
  years_tmax_i = as.numeric(sapply(tmax_i, function(x) strsplit(x, split = "_")[[1]][2]))
  years_tmin_i = as.numeric(sapply(tmin_i, function(x) strsplit(x, split = "_")[[1]][2]))
  
  for (year in daymet_years)
  {
    if (!(year %in% years_tmax_i))
      print(paste0("tile ", tile, " tmax missing year: ", year))
  }
  
  for (year in daymet_years)
  {
    if (!(year %in% years_tmin_i))
      print(paste0("tile ", tile, " tmax missing year: ", year))
  }
  
}





# Create list of data tables from rasters ----------------------

n_rows = 0
n_tiles = 0
ext_sa = as(extent(study_area), "SpatialPolygons")
proj4string(ext_sa) = proj4string(study_area)
plot(ext_sa)
plot(study_area, add = T)

annual_min_tables = winter_min_tables = vector(mode = "list", length = length(winter_min_files))

for (i in 1:length(winter_min_files))
  # for (i in 1:10)
{
  winter_min_i = brick(winter_min_files[i])
  annual_min_i = brick(annual_min_files[i])
  
  ext_i = as(extent(winter_min_i), "SpatialPolygons")
  proj4string(ext_i) = proj4string(study_area)
  
  if (!is.null(gIntersection(ext_sa, ext_i)))
  {
    print(paste0(i, ": ", winter_min_files[i]))
    plot(ext_i, add = T, col = rgb(0.1, 0.1, 1, 0.5))
    
    msk = (rasterize(study_area, subset(winter_min_i, 1)))
    
    r_to_p = data.table(rasterToPoints(mask(winter_min_i, msk)))
    r_to_p = r_to_p[complete.cases(r_to_p), ]
    
    r_to_p_a = data.table(rasterToPoints(mask(annual_min_i, msk)))
    r_to_p_a = r_to_p_a[complete.cases(r_to_p_a), ]
    
    n_rows = n_rows + nrow(r_to_p_a)
    n_tiles = n_tiles + 1
    
    names(r_to_p) = c("x", "y", paste0("winter_ending_", daymet_years + 1))
    names(r_to_p_a) = c("x", "y", paste0("year_", daymet_years))
    
    r_to_p[ , tile := tiles[i]]
    r_to_p_a[ , tile := tiles[i]]
    
    
    winter_min_tables[[n_tiles]] = r_to_p
    annual_min_tables[[n_tiles]] = r_to_p_a
  } else
    plot(ext_i, add = T, col = rgb(1, 0.1, 0.2, 0.5))
}


# Create  mmaster data tables --------------
annual_min_dt = winter_min_dt = data.table(matrix(0, nrow = n_rows, ncol = ncol(r_to_p)))
names(winter_min_dt) = names(r_to_p)
names(annual_min_dt) = names(r_to_p_a)
winter_min_dt

index = 1
#  This avoids warnings in the loop below
winter_min_dt[, tile := as.character(tile)]
annual_min_dt[, tile := as.character(tile)]

for (i in 1:n_tiles)
{
  
  if (nrow(winter_min_tables[[i]]) > 0)
  {
    index_next = index + nrow(winter_min_tables[[i]]) - 1
    print(i)
    winter_min_dt[index:index_next, ] = winter_min_tables[[i]]
    annual_min_dt[index:index_next, ] = annual_min_tables[[i]]
    index = index_next + 1
  }
}

# Get rid of the second to last columns, since they have no data
winter_min_dt = winter_min_dt[, -which(grepl("2017", names(winter_min_dt))), with = F]
annual_min_dt = annual_min_dt[, -which(grepl("2016", names(annual_min_dt))), with = F]
annual_min_dt


fwrite(winter_min_dt, paste0(spatial_data_output_dir, "winter_min_daymet_data_table.csv"))
fwrite(annual_min_dt, paste0(spatial_data_output_dir, "annual_min_daymet_data_table.csv"))


# Create bricks for winter and annual min temps ----------------

winter_min_dt = fread(paste0(spatial_data_output_dir, "winter_min_daymet_data_table.csv"))
annual_min_dt = fread(paste0(spatial_data_output_dir, "annual_min_daymet_data_table.csv"))
pine_mask = raster(paste0(spatial_data_output_dir, "pine_mask.nc"), crs = proj4_master)

# Make a raster stack with the same extent and resolution as the tree kill stack
neighbor_points = nabor::knn(
  winter_min_dt[, .(x, y)],
  coordinates(pine_mask),
  k = 1
)

winter_min_dt[winter_ending_1992 == -9999]
annual_min_dt[year_1986 == -9999]

# Problem tiles:
# 12452_1985
# 12453_1986
# 11915_1990
# 11200_1991
# The pine kill rasters are only the US, so there will be a lot of points in the SPDF that fall within Canada, or over the ocean.
exclude_ids = which(neighbor_points$nn.dists > 1000)
pine_mask_neighbor_data_table_rows = data.table(neighbor_points$nn.idx)
names(pine_mask_neighbor_data_table_rows) = "row_id"

fwrite(pine_mask_neighbor_data_table_rows, paste0(spatial_data_output_dir, "pine_mask_neighbor_data_table_rows.csv"))

templateR = raster(pine_mask)

winter_tmin_brick = annual_tmin_brick = brick()
for(i in head(daymet_years, -1)){
  # for(i in head(daymet_years, -10)){
  layer_win = layer_ann = templateR
  print(i)
  
  layer_win[] =
    winter_min_dt[pine_mask_neighbor_data_table_rows$row_id, get(paste0("winter_ending_", i + 1))]
  layer_win[exclude_ids] = NA
  layer_ann[] =
    annual_min_dt[pine_mask_neighbor_data_table_rows$row_id, get(paste0("year_", i))]
  layer_ann[exclude_ids] = NA
  
  winter_tmin_brick = addLayer(winter_tmin_brick, layer_win)
  annual_tmin_brick = addLayer(annual_tmin_brick, layer_ann)
}

writeRaster(winter_tmin_brick, paste0(spatial_data_output_dir, "winter_tmin_brick.nc"), format = "CDF", overwrite = T)
writeRaster(annual_tmin_brick, paste0(spatial_data_output_dir, "annual_tmin_brick.nc"), format = "CDF", overwrite = T)

# problem tiles

plot(winter_tmin_brick)
plot(subset(winter_tmin_brick, c(1, 40)))

sum(subset(winter_tmin_brick, 1)[] == subset(winter_tmin_brick, 43)[], na.rm = T)





arr[] = winter_min_dt[pine_mask_neighbor_data_table_rows$row_id, winter_ending_1981]
rr[exclude_ids] = NA
winter_min_dt[pine_mask_neighbor_data_table_rows, winter_ending_1981]
plot(rr)
plot(study_area, add = T)














winter_min_dt
winter_min_df[1:4, ]
backup = winter_min_df
winter_min_df = backup

winter_min_dt = data.table(winter_min_df)


coordinates(winter_min_df) = ~ X1 + X2
proj4string(winter_min_df) = proj4_master

winter_min_df@data = winter_min_df@data[, !grepl("2017", names(winter_min_df))]


g1 = ggplot(winter_min_dt[sample(1:nrow(winter_min_dt), 2000), , ], aes(x = x, y = y, color = winter_ending_2015))
g1 + geom_point()




