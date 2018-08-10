source("Globals.R")
source("daymet_class.R")
require(sp)
require(raster)
require(rgdal)

load(paste0(spatial_data_input_dir, "mpb_states/mpb_states.Rd"))
load(paste0(spatial_data_dir, "pine_kill_western_US/pine_kill_western_US_brick.Rd"))

# We need the mpb states polygons and pine kill rasters:
pine_kill_western_US_brick = brick(paste0(spatial_data_output_dir, "pine_kill_western_US_brick.nc"), crs = proj4_master)
mpb_states = readOGR(dsn = paste0(spatial_data_output_dir, "mpb_states"), layer = "mpb_states")


# The grid cells in the databasin (tree kill) and Daymet output (survival) do not coincide.
# Also, with the tiling of the Daymet data, adjacent tiles are not on the same grid
# The strategy is to create a big spatial points DF of the centroids of all Daymet raster cells.
# Then create a raster with the same extent and resolution as the tree kill data.
# Next, for each cell in the new raster, find the nearest point in the daymet data SPDF and assign that value to the raster.


# Get a lat-lon bounding box for the kill data
extent_master = as(extent(pine_kill_western_US_brick), "SpatialPolygons")
proj4string(extent_master) = proj4_master
spTransform(extent_master, proj4_lon_lat)


# Create a giant spatial points data frame with all the daymet cells:
daymet_files = paste0(daymet_output_dir, list.files(daymet_output_dir))

# A list with pre-defined length populates much faster than recursive calls to list()
daymet_spdfs = vector(mode = "list", length = length(daymet_files))
nrowsDF = 0
counterDFs = 1

# This is a little slow, there are 174 of them...
for(i in 1:length(daymet_files)) { 
  dm = makeDaymet(daymet_files[i]) 
  # if any part of the daymet tile falls within the boundary:
  if(daymetIn(extent_master, dm)){
    print(paste0(i, " ", dm@tileNum))
    dm_spdf = rasterToPoints(dm@data, spatial = T)
    stopifnot(proj4string(dm_spdf) == proj4_master)
    names(dm_spdf@data)
    dm_spdf = dm_spdf[complete.cases(dm_spdf@data), ]
    # daymet_spdfs[[counterDFs]] = dm_spdf[!is.na(over(dm_spdf, studySitePolygons$WesternUS_unmasked)), ]
    # (over(dm_spdf, mpb_states))[1000:1010, ]
    daymet_spdfs[[counterDFs]] = dm_spdf[!is.na(over(dm_spdf, mpb_states)[, 1]), ]
    nrowsDF = nrowsDF + nrow(daymet_spdfs[[counterDFs]]@data)
    counterDFs = counterDFs + 1
  }
}

# Pre-making a data frame is much faster than repeated calls to rbind()
mpb_overwinter_survival_western_US_spatial_points = data.frame(matrix(0, nrow = nrowsDF, ncol = ncol(dm_spdf) + 2))
nrowsDF; counterDFs
i = 1
count = 1
for(i in 1:(counterDFs - 1)){
  nrowsDF1 = nrow(daymet_spdfs[[i]]@data)
  if(nrowsDF1 > 0){
    from = count;
    to = from + nrowsDF1 - 1
    mpb_overwinter_survival_western_US_spatial_points[from:to, 1:2] = daymet_spdfs[[i]]@coords  
    mpb_overwinter_survival_western_US_spatial_points[from:to, 3:(2 + ncol(daymet_spdfs[[i]]@data))] = daymet_spdfs[[i]]@data
    count = count + nrow(daymet_spdfs[[i]]@data)
    print(i)
  }
}

names(mpb_overwinter_survival_western_US_spatial_points)
names(mpb_overwinter_survival_western_US_spatial_points) = c("x", "y", names(daymet_spdfs[[1]]@data))
coordinates(mpb_overwinter_survival_western_US_spatial_points) <- ~ x + y

head(mpb_overwinter_survival_western_US_spatial_points@data)

# That was a lot of work, so we'll save the intermediate result (this is slow and creates a huge file):
writeOGR(
  mpb_overwinter_survival_western_US_spatial_points, 
  dsn = paste0(spatial_data_output_dir, "mpb_overwinter_survival_western_US_spatial_points"), 
  layer = "mpb_overwinter_survival_western_US_spatial_points", driver = "ESRI Shapefile")
mpb_overwinter_survival_western_US_spatial_points = 
  readOGR(
    dsn = paste0(spatial_data_output_dir, "mpb_overwinter_survival_western_US_spatial_points"), 
    layer = "mpb_overwinter_survival_western_US_spatial_points")
# Make a raster stack with the same extent and resolution as the tree kill stack
neighbor_points = nabor::knn(
  coordinates(mpb_overwinter_survival_western_US_spatial_points),
  coordinates(pine_kill_western_US_brick),
  k = 1
)

# The pine kill rasters are only the US, so there will be a lot of points in the SPDF that fall within Canada, or over the ocean.
exclude_ids = which(neighbor_points$nn.dists > 1000)

# Template raster for the survival brick:
templateR = subset(pine_kill_western_US_brick, 1)
templateR[,] = NA

mpb_overwinter_survival_western_US_brick = brick()
for(i in 1:(ncol(mpb_overwinter_survival_western_US_spatial_points@data) - 1)){
  layer = templateR
  print(i)
  layer[] = mpb_overwinter_survival_western_US_spatial_points@data[neighbor_points$nn.idx, i]
  layer[exclude_ids] = NA  
  mpb_overwinter_survival_western_US_brick = addLayer(mpb_overwinter_survival_western_US_brick, layer)
}

plot(subset(mpb_overwinter_survival_western_US_brick, 1))
plot(mpb_states, add = T)
names(mpb_overwinter_survival_western_US_brick) = paste0("mpb_", survival_years)


# Save as ncdf rather than rdata.  Rhis should be more resistant to incompatability to R version changes.
writeRaster(
  mpb_overwinter_survival_western_US_brick, 
  filename = paste0(spatial_data_output_dir, "mpb_overwinter_survival_western_US_brick.nc"), format = "CDF" )
