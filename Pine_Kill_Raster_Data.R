source("Globals.R")
require(rgeos)
require(raster)
require(rgdal)

# Tree kill rasters -------------------------------------------------------

# for getting the right extent below:
mpb_states = readOGR(dsn = paste0(spatial_data_output_dir, "mpb_states"), layer = "mpb_states")
mpb_usa = readOGR(dsn = paste0(spatial_data_output_dir, "mpb_usa"), layer = "mpb_usa")


tree_dirs = list.dirs(paste0(databasin_dir, "Western_Conterminous_US_Killed_Trees_Per_Grid_Cell_Mountain_Pine_Beetle"), recursive = F)
i = 1; (contorta_files = list.files(tree_dirs[i], "w001001.adf", recursive = T, full.names = T))
i = 2; (ponderosa_files = list.files(tree_dirs[i], "w001001.adf", recursive = T, full.names = T))

# Save everything in a raster brick (bricks seem to play a little better witn ncdf)
pine_kill_western_US_brick = brick()

# read files --------------------
# This takes a little while:
for(i in 1:length(contorta_files)){
  print(i)
  
  conR = raster(contorta_files[i])
  ponR = raster(ponderosa_files[i])
  
  # Set the na values to zero
  conR[conR[] == -9999] = 0
  ponR[ponR[] == -9999] = 0
  
  pine_kill_western_US_brick = addLayer(pine_kill_western_US_brick, conR + ponR)
  rm(conR, ponR)
}

# kinda slow
pine_kill_western_US_brick = projectRaster(pine_kill_western_US_brick, crs = CRS(proj4_master))

# Set the extent to the same as the states with MPB
pine_kill_western_US_brick = extend(pine_kill_western_US_brick, extent(mpb_usa))
proj4string(pine_kill_western_US_brick) = proj4_master
plot(subset(pine_kill_western_US_brick, 1))




# These won't be preserved in the NCDF save below
names(pine_kill_western_US_brick) = paste0("kill_", kill_years)

# Save as ncdf rather than rdata.  Rhis should be more resistant to incompatability to R version changes.
writeRaster(pine_kill_western_US_brick, filename = paste0(spatial_data_output_dir, "pine_kill_western_US_brick.nc"), format = "CDF", overwrite = T)


# Raster masked by the western states:
pine_kill_western_US_brick = brick( paste0(spatial_data_output_dir, "pine_kill_western_US_brick.nc"), crs = CRS(proj4_master))
pine_kill_western_US_brick

pine_mask = subset(pine_kill_western_US_brick, 1)
pine_mask[!is.na(pine_mask[])] = 1
plot(pine_mask)
plot(mpb_usa, add = T)

writeRaster(pine_mask, filename = paste0(spatial_data_output_dir, "pine_mask.nc"), format = "CDF", overwrite = T)
