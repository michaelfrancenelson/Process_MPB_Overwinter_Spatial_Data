source("Globals.R")

require(raster)
require(rgdal)

# Tree kill rasters -------------------------------------------------------
  
# for getting the right extent below:
mpb_states = readOGR(dsn = paste0(spatial_data_output_dir, "mpb_states"), layer = "mpb_states")
  
tree_dirs = list.dirs(paste0(databasin_dir, "Western_Conterminous_US_Killed_Trees_Per_Grid_Cell_Mountain_Pine_Beetle"), recursive = F)
  i = 1; (contorta_files = list.files(tree_dirs[i], "w001001.adf", recursive = T, full.names = T))
  i = 2; (ponderosa_files = list.files(tree_dirs[i], "w001001.adf", recursive = T, full.names = T))
  
  # Save everything in a raster brick (bricks seem to play a little better witn ncdf)
  pine_kill_western_US_brick = brick()
  
  # This takes a little while:
  for(i in 1:length(contorta_files)){
    print(i)
    
    conR = raster(contorta_files[i])
    ponR = raster(ponderosa_files[i])
    
    # Set the na values to zero
    conR[conR[] == -9999] = 0
    ponR[ponR[] == -9999] = 0
    
    # We only want the sum (for now)
    sumR = projectRaster(conR + ponR, crs = CRS(proj4_master))
    
    pine_kill_western_US_brick = addLayer(pine_kill_western_US_brick, sumR)
    rm(conR, ponR, sumR)
  }
  
  # Set the extent to the same as the states with MPB
  pine_kill_western_US_brick = extend(pine_kill_western_US_brick, extent(mpb_states))
  proj4string(pine_kill_western_US_brick) = proj4_master
  
  # These won't be preserved in the NCDF save below
  names(pine_kill_western_US_brick) = paste0("pineKill_", kill_years)
  
  # Save as ncdf rather than rdata.  Rhis should be more resistant to incompatability to R version changes.
  writeRaster(pine_kill_western_US_brick, filename = paste0(spatial_data_output_dir, "pine_kill_western_US_brick.nc"), format = "CDF", overwrite = T)

  pine_kill_western_US_brick = brick(paste0(spatial_data_output_dir, "pine_kill_western_US_brick.nc"))
  pine_kill_western_US_brick
  
  
  # Raster masked by the western states:
  pine_mask = mask(x = subset(bbb, 1), mask = mpb_states)
  sum(is.na(pine_mask[]))
  pine_mask[!is.na(pine_mask)] = 1
  writeRaster(pine_mask, filename = paste0(spatial_data_output_dir, "pine_mask.nc"), format = "CDF")
  