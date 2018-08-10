source("Globals.R")
require(raster)
require(rgdal)

pine_kill_western_US_brick = brick(paste0(spatial_data_output_dir, "pine_kill_western_US_brick.nc"), crs = proj4_master)


national_forests = readOGR(  dsn = paste0(spatial_data_output_dir, "national_forests_in_mpb_states_1km_buffer"),
                             layer = "national_forests_in_mpb_states_1km_buffer")

# not super slow
national_forests_raster_mask = mask(subset(pine_kill_western_US_brick, 1), national_forests)
national_forests_raster_mask[!is.na(national_forests_raster_mask[])] = 1

plot(national_forests_raster_mask)

# National Forests within pine range:
pine_range_national_forests = readOGR(dsn = paste0(spatial_data_output_dir, "pine_range_polygons_national_forests"),
                                      layer = "pine_range_polygons_national_forests")
plot(pine_range_national_forests)

pine_range_national_forests_raster_mask = mask(subset(pine_kill_western_US_brick, 1), pine_range_national_forests)
pine_range_national_forests_raster_mask[!is.na(pine_range_national_forests_raster_mask[])] = 1
plot(pine_range_national_forests_raster_mask)
sum(!is.na(pine_range_national_forests_raster_mask[]))

# Save as ncdf rather than rdata.  Rhis should be more resistant to incompatability to R version changes.
writeRaster(
  pine_range_national_forests_raster_mask, 
  filename = paste0(spatial_data_output_dir, "pine_range_national_forests_raster_mask.nc"), format = "CDF" )

# Save as ncdf rather than rdata.  Rhis should be more resistant to incompatability to R version changes.
writeRaster(
  national_forests_raster_mask, 
  filename = paste0(spatial_data_output_dir, "national_forests_raster_mask.nc"), format = "CDF" )



# Data tables of survival and tree kill
require(data.table)
kill_brick = brick(paste0(spatial_data_output_dir, "pine_kill_western_US_brick.nc"), crs = proj4_master)
survival_brick = brick(paste0(spatial_data_output_dir, "mpb_overwinter_survival_western_US_brick.nc"))

pine_range_national_forests_raster_mask = raster(paste0(spatial_data_output_dir, "pine_range_national_forests_raster_mask.nc"))
mask_cell_ids = which(!is.na(pine_range_national_forests_raster_mask[]))

pine_range_national_forests_raster_mask
kill_brick


kill_table = survival_table = data.table()
kill_year = 1
for(kill_year in 1:nlayers(kill_brick))
{
  print(kill_years[kill_year])
  kill_table = rbind(kill_table, data.table(kill = subset(kill_brick, kill_year)[][mask_cell_ids], cell_id = mask_cell_ids, year = kill_years[kill_year]))
}

for(survival_year in 1:nlayers(survival_brick))
{
  print(survival_years[survival_year])
  survival_table = 
    rbind(
      survival_table, 
      data.table(
        survival = subset(survival_brick, survival_year)[][mask_cell_ids], 
        cell_id = mask_cell_ids, 
        year = survival_years[survival_year]))
}
survival_table

sum(is.na(survival_table$survival))
sum(is.na(kill_table$kill))
subset(survival_brick, 1)[][mask_cell_ids]

fwrite(survival_table, file = paste0(spatial_data_output_dir, "survival_data_table.csv"))
fwrite(kill_table, file = paste0(spatial_data_output_dir, "kill_data_table.csv"))



# Aggregated data tables
survival_aggregated = survival_table[, .(survival = mean(survival, na.rm = T), lookback = 1), by = year]
survival_aggregated

# calculate lookbacks 2 - 16
for (lb in 2:16)
{
  for (i in lb : length(survival_years))
  {
   survival_aggregated = rbind(survival_aggregated, 
                               survival_aggregated[
                                 (i - lb + 1) : i, 
                                 .(year = survival_years[i], survival = mean(survival), lookback = lb)])
  }
}
survival_aggregated
survival_kill_aggregated = merge(survival_aggregated, kill_table[, .(kill = mean(kill)), by = year])

fwrite(
  survival_aggregated, 
  file = paste0(spatial_data_output_dir, "pine_range_national_forest_survival_aggregated.csv"))
fwrite(
  survival_kill_aggregated, 
  file = paste0(spatial_data_output_dir, "pine_range_national_forest_survival_kill_aggregated.csv"))

ggplot(survival_kill_aggregated[lookback == 9], aes(x = survival, y = kill)) + geom_point()


