# The years for which we have pine kill data from the Databasin data sets
kill_years = 1997:2010

# The years for which we have weather data from Daymet
survival_years = 1981:2016

# Proj4string for lon/lat projection
proj4_lon_lat = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"

# The projection we want to use for maps
proj4_master = "+proj=lcc +lat_1=25 +lat_2=60 +lat_0=42.5 +lon_0=-100 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs +towgs84=0,0,0"

# Location of large spatial data sets
spatial_data_input_dir = "E:/Data/spatial_data/"

# Where to write derived spatial data
spatial_data_output_dir = "E:/Data/derived_spatial_data/"

# Location of the databasin pine kill data sets
databasin_dir = "E:/Data/Databasin/"

# Location of the R & B model output from the Daymet weather data
daymet_output_dir = "E:/Data/Daymet/daymet_output/"

# Location of the original daymet data files
daymet_dir = "E:/Data/Daymet/"

# Location of smaller data sets (stored within this repository)
derived_data_dir = "derived_data/"


# Shapefile layer name for the US National Parks
usfs_layer = "S_USA.NFSLandUnit"

# Shapefile layer name for the US states polygons
states_layer = "tl_2017_us_state"

# Shapefile layer name for Canadian provinces
canada_layer = "lpr_000a16a_e"

# States to include in the US maps
MPB_states = c("WA", "OR", "CA", "ID", "NV", "UT", "AZ", "MT", "WY", "CO", "NM", "SD")

# Location to store figures
figures_dir = "figures/"