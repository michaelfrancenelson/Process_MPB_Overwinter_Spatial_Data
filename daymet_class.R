require("sp")
require("raster")
require(ncdf4)

setClass("daymet",
         slots = list(
           crs = "CRS",
           tileNum = "integer",
           attributes = "list",
           mask = "Raster",
           data = "RasterBrick",
           lat = "Raster", lon = "Raster",
           x = "numeric", y = "numeric",
           projInfo = "list",
           latRange = "numeric",
           lonRange = "numeric"
         ))

makeDaymet = function(filename)
{
  fileIn1 = ncdf4::nc_open(filename)
  attr1 = c(ncatt_get(fileIn1, varid = 0))
  tileNum1 = attr1$tileid
  projAttr1 = c(ncatt_get(fileIn1, "lambert_conformal_conic"))
  crs1 = CRS(paste(
    "+proj=lcc",
    paste0("+lat_1=", projAttr1$standard_parallel[1]),
    paste0("+lat_2=", projAttr1$standard_parallel[2]),
    paste0("+lat_0=", projAttr1$latitude_of_projection_origin),
    paste0("+lon_0=", projAttr1$longitude_of_central_meridian),
    "+x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs "))
  x1 = ncvar_get(fileIn1, "x")
  y1 = ncvar_get(fileIn1, "y")
  nc_close(fileIn1)
  
  br1 = brick(filename, varname = "predicted_survival", crs = crs1)
  mask1 = raster(br1, 1);  mask1[!is.na(mask1)] = 1
  lon1 = raster(filename, varname = "lon", crs = crs1) * mask1
  lat1 = raster(filename, varname = "lat", crs = crs1) * mask1
  
  latRange1 = round(range(lat1[], na.rm = T))
  lonRange1 = round(range(lon1[], na.rm = T))  
  
  return(new("daymet", 
             crs = crs1, 
             tileNum = tileNum1,
             attributes = attr1,  
             mask = mask1,
             
             data = br1, 
             lat = lat1, lon = lon1, 
             x = c(x1), y = c(y1),
             projInfo = projAttr1,
             latRange = latRange1,
             lonRange = lonRange1
  ))
}



setMethod("plot",
          c(x = "daymet", y = "numeric"),
          function(x, y) { image(x@data[, , y], zlim = c(0, 1), col = terrain.colors(100)); print(y)})

getVal = function(x, y, dm, layer)
{
  x1 = which(dm@x == x)
  y1 = which(dm@y == y)
  if (is.null(x1) || is.null(y1)) 
    return(NaN)
  else
    return(dm@data[x1, y1, layer])
}

inDaymet = function(lat, lon, dm)
{
  inLat = lat < dm@latRange[2] & lat > dm@latRange[1]
  inLon = lon < dm@lonRange[2] & lat > dm@lonRange[1]
  return (inLat && inLon)
}

inInterval = function(x, interval)
{
  stopifnot(length(interval) == 2L)
  return((interval[1] <= x) & (x <= interval[2]))
}

daymetIn = function(spPoly, dm){
  inLon = inInterval(dm@lonRange[1], spPoly@bbox[1, ]) | inInterval(dm@lonRange[2], spPoly@bbox[1, ])
  inLat = inInterval(dm@latRange[1], spPoly@bbox[2, ]) | inInterval(dm@latRange[2], spPoly@bbox[2, ])
  return((inLon & inLat))
}

findDaymet = function(lat, lon, dmList)
{
  for (dm in dmList)
  {
    if (inDaymet(lat, lon, dm)) return(dm)
  }
  return(NULL)
}

plotrange = function(dm)
{
  x = range(dm@x); y = range(dm@y)
  polygon(x = c(x[1], x[1], x[2], x[2], x[1]), y = c(y[1], y[2], y[2], y[1], y[1]))
}






getSurvival = function(spointsdf, daymetList)
{
  
  spdf = spTransform(spointsdf, proj4string(daymetList[[1]]@data))
  outMat = matrix(0, nrow = nrow(spdf), ncol = dim(daymetList[[1]]@data)[3])
  
  latLon = spTransform(spdf, "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")@coords
  
  for (i in 1:nrow(latLon)){
    dm = findDaymet(latLon[i, 2], latLon[i, 1], daymetList)
    if (!is.null(dm)) {
      outMat[i, ] = extract(dm@data, spdf[i, ])
      print(i)
    }
  }
  
  yearNames = names(dm@data)
  
  out = cbind(spdf, outMat)
  
  names(out)[(ncol(spdf) + 1) : length(names(out))] = yearNames
  
  return(out)
}



