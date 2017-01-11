##TEAM BS - Bart Middelburg & Stijn Beernink
##10-01-2017
##Script lesson 2

rm(list=ls())

#install packages
install.packages("spatstat")
install.packages("rgeos")
install.packages("countrycode")

#install library
library(raster)
library(spatstat)
library(rgeos)
library(countrycode)

##Simple function where you can select your own country of choice and how much points need to be sampled. 
##Use this function by DEM_Country("Your country", amount of points)
##See examples below

#Create Function
DEM_Country = function(country, pointsnumber)
{ 
  
  ##to attain full country names
  cc <- countrycode(country, "country.name", "iso3c", warn = FALSE)
  
  ## the downloading of the country DEM data
  CTRY <- raster::getData('alt', country= cc, mask=TRUE) ## SRTM 90m height data
  CTRYshp <- raster::getData('GADM', country= cc, level=2) ## administrative boundaries
  ## Create random points
  dran <- runifpoint(pointsnumber, win = as.vector(extent(CTRY)))
  ## Make the random point spatial points
  S <- SpatialPoints(data.frame(x = dran$x, y = dran$y), 
                     proj4string = CRS(proj4string(CTRYshp)))
  ## Select only the ones within belgium
  Sint <- gIntersection(S, CTRYshp)
  ## Create a map
  plot(CTRY, main = paste("DEM (m) of", country), sub= paste("Number of points:",pointsnumber))
  plot(CTRYshp, add=TRUE)
  plot(Sint, add = TRUE, col = "red", pch = 19, cex = 0.2)
  
  out <- extract(CTRY, Sint, df = TRUE)
  colnames(out) <- c("id", "height")
  head(out)
  
  plot(out, type = "p", pch = 19)
  
} 

##EXAMPLES##

#Netherlands
DEM_Country("Netherlands", 1200)

#Belgium
DEM_Country("Belgium", 280)

#Zimbabwe
DEM_Country("Zimbabwe", 400)



