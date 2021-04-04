# Library -----------------------------------------------------------------

library(sf)
library(dplyr)

library(ggplot2)

library(rgdal)
library(raster)

# About -------------------------------------------------------------------

# Partially following https://www.youtube.com/watch?v=WBfcR0zN0xk
# video entitled: "How to get the bounding box of spatial polygons in R"

# Partially following datacamp.com/courses/spatial-analysis-with-sf-and-raster-in-r
# DataCamp course entitled: "Spatial Analysis with sf and raster in R"

# Sample Shape File from https://docs.mapbox.com/help/glossary/shapefile/


# Exploring shape file ----------------------------------------------------


# Reading shp file using sf package.
stations <- st_read("stations/stations.shp")

# Reads as a dataframe?
str(stations)
class(stations) # sf and data.frame class

# View first six records: Spatial metadata
head(stations)

# Geometry column is class "sfc_POINT" and "sfc"
class(stations$geometry)

# Get number of stations per line
stations %>%
  group_by(line) %>%
  tally()

# Most stations are on the "blue" or "green" lines. 
# dplyr tally output contains the metadata

# Get area. What does this mean?
st_area(stations)

# Output produces vector of zeroes?
str(st_area(stations))
class(st_area(stations))

# Isolate a point
stations$geometry[1]

st_area(stations[1])

# Not sure why this is all zeroes.


# Troubleshooting ---------------------------------------------------------

# Following https://r-spatial.github.io/sf/reference/geos_measures.html

geometries <- rbind(c(-1,-1), 
                    c(1,-1), 
                    c(1,1), 
                    c(-1,1), 
                    c(-1,-1)) # Why is this point listed twice?

# Make a polygon
b0 <- st_polygon(list(geometries))
b0
b1 <- b0 + 2
b1
b2 <- b0 + c(-0.2, 2)
b2

# "Create simple feature geometry list column, set class, and add coordinate reference system and precision"
x <- st_sfc(b0, b1, b2)

# Get area
st_area(x)


#   -----------------------------------------------------------------------

# Not a multipolygon: Only two points.
stations$geometry[1]

# Combine two
rbind(stations$geometry[1],
      stations$geometry[2])

# Not the desired result. Attempt to extract points
st_coordinates(stations$geometry)

# Combining two
geometries2 <- rbind(st_coordinates(stations$geometry[1]),
                    st_coordinates(stations$geometry[2]))
b0_2 <- st_polygon(list(geometries2))

# Error: "polygons not (all) closed"

geometries2 <- rbind(st_coordinates(stations$geometry[1]),
                     st_coordinates(stations$geometry[2]),
                     st_coordinates(stations$geometry[1])) # Listing point twice to close polygon.
b0_2 <- st_polygon(list(geometries2))

# Transform polygon into a geometry set.
x_2 <- st_sfc(b0_2)
x_2

# Get the area of this polygon.
st_area(x_2)
# Literally just a line. Area is zero. Expected.

# Better example:
# How much distance is between these four stations?
stations$name[1:4]

# Create a polygon using the four stations as points. 
# Don't forget to close the polygon by adding the first point to the end of the coordinate list.
points <- rbind(st_coordinates(stations$geometry[1:4]), 
                st_coordinates(stations$geometry[1]))

station_poly <- st_polygon(list(points))
class(station_poly)
points
# Transform polygon into a geometry set.

station_polyset <- st_sfc(station_poly)
class(station_polyset)

# Get the area
st_area(station_polyset)

# Cool. I have no idea what this means.

# View points
station_poly

# Plot the polygon
plot(station_poly)

# This is a visualization of the space between four stations. Not superimposed on a map.

# Where is this?
stations$geometry[1]

station1 <- data.frame(st_coordinates(stations$geometry[1]))
station1
world <- map_data("world")
ggplot()+
  geom_polygon(data = world, aes (x = long, y = lat, group = group)) +
  coord_fixed(1.3) +
  geom_point(data = station1, aes(x = X, y = Y), color = "red")

# North America.
stations$geometry[1]

# Need a closer view. Loading the Google API
library(ggmap)

register_google(API_KEY)

# Get a map centered on mean 
test <- data.frame(points)
map <- get_map(data.frame(mean(test$X), mean(test$Y)), zoom = 12)

# Build map using the points first, then use the polygon shapes.
ggmap(map) +
  geom_polygon(data = test, aes(x = X, y = Y)) +
  labs(title = "Area between four metro stations",
       subtitle = paste0("Area = ", st_area(station_polyset)))
ggsave("output/Test1.png")



# Get points for all the stations
points <- data.frame(rbind(st_coordinates(stations$geometry)))
points <- cbind(points, stations$line)
# Center map on average lat/lon
map <- get_map(data.frame(mean(points$X), mean(points$Y)), zoom = 11)

# Plot locations of stations
ggmap(map) +
  geom_point(data = points, aes(x = X, y = Y), color = stations$line) +
  labs(title = "Metro stations and lines")
ggsave("output/Test2.png")

# Can I get a bounding box for each line?


