library(tidyverse)
library(terra)
library(geodata)



# get extent map for our basic analyses - using outline of Kenya

nigeria_vector <- gadm(
  country = "NGA",
  level = 0,
  path = "data/downloads"
) # slow to run though not large

nigeria_vector

plot(nigeria_vector)

# let's download some data

# travel time from city to cell
# you can explore the sizes of city using ?travel time, and change size if you wish
travel <- travel_time(
  to = "city",
  size = 2,
  path = "data/downloads"
) # 421 MB - may be slow to download

# it's a good idea to have a look at your objects and even plot them


# bioclimactic variables from worldclim
# https://worldclim.org/data/bioclim.html
bioclim_nigeria <- worldclim_country(
  country = "NGA",
  var = "bio",
  res = 0.5,
  path = "data/downloads"
)

# have a look at this object first
# Do you think you don't want to plot all layers at once?

# process these data to match our extent

# generate mask of our area of interest so we can use it to process other data into this shape
nigeria_mask <- bioclim_nigeria[[1]] %>%
  mask(nigeria_vector) * 0 + 1
names(nigeria_mask) <- "mask"

plot(nigeria_mask)


# process bioclim into shape
bc_nigeria <- mask(
  x = bioclim_nigeria,
  mask = nigeria_mask
)

# process travel data
# this is a very large raster, we speed up the operation by cropping then masking
# try looking at the result of only cropping to understand why both steps are needed
travel_nigeria <- crop(
  x = travel,
  y = nigeria_mask
) %>%
  mask(
    mask = nigeria_mask
  )

# let's have a look at it


#
#
# # now let's create a bias layer out of our travel data
# # it should scale from 0-1 where 0 is hard to get to and 1 is easy
#
# plot(travel_kenya)
#
# rescale_travel <- travel_kenya
#
# rescale_travel[] <- rescale_travel[]/ max(rescale_travel[], na.rm = TRUE)
#
# rescale_travel[] <- 1 - rescale_travel[]
#
# plot(rescale_travel)



# now let's write our key rasters to disk
# NB the above download functions will only need to download once;
# once those objects are saved in that location, they will be re-read from disk

terra::writeRaster(
  x = nigeria_mask,
  filename = "data/grids/nigeria_mask.tif"
)

terra::writeRaster(
  x = bc_nigeria,
  filename = "data/grids/nigeria_kenya.tif"
)

# terra::writeRaster(
#   x = rescale_travel,
#   filename = "data/grids/rescale_travel.tif"
# )
