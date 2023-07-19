library(tidyverse)
library(terra)
library(geodata)


#
# # get extent map for our basic analyses - using outline of Kenya
#
# nigeria_vector <- gadm(
#   country = "NGA",
#   level = 0,
#   path = "data/downloads"
# ) # slow to run though not large
#
# nigeria_vector
#
# plot(nigeria_vector)
#
# # let's download some data
#
# # travel time from city to cell
# # you can explore the sizes of city using ?travel time, and change size if you wish
# travel <- travel_time(
#   to = "city",
#   size = 2,
#   path = "data/downloads"
# ) # 421 MB - may be slow to download
#
# # it's a good idea to have a look at your objects and even plot them
#
#
# # bioclimactic variables from worldclim
# # https://worldclim.org/data/bioclim.html
# bioclim_nigeria <- worldclim_country(
#   country = "NGA",
#   var = "bio",
#   res = 0.5,
#   path = "data/downloads"
# )
#
# # have a look at this object first
# # Do you think you don't want to plot all layers at once?
#
# # process these data to match our extent
#
# # generate mask of our area of interest so we can use it to process other data into this shape
# nigeria_mask <- bioclim_nigeria[[1]] %>%
#   mask(nigeria_vector) * 0 + 1
# names(nigeria_mask) <- "mask"
#
# plot(nigeria_mask)
#
#
# # process bioclim into shape
# bc_nigeria <- mask(
#   x = bioclim_nigeria,
#   mask = nigeria_mask
# )
#
# # process travel data
# # this is a very large raster, we speed up the operation by cropping then masking
# # try looking at the result of only cropping to understand why both steps are needed
# travel_nigeria <- crop(
#   x = travel,
#   y = nigeria_mask
# ) %>%
#   mask(
#     mask = nigeria_mask
#   )

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
#
# terra::writeRaster(
#   x = nigeria_mask,
#   filename = "data/grids/nigeria_mask.tif"
# )
#
# terra::writeRaster(
#   x = bc_nigeria,
#   filename = "data/grids/bc_nigeria.tif"
# )

# terra::writeRaster(
#   x = rescale_travel,
#   filename = "data/grids/rescale_travel.tif"
# )
##

## Using covariate layers from Babalola

library(stringr)

rasterfiles <- list.files(
  path = "data/grids/Clipped_Layers/",
  pattern = ".tif$",
  full.names = TRUE
)


covs <- rast(rasterfiles)

writeRaster(
  covs,
  filename = "data/grids/covariates.grd",
  overwrite = TRUE
)

covs <- rast("data/grids/covariates.grd")

nigeria_mask <- covs[[1]]*0
names(nigeria_mask) <- "mask"

writeRaster(
  nigeria_mask,
  "data/grids/nigeria_mask.grd",
  overwrite = TRUE
)

#### Occupancy data
library(readxl)
# this is NG edit
#sheets <- readxl::excel_sheets("data/tabular/Data_for_Modeling_Other_species_27_03_2023_NG_edit.xlsx")

sheets <- readxl::excel_sheets("data/tabular/Merged_Data.xlsx")
sheets

file <- "data/tabular/Merged_Data.xlsx"

raw_data <- read_excel(
  path = file,
  sheet = "All_merged",
  #col_types = "text"
)

data_checked <- raw_data %>%
  mutate(
    sp = case_when(
      # where PA is absence, make sp = Absence
      PA == "Absence" ~ "Absence",
      .default = sp
    ),
    PA = case_when(
      # where a non-zero number is recorded and the species isn't listed as
      # Absence, make it a Presence
      Number > 0 & sp != "Absence" ~ "Presence",
      # where a zero number is recorded and the species is listed as
      # Absence, make it an Absence
      Number == 0 & sp == "Absence" ~ "Absence",
      .default = PA
    ),
    inconsistent = case_when(
      PA == "Presence" & sp == "Absence" ~ TRUE,
      is.na(`Survey type`) ~ TRUE,
      .default = FALSE
    ))

all_data <- data_checked %>%
  filter(!inconsistent)

dat <-  all_data %>%
  rename(survey_type = `Survey type`) %>%
  mutate(
    sp = sub(
      "A. ",
      "An. ",
      sp
    ),
    sp = if_else(sp == "An.pharoensis", "An. pharoensis", sp)
  ) %>% # fix inconsistent names
  mutate(
    Lat = as.numeric(Lat),
    Long = as.numeric(Long)
  )

#
# dat <- dat %>%
#   filter(!is.na(Long), !is.na(Lat)) %>% # this loses 3 points
#   filter(Long !=0, Lat !=0) # loses another 11 points # fixed

plot(nigeria_mask)
points(dat$Long, dat$Lat)


# coords <- dat[, c("Long", "Lat")] %>%
#   as.matrix()
#
# values <- terra::extract(nigeria_mask, coords)
#
# bad_coords <- is.na(values)
#
# dat <- dat[!bad_coords,] # removing coords outside of Nigera raster boundary # fixed



plot(elevation)
points(dat$Long, dat$Lat)

samples <- dat %>%
  select(STATE, Site, Lat, Long, Collection_Month, Collection_Year, survey_type) %>%
  distinct()

species <- unique(dat$sp)


sp_samples <- expand_grid(sp = species, samples) %>%
  filter(sp != "Absence")

sp_pres <- dat %>%
  filter(sp != "Absence") %>%
  select(-Number) %>%
  distinct() %>% # removing duplicate presences for same date
  mutate(PA = "Presence") # fixes P p presence and incorrect "absence" in PA


clean_dat <- left_join(
  x = sp_samples,
  y = sp_pres,
  by = colnames(sp_samples)
  ) %>%
  mutate(
    PA = case_when(
      is.na(PA) ~ "absence",
      TRUE ~ PA
    )
  )

table(clean_dat$PA, clean_dat$sp)

write_csv(
  clean_dat,
  file = "data/tabular/cleaned_data.csv"
)

# to fix - Biodun
# IRM 2022 lines 32-37 sp and PA in wrong column
# formatting of CDC and PSC - need state column see NG edit example CDC 2020
# data points outside of Nigeria
# missing lat longs
# IRM 2020 93-94 repeated
# CDC 2020 60-61 in original sheet (in NG edited sheet: 57-58) same day repeated (though different count on same day)
# Italics in months  - does it mean anything?
# CDC 2020 line 49 coustani presence listed as absence in PA
# multiple spelling of presence v Presence?

# fixable by code:
## Inconsistent species names
