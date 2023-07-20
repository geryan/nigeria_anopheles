library(tidyverse)
library(terra)
library(mgcv)
library(gratia)

covs <- rast("data/grids/covariates.grd")
nigeria_mask <- rast("data/grids/nigeria_mask.grd")
plot(covs)



df_all <- read_csv("data/tabular/cleaned_data.csv")

df_locs <- df_all %>%
  mutate(
    presence_absence = if_else(PA == "Presence", 1, 0) # this is necessary to fit model
  ) %>%
  group_by(sp, Lat, Long, STATE) %>%
  summarise(
    pa = max(presence_absence),
    .groups = "drop"
  )


library(ggplot2)

ggplot(df_locs) +
  geom_point(
    aes(
      x = Long,
      y = Lat,
      col = pa
    )
  ) +
  facet_wrap(~sp)


ggplot(df_locs) +
  geom_point(
    aes(
      x = Long,
      y = Lat,
      col = STATE
    )
  )
