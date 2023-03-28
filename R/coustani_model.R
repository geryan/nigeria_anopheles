library(tidyverse)
library(terra)
library(mgcv)
library(gratia)

target_species <- "An. coustani"
target_species_print <- gsub("An. ", "", target_species)


covs <- rast("data/grids/covariates.grd")
nigeria_mask <- rast("data/grids/nigeria_mask.grd")
plot(covs)

df_occ <- data.frame(
  presence_absence = rbinom(150, 1, 0.3),
  survey_type = rep(c("IRM", "CDC", "PSC"), each = 50),
  latitude = runif(150, 6.6, 12.4),
  longitude = runif(150, 3.7, 9.1)
)

df_all <- read_csv("data/tabular/cleaned_data.csv")

df_occ <- df_all %>%
  filter(sp == target_species) %>%
  mutate(
    presence_absence = if_else(PA == "presence", 1, 0) # this is necessary to fit model
  )

coords <- df_occ[, c("Long", "Lat")]

df_covs <- terra::extract(covs, coords)


df_pres <- df_occ[, c("presence_absence", "survey_type")]

df <- cbind(df_pres, df_covs)

pred_method <- nigeria_mask
cls <- data.frame(id = 0, survey_type = c("Larval Survey"))
levels(pred_method) <- cls

pred_covs <- c(covs, pred_method)

df

## GLM

m_glm <- glm(presence_absence ~ survey_type + aspect1 + bio1 + bio10 + bio11 + bio12 + bio13 + bio14 + bio15 + bio16 + bio17 + bio18 + bio19 + bio2 + bio3 + bio4 + bio5 + bio6 + bio7 + bio8 + bio9 + elevation + hillshade1 + slope1,
             family = binomial,
             data = df)

pred_glm <- terra::predict(pred_covs, m_glm, type = "response")

plot(pred_glm)
points(coords, col = "grey80", pch = 16)
points(coords[df_occ$presence_absence == 1,], col = "black", pch = 16)

writeRaster(
  pred_glm,
  sprintf(
    "output/prediction_glm_%s.tif",
    target_species_print
  ),
  overwrite = TRUE
)

## GAM
m_gam <- gam(presence_absence ~ survey_type + s(aspect1) + s(bio1) + s(bio10) + s(bio11) + s(bio12) + s(bio13) + s(bio14) + s(bio15) + s(bio16) + s(bio17) + s(bio18) + s(bio19) + s(bio2) + s(bio3) + s(bio4) + s(bio5) + s(bio6) + s(bio7) + s(bio8) + s(bio9) + s(elevation) + s(hillshade1) + s(slope1),
             select = TRUE,
             family = binomial,
             data = df)


draw(m_gam) &
  theme_minimal()


class(p)

pred_gam <- terra::predict(pred_covs, m_gam, type = "response")


plot(pred_gam)
points(coords)

writeRaster(
  pred_gam,
  "output/prediction_gam.tif",
  overwrite = TRUE
)
