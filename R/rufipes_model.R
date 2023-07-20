library(tidyverse)
library(terra)
library(glmnet)

target_species <- "An. rufipes"
target_species_print <- gsub("An. ", "", target_species)

covs_keep <- c(
  paste0("bio", c(1:3, 9, 13, 14, 19)),
  "elevation",
  "slope1",
  "aspect1",
  "hillshade1"
)


covs <- rast("data/grids/covariates.grd")
covs <- covs[[covs_keep]]
covs <- scale(covs)
nigeria_mask <- rast("data/grids/nigeria_mask.grd")

df_all <- read_csv("data/tabular/cleaned_data.csv")

df_occ <- df_all %>%
  filter(sp == target_species) %>%
  mutate(
    presence_absence = if_else(PA == "Presence", 1, 0), # this is necessary to fit model
    survey_type = factor(survey_type)
  )

# filter down to whether each species was ever detected at each location
df_occ <- df_occ %>%
  group_by(
    sp, Lat, Long, STATE, Site, survey_type
  ) %>%
  summarise(
    presence_absence = max(presence_absence),
    .groups = "drop"
  )

table(df_occ$presence_absence)

coords <- df_occ[, c("Long", "Lat")]

df_covs <- terra::extract(covs, coords) %>%
  dplyr::select(-ID)

df_pres <- df_occ[, c("presence_absence", "survey_type")]

df <- cbind(df_pres, df_covs)

pred_covs <- covs

x <- model.matrix(
  ~ survey_type +
    bio1 +
    bio2 +
    bio3 +
    bio9 +
    bio13 +
    bio14 +
    bio19 +
    elevation +
    slope1 +
    aspect1 +
    hillshade1,
  data = df)

m_glmnet <- cv.glmnet(
  x,
  df$presence_absence,
  nfolds = nrow(x),
  grouped = FALSE,
  family = "binomial"
)

all_cells <- which(!is.na(as.vector(nigeria_mask)))
pred_covs_df <- extract(pred_covs, all_cells) %>%
  bind_cols(
    tibble(`(Intercept)` = 1,
           `survey_typeLarval Survey` = 1,
           `survey_typePyrethrum Spray Catches` = 0
    ),
    .
  ) %>%
  as.matrix()

pred_glmnet_df <- predict(m_glmnet, pred_covs_df,
                          type = "response")

pred_glmnet <- nigeria_mask
names(pred_glmnet) <- "lyr1"
pred_glmnet[all_cells] <- pred_glmnet_df

writeRaster(
  pred_glmnet,
  sprintf(
    "output/prediction_glmnet_%s.tif",
    target_species_print
  ),
  overwrite = TRUE
)

plot(pred_glmnet)
points(coords, bg = "grey80", pch = 21)
points(coords[df_occ$presence_absence == 1,], col = "black", pch = 16)

# which covariates are important?
coefs_matrix <- t(m_glmnet$glmnet.fit$beta[, m_glmnet$index["1se", ]])
coefs <- as.vector(coefs_matrix)
names(coefs) <- colnames(coefs_matrix)
coefs_keep <- coefs != 0 & !grepl("survey_type", names(coefs))
important_coefs <- sort(coefs[coefs_keep]) %>%
  t() %>%
  as_tibble()
important_coefs

write_csv(
  important_coefs,
  sprintf(
    "output/coefs_glmnet_%s.csv",
    target_species_print
  )
)


