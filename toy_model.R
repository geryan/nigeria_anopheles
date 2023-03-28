

covs <- bc_nigeria[[c(4,5,7)]]
names(covs) <- c("tseas", "tmax", "trange")

plot(covs)

df_occ <- data.frame(
  presence_absence = rbinom(150, 1, 0.3),
  trapping_method = rep(c("IRM", "CDC", "PSC"), each = 50),
  latitude = runif(150, 6.6, 12.4),
  longitude = runif(150, 3.7, 9.1)
)

coords <- df_occ[, c("longitude", "latitude")]
df_covs <- terra::extract(covs, coords)
df_pres <- df_occ[, c("presence_absence", "trapping_method")]

df <- cbind(df_pres, df_covs)

pred_method <- nigeria_mask
cls <- data.frame(id=1, trapping_method=c("IRM"))
levels(pred_method) <- cls

pred_covs <- c(covs, pred_method)

## GLM

m_glm <- glm(presence_absence ~ trapping_method + tseas + tmax + trange,
         family = binomial,
         data = df)

pred_glm <- terra::predict(pred_covs, m_glm, type = "response")

plot(pred_glm)
points(coords)

writeRaster(
  pred_glm,
  "output/prediction_glm.tif",
  overwrite = TRUE
)

## GAM
library(mgcv)
library(gratia)

m_gam <- gam(presence_absence ~ trapping_method + s(tseas) + s(tmax) + s(trange),
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
