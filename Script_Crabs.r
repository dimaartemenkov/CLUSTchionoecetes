library(xlsx)
library(dplyr)
library(mclust)

setwd('C:/R/Chionoecetes')

crabs <- read.xlsx2("01_Crabs_2021_baird_A.xlsx", sheetIndex = 1,
                                        colClasses = c("character",
                                                       rep("numeric", 7)))

# head(crabs)
# summary(crabs)

crabs <- crabs %>%
  filter(!is.na(Num)) %>%
  select(CW, LL, Ln_CW, Ln_LL, rel_CW.LL, Ln_rel_CW.LL)

plot(crabs$Ln_CW, crabs$Ln_LL)
# plot(crabs$CW, crabs$LL)
# plot(crabs$CW^1.7, crabs$LL)
# plot(crabs$CW, crabs$rel_CW.LL)	plot(crabs$rel_CW.LL, crabs$CW)
# plot(crabs$CW, crabs$Ln_rel_CW.LL)
plot(crabs$Ln_CW, crabs$rel_CW.LL)
#plot(crabs$Ln_CW, crabs$Ln_rel_CW.LL)

# Visualisation of distributions prior to further work
X <- crabs[, c(3, 5)]

dens <- densityMclust(X, G = 2)
summary(dens)
plot(dens, what = "density", data = X, grid = 200,
     points.cex = 0.5, drawlabels = FALSE)
plot(dens, what = "density", type = "image",
     col = "steelblue", grid = 200)

# Outliers detection with Mahalanobis distance
# Note this step is not necessary in some cases


library(rrcovHD)

obj <- OutlierMahdist(crabs[, c(3, 5)])
summary(as.factor(obj@flag))
crabs$flag <- obj@flag
crabs <- crabs %>%
  arrange(desc(flag), Num)
# head(crabs)
# tail(crabs)


X <- crabs[crabs$flag == 1, c(3, 5)]
plot(X[, 1], X[, 2])

# The list of available models
model_names <- c("EEE", "EVE", "VEE", "EEV", "VEV", "VVE", "VVV")

# In some cases EEE (1) works quite fine,
# but in other cases VEV (5) or VVV (7) work better
mod_num <- 1
gmm.mclust <- Mclust(X, 2,
                     modelNames = model_names[mod_num])

plot(X[, 1], X[, 2], col = gmm.mclust$classification)

# Saving results of classification with zeros for outliers
crabs$classification <- 0
crabs$classification[1:dim(X)[1]] <- gmm.mclust$classification

# write.csv(crabs, file = "02_Crabs_2021_baird_KG_classified.csv", row.names = F)
# write.csv(crabs, file = "03_Crabs_2021_opil_ZB_classified.csv", row.names = F)
write.csv(crabs, file = "Crabs_2022_opilio_Barents6_22.csv", row.names = F)
