rm(list = ls())
pacman::p_load(data.table, sf, ggplot2)

# there are 4 datasets, land_use_panel.csv, cropReturns.csv, weatherAnd.csv that are needed to perform estimations..

# read the land use data
land_use <- fread("CodingExercise/Data/land_use_panel.csv")
# > unique(land_use$LU)
# [1] "olive"  "other"  "nuts"   "arable" "citric"
# reclassify the land use by crop type + irrigation
land_use[, crop := LU]
land_use[, crop_irr := paste(crop, irrigation, sep = "_")]
# > unique(land_use$crop_irr)
# [1] "olive_0"  "other_0"  "olive_1"  "nuts_0"   "arable_1" "nuts_1"   "arable_0"
# [8] "other_1"  "citric_1"
setorder(land_use, year)

# plotting irrigation
grid <- st_read("CodingExercise/Data/sigpacGrid250.shp")
colnames(grid)
setDT(grid)
grid[, area := st_area(geometry) / 10000] # convert to hectares
colnames(grid)
head(grid)
# export the head of the grid into a latex table
library(xtable)
grid_head <- head(grid, 10)
grid_head$geometry <- NULL
grid_head_xtable <- xtable(grid_head, digits = 2)
print(grid_head_xtable, type = "latex", file = "CodingExercise/Tables/grid_head.tex", include.rownames = FALSE)

# plot irrigated and non-irrigated cropland in Almeria for 2021
land_2021 <- land_use[year == 2021 & prov == "Almeria" & crop != "other", ]
land_2021[, irrigation := as.factor(irrigation)]
# merge with grid map
land_2021_grid <- merge(land_2021, grid, by = "grid_id", all.x = TRUE)

# plot the spatial distribution
land_2021_sf <- st_as_sf(land_2021_grid)
class(land_2021_sf)

p <- ggplot(data = land_2021_sf) +
    geom_sf(aes(fill = irrigation)) +
    scale_fill_manual(name = "Irrigation", values = c("0" = "red", "1" = "blue"), labels = c("0" = "Non-Irrigated", "1" = "Irrigated"))
p
ggsave("CodingExercise/Figures/land_use_distribution_almeria_2021.pdf", plot = p, width = 10, height = 6)

# plot total area under irrigation over time
land_merged <- merge(land_use, grid, by = "grid_id", all.x = TRUE)
saveRDS(land_merged, "CodingExercise/Data/landuse_grid_merged.rds")
land_merged <- land_merged[crop != "other"]
area_by_crop <- land_merged[, .(area_ha = sum(area, na.rm = TRUE)), by = .(year, crop)]
area_by_crop_avg <- area_by_crop[, .(avg_area_ha = mean(area_ha, na.rm = TRUE)), by = crop] # ignoring irrigation
area_by_crop_avg
# export
print(xtable(area_by_crop_avg, digits = 2), type = "latex", file = "CodingExercise/Tables/area_by_crop_avg.tex", include.rownames = FALSE)

# by crop and irrigation
area_by_crop <- land_merged[, .(area_ha = sum(area, na.rm = TRUE)), by = .(year, crop, irrigation)]
area_by_crop_avg <- area_by_crop[, .(avg_area_ha = mean(area_ha, na.rm = TRUE)), by = .(crop, irrigation)] # ignoring irrigation
print(xtable(area_by_crop_avg, digits = 2), type = "latex", file = "CodingExercise/Tables/area_by_crop_irr_avg.tex", include.rownames = FALSE)

area_by_crop_irrigation <- land_merged[irrigation == 1, .(area_ha = sum(area, na.rm = TRUE)), by = .(year, crop)]
area_irrigation <- area_by_crop_irrigation[, .(area_ha = sum(area_ha, na.rm = TRUE)), by = year]
area_irrigation[, crop := "total"]
area_irrigation_merged <- rbind(area_by_crop_irrigation, area_irrigation)
area_irrigation_merged[, area_ha := as.numeric(area_ha)]

# plot irrigated area by crop over time
p_irrigation <- ggplot(area_irrigation_merged[, ], aes(x = year, y = area_ha, color = crop)) +
    geom_line() +
    labs(x = "year", y = "Irrigated Area (ha)")
p_irrigation
ggsave("CodingExercise/Figures/irrigated_area_by_crop_over_time.pdf", plot = p_irrigation, width = 10, height = 6)
