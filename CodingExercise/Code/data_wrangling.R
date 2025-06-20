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

land_merged <- merge(land_use[crop != "other"], grid, by = "grid_id", all.x = TRUE)
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


# estimate the ccp by frequency (without smoothing)
land_use[, crop_irr_l := shift(crop_irr, type = "lag"), by = .(year, prov)]
land_use[, k_count := .N, by = .(year, prov, crop_irr_l)]
land_use[, kj_count := .N, by = .(year, prov, crop_irr_l, crop_irr)]
dt <- unique(land_use[, .(year, prov, crop_irr_l, crop_irr, k_count, kj_count)])
dt[, kj_ccp := kj_count / k_count]

library(stringr)
# read subcrop return data
return <- fread("CodingExercise/Data/cropReturns.csv")
return[, irrigation := as.integer(str_detect(subcrop, regex("irrigated", ignore_case = TRUE)))]
return[, crop_irr := paste(crop, irrigation, sep = "_")] # subcrop has irrigation label but not the crop level
# read weather data
weather <- fread("CodingExercise/Data/weatherAnd.csv")

# the prediction is really bad. let's not use the it for the moment.
return <- merge(return, weather, by = c("year", "prov"))
crop_type <- unique(return$crop_irr)
for (i in crop_type) {
    for (m in unique(return$prov)) {
        data <- return[crop_irr == i & prov == m, ]
        model <- lm(log(yield) ~ year + DD10 + DD30 + precipitation, data = data) # issue: to be modified.
        prediced_yield <- exp(predict(model, newdata = data))
        return[crop_irr == i & prov == m, predictedYield := prediced_yield]
    }
}

return[, Return := price * yield + subsidies - cost]
return[, predictedReturn := predictedPrice * predictedYield - cost + subsidies]
return_agg <- return[, .(
    predictedYield = weighted.mean(predictedYield, area, na.rm = TRUE),
    predictedReturn = weighted.mean(predictedReturn, area, na.rm = TRUE),
    Return = weighted.mean(Return, area)
), by = .(year, prov, crop_irr)]
return_agg[prov == "Almería", prov := "Almeria"]
return_agg[prov == "Cádiz", prov := "Cadiz"]
return_agg <- return_agg[prov == "Almeria" | prov == "Cadiz", ]

# the nicely derived regresion equation requires the construction of lhs and rhs variables
# for every k-j-a-r combination. Last year planted k, this year planted j, this year planted a, next year plant r
# let us define the new panel data as kjar
beta <- 0.9
# create crop by taking the letters before _
kjar <- copy(dt)
kjar <- merge(kjar, return_agg, by = c("year", "prov", "crop_irr"), all.x = TRUE)
kjar[is.na(Return), Return := 350]
kjar[is.na(predictedReturn), predictedReturn := 350]

dt_new <- data.table()

for (t in 2018:2023) {
    prov_type <- unique(kjar[year == t]$prov)
    for (m in prov_type) {
        data_t <- kjar[year == t & prov == m, ]
        for (k in unique(data_t$crop_irr_l)) {
            data_k <- data_t[crop_irr_l == k, ]
            for (j in unique(data_k$crop_irr)) {
                for (a in unique(data_k[crop_irr != j]$crop_irr)) {
                    ccp_kj <- data_k[crop_irr == j, kj_ccp]
                    ccp_ka <- data_k[crop_irr == a, kj_ccp]
                    data_t1 <- kjar[(year == t + 1) & prov == m, ]
                    data_j <- data_t1[crop_irr_l == j, ]
                    data_a <- data_t1[crop_irr_l == a, ]
                    for (r in unique(data_t1[crop_irr != j & crop_irr != a]$crop_irr)) {
                        ccp_jr <- data_j[crop_irr == r, kj_ccp]
                        ccp_ar <- data_a[crop_irr == r, kj_ccp]
                        y <- log(ccp_kj / ccp_ka) + beta * log(ccp_jr / ccp_ar)
                        return_j <- data_k[crop_irr == j, Return]
                        return_a <- data_k[crop_irr == a, Return]
                        dt_new <- rbind(dt_new, data.table(year = t, prov = m, k = k, j = j, a = a, r = r, lhs = y, rhs = return_j - return_a))
                    }
                }
            }
        }
    }
}

# let's say we have n crop type. then for a given state k, we have n*(n-1)*(n-2) combinations of j, a, r
# let us construct a new panel that has n*(n-1)*(n-2) rows for each state k

# run the ols first with nothing eles
model <- lm(lhs ~ rhs, data = dt_new)
summary(model)

# for each k-j-a-r combination, we have a few years of observations.
# the unit is indexed by kjar (but also the prov)
# the time is indexed by year

# use arellano-bond estimator
library(plm)
# create a new variable by combining k and prov
dt_new[, unit := paste(k, j, a, r, prov, sep = "_")]
unique(dt_new$unit)
panel <- dt_new[, .N, by = unit]
dt_panel <- pdata.frame(dt_new, index = c("unit", "year"))
model2 <- plm(lhs ~ rhs, data = dt_panel, index = c("unit", "year"), model = "within", effect = "individual")
summary(model2)
model3 <- pgmm(lhs ~ rhs | lag(rhs, 1:1), data = dt_panel, effect = "individual")
summary(model3)
?pgmm
?plm

# let us say we recover the fixed effect from each kjar(m) combination
