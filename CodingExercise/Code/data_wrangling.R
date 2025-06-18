rm(list = ls())
library(data.table)

# there are 4 datasets, land_use_panel.csv, cropReturns.csv, weatherAnd.csv that are needed to perform estimations..
# read the land use data
land_use <- fread("CodingExercise/Data/land_use_panel.csv")
# > unique(land_use$LU)
# [1] "olive"  "other"  "nuts"   "arable" "citric"
land_use[, crop := LU]
land_use[, crop_irr := paste(crop, irrigation, sep = "_")]
setorder(land_use, year)
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
