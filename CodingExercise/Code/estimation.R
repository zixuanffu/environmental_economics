rm(list = ls())
pacman::p_load(data.table)

land_use <- fread("CodingExercise/Data/land_use_panel.csv")
land_use[, crop := LU]
land_use[, crop_irr := paste(crop, irrigation, sep = "_")]
head(land_use)

# there are 4 datasets, land_use_panel.csv, cropReturns.csv, weatherAnd.csv that are needed to perform estimations..

setorder(land_use, grid_id, year)
land_use[, crop_irr_l := shift(crop_irr, type = "lag"), by = grid_id]
land_use[, k_count := .N, by = .(year, prov, crop_irr_l)]
land_use[, kj_count := .N, by = .(year, prov, crop_irr_l, crop_irr)]
dt <- unique(land_use[, .(year, prov, crop_irr_l, crop_irr, k_count, kj_count)])
dt[, kj_ccp := kj_count / k_count]
head(dt)
dt <- dt[!is.na(crop_irr_l) & !is.na(crop_irr)]
dt[kj_count == 0] # check zero values ?

pacman::p_load(stringr)
# read subcrop return data
return <- fread("CodingExercise/Data/cropReturns.csv")
return[, irrigation := as.integer(str_detect(subcrop, regex("irrigated", ignore_case = TRUE)))]
return[, crop_irr := paste(crop, irrigation, sep = "_")] # subcrop has irrigation label but not the crop level
head(return)

# read weather data
weather <- fread("CodingExercise/Data/weatherAnd.csv")

# estimate the predicted yield

return <- merge(return, weather, by = c("year", "prov"))
colnames(return)
model <- lm(log(yield) ~ DD30 * precipitation + DD10 * precipitation + year + prov + subcrop, data = return)
summary(model)
return[, predictedYield := exp(predict(model, newdata = return))]
return[, predictedReturn := predictedPrice * predictedYield - cost + subsidies]
return[, Return := price * yield + subsidies - cost]

# aggregate subcrop prediction to crop level
return_agg <- return[, .(
    predictedYield = weighted.mean(predictedYield, area, na.rm = TRUE),
    predictedReturn = weighted.mean(predictedReturn, area, na.rm = TRUE),
    Return = weighted.mean(Return, area)
), by = .(year, prov, crop_irr)]

return_agg[prov == "Almería", prov := "Almeria"]
return_agg[prov == "Cádiz", prov := "Cadiz"]
return_agg <- return_agg[prov == "Almeria" | prov == "Cadiz", ]

# the estimating equation requires the construction of lhs variables
# though there are many paths from k to j to a to r
# we restrict ourselves to two paths kka and kaa

kjar <- copy(dt)
kjar <- merge(kjar, return_agg, by = c("year", "prov", "crop_irr"), all.x = TRUE)
kjar[is.na(Return), Return := 350]
kjar[is.na(predictedReturn), predictedReturn := 350]

dt_new <- data.table()
beta <- 0.9
for (t in 2018:2023) {
    prov_type <- unique(kjar[year == t]$prov)
    for (m in prov_type) {
        data_t <- kjar[year == t & prov == m, ] # data at time t
        for (k in unique(data_t$crop_irr_l)) {
            data_k1 <- data_t[crop_irr_l == k, ]
            for (a in unique(data_k1[crop_irr != k]$crop_irr)) {
                ccp_kk <- data_k1[crop_irr == k, kj_ccp] # p_kk at time t
                ccp_ka1 <- data_k1[crop_irr == a, kj_ccp] # p_ka at time t
                data_t1 <- kjar[(year == t + 1) & prov == m, ] # data at time t+1
                data_k2 <- data_t1[crop_irr_l == k, ]
                data_a <- data_t1[crop_irr_l == a, ]
                ccp_ka2 <- data_k2[crop_irr == a, kj_ccp] # p_ka at time t+1
                ccp_aa <- data_a[crop_irr == a, kj_ccp] # p_aa at time t+1
                y <- log(ccp_kk / ccp_ka1) + beta * log(ccp_ka2 / ccp_aa)
                return_k <- data_k1[crop_irr == k, predictedReturn]
                return_a <- data_k1[crop_irr == a, predictedReturn]
                dt_new <- rbind(dt_new, data.table(year = t, prov = m, k = k, a = a, lhs = y, rhs = return_k - return_a))
            }
        }
    }
}

unique(dt_new$prov) # only Almeria
unique(land_use[prov == "Cadiz"]$year) # because we need at least three time periods to construct the lhs var

# use ols
summary(lm(lhs ~ rhs, data = dt_new))

# create a new variable by combining k-a
dt_new[, unit := paste(k, a, sep = "_")]

setorder(dt_new, unit, year)

# Step 1:
# First differences
setorder(dt_new, unit, year)
dt_new[, lag_lhs := shift(lhs, type = "lag"), by = unit]
dt_new[, lag_rhs := shift(rhs, type = "lag"), by = unit]
dt_new[, dy := lhs - lag_lhs]
dt_new[, dx := rhs - lag_rhs]
dt_new[, lag2_rhs := shift(rhs, n = 2, type = "lag"), by = unit]

# Step 2: Remove rows with missing values in any relevant variable
dt_gmm <- dt_new[!is.na(dy) & !is.na(dx) & !is.na(lag2_rhs)]

# Step 3: Convert to matrix form for gmm()
dy_vec <- dt_gmm$dy
dx_vec <- dt_gmm$dx
instr_vec <- dt_gmm$lag2_rhs

# Step 4: Define moment function
gfun <- function(theta, data) {
    beta <- theta[1]
    dy <- data$dy
    dx <- data$dx
    z <- data$instr # instrument: lag2_rhs

    res <- dy - beta * dx
    gmat <- res * z # element-wise product gives moment condition
    return(as.matrix(gmat))
}

# Step 5: Estimate using gmm
data_list <- list(dy = dy_vec, dx = dx_vec, instr = instr_vec)
theta0 <- c(0.1)

gmm_result <- gmm(gfun, x = data_list, t0 = theta0)
summary(gmm_result)


# Get the intercept
# compute time averages
fe_dt <- dt_new[!is.na(lhs) & !is.na(rhs), .(
    y_bar = mean(lhs),
    x_bar = mean(rhs)
), by = unit]

# recover fixed effects
beta_hat <- coef(gmm_result)[1] # from your GMM estimation
fe_dt[, alpha_hat := y_bar - beta_hat * x_bar]
