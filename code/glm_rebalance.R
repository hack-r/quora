# Resample ----------------------------------------------------------------
set.seed(1)
samplepcnt = 16.5
minClass <- floor(min(table(train$is_duplicate))*(100/samplepcnt-1))
dt  =  subset(train, is_duplicate==0)
dt2 = sort(sample(nrow(dt), minClass, replace = T))
dt3 = dt[dt2,]
dt4 =  rbind(subset(train, is_duplicate==1),dt3)
nrow(dt4)

train.model <- sample_n(dt4,404290)

train.valid <- sample_n(dt4,100000)

# GLM1 --------------------------------------------------------------------
train.model$is_duplicate <- as.numeric(train.model$is_duplicate)

glm1 <- speedglm(is_duplicate ~ clean_lcs, data = train.model)
glm1

# GLM2 --------------------------------------------------------------------
train.model$is_duplicate <- as.numeric(train.model$is_duplicate)

glm2 <- speedglm(is_duplicate ~ clean_lcs + 0, data = train.model)
glm2

# GLM3 --------------------------------------------------------------------
train.model$is_duplicate <- as.numeric(train.model$is_duplicate)

glm3 <- speedglm(is_duplicate ~ clean_lcs + abs_wc_delta, data = train.model)
glm3

# GLM4 --------------------------------------------------------------------
train.model$is_duplicate <- as.numeric(train.model$is_duplicate)

glm4 <- speedglm(is_duplicate ~ clean_lcs + I(abs(wc_delta) > 20), family=binomial(), data = train.model)
glm4

# GLM5 --------------------------------------------------------------------
train.model$is_duplicate <- as.numeric(train.model$is_duplicate)

glm5 <- speedglm(is_duplicate ~ clean_lcs + 
                                I(abs(nchar_delta)) +
                                I(sum_flag_why == 2) +
                                I(abs(wc_delta) > 20), family=binomial(), data = train.model)
glm5


# Predictions -------------------------------------------------------------
p <- predict(glm4, test)

saveRDS(p, "glm4_predictions.RDS")

p <- predict(glm5, test)

saveRDS(p, "glm5_predictions.RDS")
