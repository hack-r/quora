# Resample ----------------------------------------------------------------
set.seed(1)
samplepcnt = 16.5
minClass <- floor(min(table(x_train2$y))*(100/samplepcnt-1))
dt  =  subset(x_train2, y==0)
dt2 = sort(sample(nrow(dt), minClass, replace = T))
dt3 = dt[dt2,]
dt4 =  rbind(subset(x_train2, y==1),dt3)
nrow(dt4)

train.model <- sample_n(dt4,404290)
mean(as.numeric(train.model$y))


# DRF ---------------------------------------------------------------------
set.seed(1)

# H2O Setup
system("java -Xmx20g -jar E://Jason//h2o//h2o.jar", wait = F)
h2o.init(nthreads = -1)

# Send data to H2O
x_train2$y <- y_train$V1
x_valid2$y <- y_valid$V1

trainHex <- as.h2o(x_train2) 
validHex <- as.h2o(x_valid2)
scoreHex <- as.h2o(x_test2)

trainHex[,"y"] <- as.factor(trainHex[,"y"])
validHex[,"y"] <- as.factor(validHex[,"y"])

drf <- h2o.grid(algorithm="randomForest",
                x= vi$variable[1:67],#setdiff(colnames(x_train2),"y"),
                y="y",
                training_frame = trainHex,
                ignore_const_cols = T,
                #ntrees = 200,
                max_depth = 18,
                #mtries=10,
                sample_rate=.632,
                stopping_metric = "logloss",
                hyper_params=list(ntrees = c(200)#,,1000
                                  #max_depth=c(18)
                                  #,mtries=c(10)
                                  #,sample_rate=c(0.632,)
                                  #col_sample_rate_per_tree=c(.8,1)
                )) # 21md better in CV but worse on public LB than 18md

summary(drf)
drf.best <- h2o.getModel("Grid_DRF_RTMPNA_3_model_R_1496778523532_41_model_0")

summary(drf.best)
h2o.varimp(drf.best)
vi <- as.data.frame(h2o.varimp(drf.best)); #saveRDS(vi, "vi_reusable.RDS")

# Validation
valid_pred <- predict(drf.best, validHex)
table(x_valid2$y)
round(colSums(as.data.frame(valid_pred[,2:3])))

MultiLogLoss(as.data.frame(valid_pred[,2:3]),x_valid2$is_duplicate)

p <- as.data.frame(valid_pred)#d$p1
plot(density(as.numeric(p$p1)))

# Scoring
scoreHex[,"same_word_count"] <- as.factor(scoreHex[,"same_word_count"]) # if needed

pred <- predict(drf.best, scoreHex)
pred <- as.data.frame(pred)

sample$is_duplicate <- pred$p1
plot(density(sample$is_duplicate))

#fwrite(sample, "drf_18deep_1000trees_pybalance_new_feats_all.csv", row.names = F) #
saveRDS(sample, "drf_18deep_1000trees_pybalance_new_feats_all.RDS")
