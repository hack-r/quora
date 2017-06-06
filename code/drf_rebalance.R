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
mean(as.numeric(train.model$is_duplicate))

# CV Split ----------------------------------------------------------------
# train.model <- random(train, over)
# train.valid <- train[!train$qid1 %in% train.model$qid1,]

# DRF ---------------------------------------------------------------------
set.seed(1)

train.model <- as.data.frame(train.model)
#train.valid <- as.data.frame(train.valid)
test        <- as.data.frame(test)

# Define predictors
x_names <- colnames(train.model)
x_names <- setdiff(x_names, c("id", "qid1", 
                              "qid2", "question1","question2",
                              "q1_clean", "q2_clean",
                              "flag_500_1000"#,
#                               "q1_flag_book",
#                               "q1_flag_can",
#                               "q1_flag_day",
#                               "q1_flag_ever",
#                               "q1_flag_get",
#                               "q1_flag_good",
#                               "q1_flag_how",
#                               "q1_flag_india",
#                               "q1_flag_life",
#                               "q1_flag_math",
#                               "q1_flag_new",
#                               "q1_flag_trump",
#                               "q1_flag_what",
#                               "q1_flag_when",
#                               "q1_flag_where",
#                               "q1_flag_which",
#                               "q1_flag_who",
#                               "q1_flag_why",
#                               "q2_flag_book",
#                               "q2_flag_can",
#                               "q2_flag_day",
#                               "q2_flag_ever",
#                               "q2_flag_get",
#                               "q2_flag_good",
#                               "q2_flag_how",
#                               "q2_flag_india",
#                               "q2_flag_life",
#                               "q2_flag_math",
#                               "q2_flag_new",
#                               "q2_flag_trump",
#                               "q2_flag_what",
#                               "q2_flag_when",
#                               "q2_flag_where",
#                               "q2_flag_which",
#                               "q2_flag_who",
#                               "q2_flag_why"
                              )) #,


# H2O Setup
system("java -Xmx20g -jar E://Jason//h2o//h2o.jar", wait = F)
h2o.init(nthreads = -1)

# Send data to H2O
trainHex <- as.h2o(train.model[,x_names]) #validHex <- as.h2o(train.valid[,x_names])
scoreHex <- as.h2o(test[,setdiff(x_names,"is_duplicate")])

trainHex[,"is_duplicate"] <- as.factor(trainHex[,"is_duplicate"])

drf <- h2o.grid(algorithm="randomForest",
                x= x_names,
                y="is_duplicate",
                training_frame = trainHex,
                ignore_const_cols = T,
                #ntrees = 200,
                max_depth = 18,
                mtries=10,
                sample_rate=.632,
                stopping_metric = "logloss",
                hyper_params=list(ntrees = c(200)#,
                  #max_depth=c(18)
                  #,mtries=c(10)
                  #,sample_rate=c(0.632,)
                  #col_sample_rate_per_tree=c(.8,1)
                )) # 21md better in CV but worse on public LB than 18md

summary(drf)
drf.best <- h2o.getModel("Grid_DRF_RTMPNA_9_model_R_1496282637970_2_model_0")

summary(drf.best)
h2o.varimp(drf.best)
vi <- as.data.frame(h2o.varimp(drf.best)); #saveRDS(vi, "vi_reusable.RDS")

# Validation
# valid_pred <- predict(drf.best, validHex)
# table(train.valid$is_duplicate)
# round(colSums(as.data.frame(valid_pred[,2:3])))

# MultiLogLoss(as.data.frame(valid_pred[,2:3]),train.valid$is_duplicate) 

# Scoring
scoreHex[,"same_word_count"] <- as.factor(scoreHex[,"same_word_count"]) # if needed

pred <- predict(drf.best, scoreHex)
pred <- as.data.frame(pred)

sample$is_duplicate <- pred$p1

fwrite(sample, "drf_18deep_400trees_rebalance_tfidf_enhanced_absh.csv", row.names = F) #
saveRDS(sample, "may31_drf.RDS")
