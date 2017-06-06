
# Libraries and Options ---------------------------------------------------
options(scipen = 20)
set.seed(1)

# Data --------------------------------------------------------------------
# Convert dataset to sparse format
print("get variable names")
x_names <- colnames(train.model)
x_names <- setdiff(x_names, c("id", "qid1", "is_duplicate",
                              "qid2", "question1","question2",
                              "q1_clean", "q2_clean",
                              "q1_flag_book",
                              "q1_flag_can",
                              "q1_flag_day",
                              "q1_flag_ever",
                              "q1_flag_get",
                              "q1_flag_good",
                              "q1_flag_how",
                              "q1_flag_india",
                              "q1_flag_life",
                              "q1_flag_math",
                              "q1_flag_new",
                              "q1_flag_trump",
                              "q1_flag_what",
                              "q1_flag_when",
                              "q1_flag_where",
                              "q1_flag_which",
                              "q1_flag_who",
                              "q1_flag_why",
                              "q2_flag_book",
                              "q2_flag_can",
                              "q2_flag_day",
                              "q2_flag_ever",
                              "q2_flag_get",
                              "q2_flag_good",
                              "q2_flag_how",
                              "q2_flag_india",
                              "q2_flag_life",
                              "q2_flag_math",
                              "q2_flag_new",
                              "q2_flag_trump",
                              "q2_flag_what",
                              "q2_flag_when",
                              "q2_flag_where",
                              "q2_flag_which",
                              "q2_flag_who",
                              "q2_flag_why")) 

print("converting data to sparse format")

train$same_word_count <- as.numeric(train$same_word_count)
test                  <- as.data.table(test)

test$same_word_count  <- as.numeric(test$same_word_count)
t1_sparse <- Matrix(as.matrix(train[,x_names, with=FALSE]), sparse=TRUE)
s1_sparse <- Matrix(as.matrix(test[,x_names, with=FALSE]), sparse=TRUE)

print("converting data into xgb format")
dtrain <- xgb.DMatrix(data=t1_sparse, label=train$is_duplicate)
dtest  <- xgb.DMatrix(data=s1_sparse)
saveRDS(dtrain, "dtrain.RDS")
saveRDS(dtest, "dtest.RDS")

# Model -------------------------------------------------------------------
param <- list(booster="gbtree",
              objective="multi:softprob",
              eval_metric="mlogloss",
              #nthread=13,
              num_class=2,
              eta_decay = .99,
              eta = .01,
              gamma = 1,
              max_depth = 4,
              min_child_weight = .9,
              subsample = .7,
              colsample_bytree = .5
)

#CV / tree selection
set.seed(1)
(tme <- Sys.time())
xgb2cv <- xgb.cv(data = dtrain,
                 params = param,
                 nrounds = 22000,
                 maximize=FALSE,
                 prediction = TRUE,
                 #folds = cvFoldsList,
                 nfold = 3,
                 #print_every_n = 50,
                 early_stopping_round=10)
Sys.time() - tme

# Final Model
watch       <- list(dtrain=dtrain)
xgb2cv$dt$j <- xgb2cv$dt$test.mlogloss.mean+xgb2cv$dt$test.mlogloss.std
nrounds     <- which(xgb2cv$dt$j==min(xgb2cv$dt$j))[1]; nrounds
(tme <- Sys.time())
xgb2        <- xgb.train(data    = dtrain,
                         params  = param,
                         nrounds = nrounds 
)
Sys.time() - tme #  for 1800 rounds

# Scoring
sPreds              <- as.data.table(t(matrix(predict(xgb2, dtest), nrow=2, ncol=nrow(dtest))))
sample$is_duplicate <- sPreds$V2
fwrite(data.table(sample),
       "xgb_22000_eta01.csv")
