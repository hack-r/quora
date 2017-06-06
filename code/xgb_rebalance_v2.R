
# Libraries and Options ---------------------------------------------------
options(scipen = 20)
set.seed(1)

# Data --------------------------------------------------------------------
# Convert dataset to sparse format
print("get variable names")
vi      <- readRDS("vi_reusable.RDS")
x_names <-  vi$variable[1:20]
x_names <- c(x_names, "tfidf")

print("converting data to sparse format")
test                        <- as.data.table(test)
train.model                 <- as.data.table(train.model)
train.model$same_word_count <- as.numeric(train.model$same_word_count)
test$same_word_count        <- as.numeric(test$same_word_count)

t1_sparse <- Matrix(as.matrix(train.model[,x_names, with=FALSE]), sparse=TRUE)
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
              eta = .01, # .01 .02
              gamma = 1,
              max_depth = 6,
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
Sys.time() - tme #  1.7 hours for 1800 rounds

# Scoring
sPreds              <- as.data.table(t(matrix(predict(xgb2, dtest), nrow=2, ncol=nrow(dtest))))
sample$is_duplicate <- sPreds$V2
fwrite(data.table(sample),
       "xgb_rebalance_v2_lean.csv")
