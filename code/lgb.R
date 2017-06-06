
# Libraries and Options ---------------------------------------------------
options(scipen = 20)
set.seed(1)
library(lightgbm)

# Data --------------------------------------------------------------------
# Convert dataset to sparse format
print("get variable names")
x_names <- colnames(train.model) # make sure it's from drf_rebalance.R
x_names <- setdiff(x_names, c("is_duplicate",
                              "id", 
                              "qid1", 
                              "qid2", 
                              "question1",
                              "question2",
                              "q1_clean", 
                              "q2_clean",
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

test                        <- as.data.table(test)
train.model                 <- as.data.table(train.model)
train.model$same_word_count <- as.numeric(train.model$same_word_count)
test$same_word_count        <- as.numeric(test$same_word_count)

t1_sparse <- Matrix(as.matrix(train.model[1:400000,x_names, with=FALSE]), sparse=TRUE)
s1_sparse <- Matrix(as.matrix(test[,x_names, with=FALSE]), sparse=TRUE)
v1_sparse <- Matrix(as.matrix(train.model[400001:nrow(train.model),x_names, with=FALSE]), sparse=TRUE)

labels             <- train$is_duplicate[1:400000]
valid.labels       <- train$is_duplicate[400001:nrow(train)]



# Train on Sparse Mats ----------------------------------------------------
print("Training lightgbm with sparseMatrix")
bst <- lightgbm(data = t1_sparse,
                label = labels,
                num_leaves = 4,
                nrounds=100,
                learning_rate = 1,
                boosting="dart",
                objective = "binary")



# LGB Data Sets for CV ----------------------------------------------------
dtrain <- lgb.Dataset(data = as.matrix(train.model[1:400000,x_names, with=FALSE]), 
                      label = as.numeric(labels),
                      free_raw_data = FALSE
                      #,colnames = colnames(train_mat),
                      #categorical_feature = which(colnames(train_mat) %in% cf)
                      )

lgb.Dataset.construct(dtrain)
saveRDS(dtrain, "dtrain.RDS")

dtest     <- lgb.Dataset.create.valid(dtrain,
                         data                = as.matrix(train.model[400001:nrow(train.model),x_names, with=FALSE]),
                         label               = as.numeric(valid.labels))
lgb.Dataset.construct(dtest)


lgb.Dataset.save(dtrain, "dtrain.buffer")
lgb.Dataset.save(dtest, "dtest.buffer")

#--------------------Using validation set-------------------------
# valids is a list of lgb.Dataset, each of them is tagged with name
# valids allows us to monitor the evaluation result on all data in the list
valids <- list(train = dtrain, test = dtest)

# We can change evaluation metrics, or use multiple evaluation metrics
print("Train lightgbm using lgb.train with valids, watch logloss and error")
bst <- lgb.train(data = dtrain,
                 valids = valids,
                 eval = c("binary_logloss"), #,mean_absolute_error
                 nthread = 2,
                 #num_leaves = 8,
                 learning_rate = .02,
                 nrounds = 2500,
                 boosting='dart',
                 objective = "binary",
                 #max_bin=9, #100
                 #metric='l2',
                 sub_feature=.5,
                 bagging_fraction=.85, #.85
                 bagging_freq=1,
                 min_data=100,
                 min_hessian=.05,
                 #max_depth = 20,
                 verbose = 1,
                 early_stopping_rounds = 30,
                 metric = "binary_logloss"
                 #,categorical_feature = cf
) #
bst$best_iter

# Validation prediction / error
label.valid = getinfo(dtest, "label")
pred.valid <- predict(bst, as.matrix(valid_mat))
err        <- Metrics::mae(label.valid, pred.valid)
print(paste("test-error=", err)) # 0.0655485186283588

# OOS Scoring
t <- Sys.time()
score <- predict(bst, as.matrix(s1_sparse))
Sys.time()-t #

# Save Predictions --------------------------------------------------------
saveRDS(pred.valid, "lgb_pred_valid.RDS")
saveRDS(score,      "lgb_pred_score.RDS")
fwrite(as.data.frame(score), "score.csv")

#--------------------Save and load models-------------------------
# Save model to binary local file
lgb.save(bst, "lightgbm.model.v1")
