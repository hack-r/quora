library(h2oEnsemble)
library(SuperLearner)  # For metalearner such as "SL.glm" -- never got this to work tho
library(cvAUC)         # Used to calculate test set AUC (requires version >=1.0.1 of cvAUC)

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

# Data --------------------------------------------------------------------
set.seed(1)

train.valid <- as.data.frame(train.model[300001:nrow(train.model),])
train.model <- as.data.frame(train.model[1:300000,])
test        <- as.data.frame(test)

# Define predictors
x_names <- colnames(train.model)
x_names <- setdiff(x_names, c("id", "qid1", 
                              "qid2", "question1","question2",
                              "q1_clean", "q2_clean")) 
#x_names <- c(x_names[1:104],"flag_lose_weight","flag_make_money")

# H2O Setup
system("java -Xmx20g -jar E://Jason//h2o//h2o.jar", wait = F)
h2o.init(nthreads = -1)

# Send data to H2O
trainHex <- as.h2o(train.model[,x_names]) 
validHex <- as.h2o(train.valid[,x_names])
scoreHex <- as.h2o(test[,setdiff(x_names,"is_duplicate")])

trainHex[,"is_duplicate"] <- as.factor(trainHex[,"is_duplicate"])
validHex[,"is_duplicate"] <- as.factor(validHex[,"is_duplicate"])

scoreHex[,"same_word_count"] <- as.factor( scoreHex[,"same_word_count"])

# Model -------------------------------------------------------------------
family <- "binomial"

# Create a custom base learner library & specify the metalearner
h2o.randomForest.1 <- function(..., ntrees = 200, nbins = 100, seed = 1, max_depth = 18) h2o.randomForest.wrapper(..., ntrees = ntrees, nbins = nbins, seed = seed, max_depth = max_depth)
h2o.deeplearning.1 <- function(..., hidden = c(500,500), activation = "Rectifier", seed = 1)  h2o.deeplearning.wrapper(..., hidden = hidden, activation = activation, seed = seed)
#h2o.deeplearning.2 <- function(..., hidden = c(200,200,200), activation = "Tanh", seed = 1)  h2o.deeplearning.wrapper(..., hidden = hidden, activation = activation, seed = seed)
learner     <- c("h2o.randomForest.1", "h2o.deeplearning.1", "h2o.glm.wrapper")
metalearner <- c("SL.glm") # doesn't work

# Train the ensemble using 4-fold CV to generate level-one data
# More CV folds will take longer to train, but should increase performance

fit <- h2o.ensemble(x              = setdiff(x_names, "is_duplicate"),
                    y              = "is_duplicate",
                    training_frame = trainHex,
                    metalearner    ="h2o.randomForest.wrapper",
                    learner        = learner)

# Generate predictions on the validation set
pred <- predict(fit, validHex)
labels <- as.data.frame(validHex[,c(y)])[,1]


# Ensemble test AUC 
AUC(predictions=as.data.frame(pred$pred)[,1], labels=labels)
# 0.7681649


# Base learner test AUC (for comparison)
L <- length(learner)
sapply(seq(L), function(l) AUC(predictions = as.data.frame(pred$basepred)[,l], labels = labels)) 


# Scoring -----------------------------------------------------------------
scoreHex[,"same_word_count"] <- as.factor(scoreHex[,"same_word_count"]) # if needed

scores <- predict(fit, scoreHex)
scores <- as.data.frame(scores$pred)

sample$is_duplicate <- scores$p1

fwrite(sample, "h2o_ensemble_newvar_somenew.csv", row.names = F)
saveRDS(sample, "h2o_ensemble_newvar_somenew.RDS")

# Alternate Model 1 -------------------------------------------------------
family <- "binomial"

# Create a custom base learner library & specify the metalearner
h2o.glm.1 <- function(..., alpha = 0.0, link="logit") h2o.glm.wrapper(..., alpha = alpha, link = link)
h2o.glm.2 <- function(..., alpha = 0.5) h2o.glm.wrapper(..., alpha = alpha)
h2o.glm.3 <- function(..., alpha = 1.0) h2o.glm.wrapper(..., alpha = alpha)
h2o.randomForest.1 <- function(..., ntrees = 200, nbins = 100, seed = 1, max_depth = 18) h2o.randomForest.wrapper(..., ntrees = ntrees, nbins = nbins, seed = seed, max_depth = max_depth)
h2o.randomForest.2 <- function(..., ntrees = 200, sample_rate = 0.75, seed = 1) h2o.randomForest.wrapper(..., ntrees = ntrees, sample_rate = sample_rate, seed = seed)
h2o.randomForest.3 <- function(..., ntrees = 200, sample_rate = 0.85, seed = 1) h2o.randomForest.wrapper(..., ntrees = ntrees, sample_rate = sample_rate, seed = seed)
h2o.randomForest.4 <- function(..., ntrees = 200, nbins = 50, balance_classes = TRUE, seed = 1) h2o.randomForest.wrapper(..., ntrees = ntrees, nbins = nbins, balance_classes = balance_classes, seed = seed)
h2o.gbm.1 <- function(..., ntrees = 100, seed = 1) h2o.gbm.wrapper(..., ntrees = ntrees, seed = seed)
h2o.gbm.2 <- function(..., ntrees = 100, nbins = 50, seed = 1) h2o.gbm.wrapper(..., ntrees = ntrees, nbins = nbins, seed = seed)
h2o.gbm.3 <- function(..., ntrees = 250, max_depth = 10, seed = 1) h2o.gbm.wrapper(..., ntrees = ntrees, max_depth = max_depth, seed = seed)
h2o.gbm.4 <- function(..., ntrees = 100, col_sample_rate = 0.6, seed = 1) h2o.gbm.wrapper(..., ntrees = ntrees, col_sample_rate = col_sample_rate, seed = seed)
h2o.gbm.5 <- function(..., ntrees = 500, col_sample_rate = 0.6, seed = 1) h2o.gbm.wrapper(..., ntrees = ntrees, col_sample_rate = col_sample_rate, seed = seed)
h2o.gbm.6 <- function(..., ntrees = 999, col_sample_rate = 0.6, seed = 1) h2o.gbm.wrapper(..., ntrees = ntrees, col_sample_rate = col_sample_rate, seed = seed)
h2o.gbm.7 <- function(..., ntrees = 100, max_depth = 4, seed = 1) h2o.gbm.wrapper(..., ntrees = ntrees, max_depth = max_depth, seed = seed)
h2o.gbm.8 <- function(..., ntrees = 100, max_depth = 5, seed = 1) h2o.gbm.wrapper(..., ntrees = ntrees, max_depth = max_depth, seed = seed)
h2o.deeplearning.1 <- function(..., hidden = c(500,500), activation = "Rectifier", epochs = 50, seed = 1)  h2o.deeplearning.wrapper(..., hidden = hidden, activation = activation, seed = seed)
h2o.deeplearning.2 <- function(..., hidden = c(200,200,200), activation = "Tanh", epochs = 50, seed = 1)  h2o.deeplearning.wrapper(..., hidden = hidden, activation = activation, seed = seed)
h2o.deeplearning.3 <- function(..., hidden = c(500,500), activation = "RectifierWithDropout", epochs = 50, seed = 1)  h2o.deeplearning.wrapper(..., hidden = hidden, activation = activation, seed = seed)
h2o.deeplearning.4 <- function(..., hidden = c(500,500), activation = "Rectifier", epochs = 50, balance_classes = TRUE, seed = 1)  h2o.deeplearning.wrapper(..., hidden = hidden, activation = activation, balance_classes = balance_classes, seed = seed)
h2o.deeplearning.5 <- function(..., hidden = c(100,100,100), activation = "Rectifier", epochs = 50, seed = 1)  h2o.deeplearning.wrapper(..., hidden = hidden, activation = activation, seed = seed)
h2o.deeplearning.6 <- function(..., hidden = c(50,50), activation = "Rectifier", epochs = 50, seed = 1)  h2o.deeplearning.wrapper(..., hidden = hidden, activation = activation, seed = seed)
h2o.deeplearning.7 <- function(..., hidden = c(100,100), activation = "Rectifier", epochs = 50, seed = 1)  h2o.deeplearning.wrapper(..., hidden = hidden, activation = activation, seed = seed)

learner <- c(
             "h2o.glm.1",
             "h2o.glm.2",
             "h2o.glm.3",
             "h2o.randomForest.1", 
             "h2o.randomForest.2",
             "h2o.randomForest.3",
             "h2o.randomForest.4",
             "h2o.gbm.1", 
             "h2o.gbm.3", 
             "h2o.gbm.4",
             "h2o.gbm.5",
             "h2o.gbm.6",
             "h2o.deeplearning.1",
             "h2o.deeplearning.5",
             "h2o.deeplearning.6", 
             "h2o.deeplearning.7"
             )

metalearner <- c("h2o.randomForest.wrapper")

# Train the ensemble using 4-fold CV to generate level-one data
# More CV folds will take longer to train, but should increase performance
s <- System.time()
  fit <- h2o.ensemble(x              = setdiff(x_names, "is_duplicate"),
                      y              = "is_duplicate",
                      training_frame = trainHex,
                      metalearner    ="h2o.randomForest.wrapper",
                      learner        = learner)
System.time()-s
saveRDS(fit, "big_ensemble2.RDS")

# validation
scores <- predict(fit, validHex)
scores <- as.data.frame(scores$pred)
mean(scores$p1)

cat("ground truth:")
mean(as.numeric(train.valid$is_duplicate))

# scoring
scores <- predict(fit, scoreHex)
score  <- as.data.frame(scores$pred)
base   <- as.data.frame(scores$basepred)

sample$is_duplicate <- score$p1

mean(sample$is_duplicate)
summary(sample$is_duplicate)

saveRDS(sample, "big_ens2.RDS")
fwrite(sample,  "big_ens2.csv", row.names = F)

saveRDS(base, "big_ens_base2.RDS")
