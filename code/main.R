
# Libraries and Options ---------------------------------------------------
options(scipen=20)
setwd("data")
pacman::p_load(data.table, dplyr, h2o, h2oEnsemble, lightgbm, Matrix, Metrics, 
               MLmetrics, mlr, RWeka, speedglm,
               stringr, stringdist, sqldf, tau, tidytext, tm, xgboost)

# Load Raw Data -----------------------------------------------------------
sample <- fread("sample_submission.csv")
train  <- fread("train.csv")
test   <- fread("test.csv")

train_feats <- fread("train_features.csv")
test_feats  <- fread("test_features.csv")


# TM ----------------------------------------------------------------------
source("..//code//TM.R") # tm package text mining
 
# EDA ---------------------------------------------------------------------
source("..//code//EDA.R")

# Feature Engineering -----------------------------------------------------
source("..//code//FE.R")
source("..//code//tf_idf.R")

# Model -------------------------------------------------------------------
source("..//code//drf.R")
source("..//code//xgb.R")

# Special -----------------------------------------------------------------
# Read in final data sets from Python
x_test    <- fread("x_test_ds.csv")
x_test$V1 <- NULL

x_train <- fread("x_train_ds2.csv")
y_train <- fread("y_train2.csv")
x_valid <- fread("x_valid_ds2.csv")
y_valid <- fread("y_valid2.csv")


# Find features to add
intersect(colnames(x_test), vi$variable)
setdiff(vi$variable, colnames(x_test))

vi$included <- ifelse(vi$variable %in% colnames(x_test),1,0)
to_add      <- vi$variable[vi$scaled_importance > .02 & vi$included == 0]
to_add      <- setdiff(to_add, c("jaccard_distance", "nchar_delta", "q1_nchar", 
                                 "q2_nchar", "q1_word_count", "q1_word_count"))
table(to_add %in% colnames(test))

# Make sure data sets sorted the same
head(test$wmd)
head(x_test$wmd)

# Add the features
tmp     <- as.data.frame(test)
tmp     <- tmp[,to_add]
x_test2 <- cbind(x_test, tmp)
rm(x_test, tmp); gc()

tmp           <- as.data.frame(train)
tmp           <- as.data.table(tmp[,c(to_add,"id")])
tmp$id        <- as.integer(tmp$id)
x_train2      <- merge(x_train, tmp,by.x="V1",by.y="id")
x_valid2      <- merge(x_valid, tmp,by.x="V1",by.y="id")
rm(x_train, x_valid,tmp); gc()

# Write out new data sets
fwrite(x_test2, "x_test2.csv") # FULL test data set with all features
fwrite(x_train2, "x_train2.csv") # raw train ds + feats missing in Py
fwrite(x_valid2, "x_valid2.csv") # raw train ds + feats missing in Py

x_valid2 <- fread("x_valid2.csv")
x_test2  <- fread("x_test2.csv")
x_train2 <- fread("x_train2.csv")

# Model Stacking ----------------------------------------------------------
mod_drf   <- fread("drf_18deep_400trees_rebalance_tfidf_enhanced_top60.csv")
mod_ens   <- readRDS("h2o_ensemble.RDS")
mod_py    <- fread("xgb_seed12357_n500.csv")
mod_py2   <- fread("xgb_seed12357_n315_may30.csv")
mod_stack <- fread("stack_drf0_rxgb0_ens20_pyxgb80_enhanced_tm.csv")
mod_be    <- readRDS("big_ens.RDS")
mod_be2   <- readRDS("big_ens2.RDS")
mod_base  <- readRDS("big_ens_base.RDS")
mod_base2 <- readRDS("big_ens_base2.RDS")
mod_nn    <- mod_base$h2o.deeplearning.5
mod_drf1  <- mod_base2$h2o.randomForest.1
mod_py3   <- fread("T://RNA//Baltimore//Jason//ad_hoc//quora//data//xgb_py_absh.csv")
#mod_drf0  <- readRDS("may31_drf.RDS")  
#mod_base2$h2o.randomForest.2
mod_lgb   <- readRDS("lgb_pred_score.RDS"); mod_lgb[mod_lgb<0] <- 0
mod_smts  <- fread("sub_minimal_test_set.csv")
mod_new_lgb <- readRDS("new_lgb_pred_score_dart.RDS")
mod_new_drf <- readRDS("")


# Model A: py XGB ts .01 (2nd to newest feature set)
# Model B: Old best stack (stack_drf0_rxgb0_ens20_pyxgb80_enhanced_tm.csv)
# Model C: DRF with Newest features  - .36/.37 mean oversampling
# Model D: py XGB ts .01 Newest feature set
# Model E: DRF with Newest features  - .165 mean oversampling

stack <- data.frame(test_id = mod_py3$test_id, is_duplicate = 
                      mod_smts$is_duplicate   * .995 +  # Model A
                      mod_stack$is_duplicate  * .005 +  # Model B
                      mod_new_drf             * .0   +  # Model C
                      mod_new_py$is_duplicate * .0      # Model D
                      )

fwrite(stack, "a995_b005_c00_d00.csv") 

# A 0 B 0    C 1 D 0    LB:
# A 0 B 0    C 0 D 1    LB:
# A 0 B .005 C 0 D .995 LB: 

# Older -------------------------------------------------------------------
# Model A: Python XGB with Jaccard and Absh. Feats. LB: 0.15845
# Model B: Old H2O Ensemble with glm.1-3, DRF 1-4, gbm.1-6 (- #2), NN 1+5-7, DRF metalearner
# Model C: New H2O Ensemble, same as above but with added feats
# Model D: Best stack thusfar (stack_drf0_rxgb0_ens20_pyxgb80_enhanced_tm.csv)
# Model E: Prior DRF model retrained with newest features
# Model F: R LGB
# Model G: Homebrew py XGB ts .01


#  A .00,   B .00, C .00, D .005 G .995     -- LB 0.15828
#  A .995,  B .00, C .00, D .005            -- LB 0.15830
#  A .996,  B .00, C .00, D .004            -- LB 0.15830
#  A .995,  B .00, C .002,D .003            -- LB 0.15831
#  A .99,   B .00, C .00, D .01             -- LB 0.15841
#  A .99,   B .00, C .01, D .00             -- LB 0.15842
#  A .99,   B .00, C .00, D .00 E .01       -- LB 0.15845
#  A 1,     B .00, C .00, D .00             -- LB 0.15845
#  A .98,   B .00, C .00, D .02             -- LB 0.15884
#  A .99,   B .00, C .00, D .00 E .00 F .01 -- LB 0.15992
#  A .95,   B .00, C .05, D .00             -- LB 0.16070
#  A .85,   B .00, C .00, D .15             -- LB 0.17036


# Model AA: UPDATED MAY 30 New Py XGB (xgb_seed12357_n315.csv)
# Model A: New Py XGB (xgb_seed12357_n315.csv)
# Model B: Old Py XGB (xgb_seed12357_n500.csv)
# Model C: H2O Ensemble with DRF, NN, GLM base learners, DRF metalearner, word shares (h2o_ensemble.RDS)
# Model D: Best stack thusfar (stack_drf0_rxgb0_ens20_pyxgb80_enhanced_tm.csv)
# Model E: H2O Ensemble with glm.1-3, DRF 1-4, gbm.1-6 (- #2), NN 1+5-7, DRF metalearner
# Model F: NN (h2o.deeplearning.5) from Model E


# AA .60  B. .00 C .00 D.40 E .00 F .00   -- LB 0.32649
# AA .80  B. .00 C .00 D.20 E .00 F .00   -- LB 0.32653
# AA .95  B. .00 C .00 D.05 E .00 F .00   -- LB 0.32784
# AA .40  B. .00 C .00 D.60 E .00 F .00   -- LB 0.32798
# AA 1    B. .00 C .00 D.00 E .00 F .00   -- LB 0.32867
#  A .00, B .00, C .00, D .99 E .00 F .01 -- LB 0.33479
#  prior models benchmark                 -- LB 0.33483 # 20/80 DRF-based ensemble and py xgb
#  A .00, B .80, C .00, D .00 E .20       -- LB 0.33492
#  A .80, B .00, C .20, D .00             -- LB 0.36236 ????
#  A 1,   B .00, C .00, D .00             -- LB 0.37x   ???? doesn't match LB results!?!


# Older results -----------------------------------------------------------
## Model A: H2O Ensemble with DRF, NN, GLM base learners, DRF metalearner, word shares, new flags/transforms
## Model B: Python XGB, 500 rounds, 5md, rebalanced, word share features
## Model C: H2O GLM ensemble
## Model D: DRF 400 trees, 60 feats, some new transformations
## Model E: Model A H2O ensemble but with the new transformations (all 119 var)
## Model F: Same as E but dropped some of the newest vars, to see if any might work

# prior models benchmark            -- LB 0.33483 # 20/80
# A .00, B .80, C .00, D .00 E: .20 -- LB 0.33583 # new vars are harmful
# A .00, B .80, C .00, D .00 F: .20 -- LB 0.33595 # new vars are harmful
# A .21, B .79, C .00, D .00        -- LB 0.33728 # suspiciously worse for a 1% change
# A .19, B .80, C .00, D .01        -- LB 0.33728
# A .19, B .81, C .00, D .00        -- LB 0.33734 # suspiciously worse for a 1% change... apparently 20/80 is the magic ratio?
# A .19, B .80, C .01, D .00        -- LB 0.33737 # GLM ensemble is garbage

## Model A: H2O DRF, rebalanced, 18md, 200 trees, with all flags, tfidf, word shares
## Model B: H2O Ensemble with gbm1, gbm3 base learners, DRF metalearner, word shares
## Model C: H2O Ensemble with DRF, NN, GLM base learners, DRF metalearner, word shares
## Model D: Python XGB, 500 rounds, 5md, rebalanced, word share features

# A .00, B .00, C .20, D .80 -- LB 0.33483 # DRF ensemble .00265 better than DRF
# A .00, B .01, C .19, D .80 -- LB 0.33700 # gbm1 + gbm3 are no good
# prior models benchmark     -- LB 0.33730
# A .20, B .00, C .00, D .80 -- LB 0.33748

## Model A: H2O DRF, rebalanced, 18md, 200 trees, with all flags and tfidf
## Model B: R XGB, NOT rebalanced, 17995 rounds/trees, eta .01
## Model C: Speed GLM v4
## Model D: H2O Ensemble with DRF, NN, GLM base learners, DRF metalearner
## Model E: Python XGB, 500 rounds, 5md, rebalanced, word share features

# A .00, B .00, C 0, D .20, E .80 -- LB 0.33730 # B (old xgb) no longer relevant
# A .00, B .03, C 0, D .17, E .80 -- LB 0.33789 # ens is similar but > drf
# A .14, B .03, C 0, D .03, E .80 -- LB 0.33865 
# A .20, B .03, C 0, D .03, E .74 -- LB 0.33875 # too little E
# A .09, B .03, C 0, D .03, E .85 -- LB 0.33883
# A .07, B .02, C 0, D .02, E .89 -- LB 0.33907
# A .07, B .02, C 0, D .01, E .90 -- LB 0.33927
# A .02, B .05, C 0, D .04, E .89 -- LB 0.33950
# A .0,  B .0,  C 0, D 0,   E 1   -- LB 0.34153

# old:
# xgb .14 drf_rebalanced_18d_tf_af   .86         -- LB 0.37075
# xgb .13 drf_rebalanced_18d_tf_af   .87         -- LB 0.37075
# xgb .15 drf_rebalanced_18d_tf_af   .85         -- LB 0.37078
# xgb .13 drf_rebalanced_18d_tf_af   .86 glm .01 -- LB 0.37079
# xgb .14 drf_rebalanced_18d_tf_af   .85 glm .01 -- LB 0.37079
# xgb .16 drf_rebalanced_18d_tf_af   .84         -- LB 0.37082
# xgb .14 drf_rebalanced_18d_tf_af   .86 abs     -- LB 0.37098
# xgb .14 drf166rebalanced_18d_tf_af .86         -- LB 0.37112
# xgb .15 drf_rebalanced_18d_tfidf   .85         -- LB 0.37188
# xgb .15 drf_rebalanced_18d         .85         -- LB 0.37194
# xgb .10 drf_rebalanced_18d         .90         -- LB 0.37199
# xgb .15 drf_rebalanced_16d         .85         -- LB 0.37208
# xgb .20 drf_rebalanced_18d_tfidf   .80         -- LB 0.37227
# xgb .00 drf_rebalanced_18d_tf_af   1.0         -- LB 0.37263
# xgb .15 drf_rebalanced_21d         .85         -- LB 0.37279
# xgb .05 drf_rebalanced_16d         .95         -- LB 0.37305
# xgb .30 drf_rebalanced_16d         .70         -- LB 0.37412
# xgb .00 drf_rebalanced_16d         1.0         -- LB 0.37441
# xgb .15 drf_rebalanced_16d_ns      .85         -- LB 0.38285