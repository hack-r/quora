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
mod_lgb   <- readRDS("lgb_pred_score.RDS")

stack <- data.frame(test_id = mod_py3$test_id, is_duplicate = 
                      mod_py3$is_duplicate    * .0  +  # Model A
                      mod_be$is_duplicate     * .0  +  # Model B
                      mod_be2$is_duplicate    * .0  +  # Model C
                      mod_stack$is_duplicate  * .005+  # Model D
                      mod_drf1                * .0  +  # Model E
                      mod_lgb                 * .0  +  # Model F
                      mod_smts$is_duplicate   * .995   # Model G
)

stack$is_duplicate <- mod_py3$is_duplicate
plot(density(stack$is_duplicate),xlim=c(.2,.55))
lines(density(mod_stack$is_duplicate),xlim=c(.2,.55),col="green")

stack$is_duplicate[ stack$is_duplicate > .3] <- 
  stack$is_duplicate[stack$is_duplicate > .3] * .99 +
  mod_stack$is_duplicate[stack$is_duplicate > .3] * .01

fwrite(stack, "crazy9.csv") 

# crazy6 LB: 0.15821
# crazy4 LB: 0.15822
# crazy7 LB: 0.15832
# crazy8 LB: 0.15832
# crazy5 LB: 0.15843

# crazy 7 def:
stack <- data.frame(test_id = mod_py3$test_id, is_duplicate = 
                      mod_py3$is_duplicate    * .0  +  # Model A
                      mod_be$is_duplicate     * .0  +  # Model B
                      mod_be2$is_duplicate    * .0  +  # Model C
                      mod_stack$is_duplicate  * .005+  # Model D
                      mod_drf1                * .0  +  # Model E
                      mod_lgb                 * .0  +  # Model F
                      mod_smts$is_duplicate   * .995   # Model G
)

#stack$is_duplicate <- mod_py3$is_duplicate
#plot(density(mod_py3$is_duplicate),xlim=c(0,.25))
#lines(density(mod_stack$is_duplicate),xlim=c(0,.25),col="green")

stack$is_duplicate[ stack$is_duplicate > .25] <- 
  stack$is_duplicate[stack$is_duplicate > .25] * .995 +
  mod_stack$is_duplicate[stack$is_duplicate > .25] * .005

# crazy 6 definition
stack$is_duplicate[ stack$is_duplicate > .25] <- 
  stack$is_duplicate[stack$is_duplicate > .25] * .995 +
  mod_stack$is_duplicate[stack$is_duplicate > .25] * .005


# crazy4 definition ####
stack$is_duplicate[ stack$is_duplicate > .2] <- 
  stack$is_duplicate[stack$is_duplicate > .2] * .995 +
  mod_stack$is_duplicate[stack$is_duplicate > .2] * .005

# crazy5 def
stack$is_duplicate[ stack$is_duplicate > .2] <- 
  stack$is_duplicate[stack$is_duplicate > .2] * .98 +
  mod_stack$is_duplicate[stack$is_duplicate > .2] * .02
