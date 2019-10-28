# GRADIENT BOOSTING
# Steering ----------------------------------------------------------------
setwd("J:\\Studium\\Master\\Masterthesis")

# Tuning stage
benchmarking_gb <- TRUE
finetuning_gb <- FALSE

# First-stage tuning setup
npar_gb <- 3      # Number of tuning parameters


# Finetuning setup
# Final learner
learner_gb <- makeLearner(cl = "regr.xgboost", 
                          id = "xgboost", 
                          predict.type = "response", 
                          objective = "reg:squarederror",             # squared loss function
                          colsample_bytree = 1,                       # equivalent to no bagging. I.e. all predictors are used building trees
                          min_child_weight = 1                        # minimum number of instances needed to be in each node
)

# Name of tuning parameters
ntree_gb <- "nrounds" 
rate_gb <- "eta"
depth_gb <- "max_depth"

# Finetuning parameter threshholds
ntree_low <- 300
ntree_up <- 600
depth_low <- 2
depth_up <- 8
rate_low <- 0.001   # check trafo
rate_up <- 0.01



#--------------------------------------------------------------------------
if(benchmarking_gb){
# Algorithm benchmarking --------------------------------------------------
listLearners(task_training) %>% 
  as_tibble() %>% 
  filter(str_detect(name, regex("boost", ignore_case = TRUE))) %>% 
  select(class, name, short.name, package, note, se, featimp)
## Learners ===============================================================


learner_gb.gbm <- makeLearner(cl = "regr.gbm", 
                              id = "gbm", 
                              predict.type = "response",
                              distribution = "gaussian",                  # squared loss function
                              bag.fraction = 1,                           # equivalent to no bagging. I.e. all predictors are used building trees
                              n.minobsinnode = 1,                         # minimum number of instances needed to be in each node
                              keep.data = FALSE)

learner_gb.xgb <- makeLearner(cl = "regr.xgboost", 
                              id = "xgboost", 
                              predict.type = "response", 
                              objective = "reg:squarederror",             # squared loss function
                              colsample_bytree = 1,                       # equivalent to no bagging. I.e. all predictors are used building trees
                              min_child_weight = 1                        # minimum number of instances needed to be in each node
                              )             


## Tuning =================================================================
# Inner loop of nested resampling
# Note: Hyperparameters are tuned only once on overall training data (data_training) using time series cross validation.
# The tuned hyperparameters are then held constant in the outer loop for performance estimation. Performance evaluation is
# also based on time series cross validation which means that in each iteration step the model is retrained given the hyperparameters
# from the inner loop.


### List tuning parameters ################################################

# Gradient Boosting
getParamSet("regr.gbm")
# number of trees:                n.trees
# maximum depth of each tree:     interaction.depth 
# learning rate:                  shrinkage
# fraction of features:           bag.fraction
# Note: squared error loss (distribution = "gaussian")
# set bag.fraction = 1 as all observations shall be used for training

getParamSet("regr.xgboost")
# number of trees:                nrounds
# maximum depth of each tree:     max_depth
# learning rate:                  eta
# fraction of features:           subsample
# Note: squared error loss (objective = "reg:squarederror")
# default of subsample = 1

### Create hyperparameter set #############################################

tuning_ps_gb.gbm <- makeParamSet(
  makeNumericParam("n.trees",                                              # define tuning parameter
                   lower = 10,                                             # lower value of continuous parameter space
                   upper = 500,                                           # upper value of parameter space
                   trafo = function(x) ceiling(x)),                        # function applied to parameter values (here ceiling to assure that parameters are integers)
  makeNumericParam("interaction.depth", 
                   lower = 1,                                              # lower limit of maximum depth of trees, here set to 1 (= all trees are stumps)
                   upper = 10,                                             # upper limit of maximum depth of trees
                   trafo = function(x) ceiling(x)),
  makeNumericParam("shrinkage", 
                   lower = -3,                                             # as recommended in package documentation (consider transformation function)
                   upper = -1,                                             # as recommended in package documentation (consider transformation function)
                   trafo = function(x) 10^(x))
)

tuning_ps_gb.xgb <- makeParamSet(
  makeNumericParam("nrounds",                                              # define tuning parameter
                   lower = 10,                                             # lower value of continuous parameter space
                   upper = 500,                                           # upper value of parameter space
                   trafo = function(x) ceiling(x)),                        # function applied to parameter values (here ceiling to assure that parameters are integers)
  makeNumericParam("max_depth", 
                   lower = 1,                                              # lower limit of maximum depth of trees, here set to 1 (= all trees are stumps)
                   upper = 10,                                             # upper limit of maximum depth of trees
                   trafo = function(x) ceiling(x)),
  makeNumericParam("eta", 
                   lower = -3,                                              
                   upper = -1,                                              
                   trafo = function(x) 10^(x))
)


### Define optimization algorithm #########################################
# Random search in first tuning stage is applied in this thesis
tuning_control <- makeTuneControlRandom(maxit = tuning_factor*npar_gb)     # random search

# Alternatively iterated F-racing could be applied as grid search turns out to be rather ineffective (spends too much time searching in areas of poor performance)
#tuning_control <- makeTuneControlIrace(maxExperiments = 200)              # promising tuning method (but not working on data in this thesis)

# Alternatively grid search
#tuning_control <- makeTuneControlGrid(resolution = tuning_resolution)     # resolution picks tuning_resolution equally distanced parameter values from the continuous parameter space above



### Tuning results ########################################################
parallelMap::parallelStartSocket(cpus = 4, 
                                 show.info = TRUE)

set.seed(333)
tuning_results_gb.gbm <- tuneParams(learner = learner_gb.gbm,
                                   task = task_training,
                                   resampling = cv_tuning,
                                   par.set = tuning_ps_gb.gbm, 
                                   control = tuning_control, 
                                   show.info = TRUE)

set.seed(333)
tuning_results_gb.xgb <- tuneParams(learner = learner_gb.xgb,
                                   task = task_training,
                                   resampling = cv_tuning,
                                   par.set = tuning_ps_gb.xgb, 
                                   control = tuning_control, 
                                   show.info = TRUE)

## Performance estimation =================================================
# Outer loop of nested resampling
# Note: Outer loop serves for measuring generalization performance on unseen data (observations in overall data (data_overall) 
# starting at index nrow(data_training)+1)


### Redefine learner given optimal parameters #############################
learner_tuned_gb.gbm <- setHyperPars(learner = learner_gb.gbm,
                                     n.trees = tuning_results_gb.gbm$x$n.trees, 
                                     interaction.depth = tuning_results_gb.gbm$x$interaction.depth,
                                     shrinkage = tuning_results_gb.gbm$x$shrinkage)

learner_tuned_gb.xgb <- setHyperPars(learner = learner_gb.xgb,
                                     nrounds = tuning_results_gb.xgb$x$nrounds, 
                                     max_depth = tuning_results_gb.xgb$x$max_depth,
                                     eta = tuning_results_gb.xgb$x$eta)



learner_list <- list(learner_tuned_gb.gbm, 
                     learner_tuned_gb.xgb)

### Performance results ###################################################


set.seed(333)
performance_benchmark_gb <- benchmark(learners = learner_list, 
                                      tasks = task_overall, 
                                      resamplings = cv_test, 
                                      measures = rmse,
                                      keep.pred = TRUE, 
                                      keep.extract = TRUE, 
                                      show.info = TRUE)
parallelMap::parallelStop()



  

  
}

if(finetuning_gb){
# Finetuning --------------------------------------------------------------
## Learner ================================================================
  
# see Steering  

## Tuning =================================================================
# Inner loop of nested resampling
# Note: Hyperparameters are tuned only once on overall training data (data_training) using time series cross validation.
# The tuned hyperparameters are then held constant in the outer loop for performance estimation. Performance evaluation is
# also based on time series cross validation which means that in each iteration step the model is retrained given the hyperparameters
# from the inner loop.

### Create hyperparameter set #############################################

  tuning_ps_gb <- makeParamSet(
    makeNumericParam("nrounds",                                             # define tuning parameter
                     lower = ntree_low,                                     # lower value of continuous parameter space
                     upper = ntree_up,                                      # upper value of parameter space
                     trafo = function(x) ceiling(x)),                       # function applied to parameter values (here ceiling to assure that parameters are integers)
    makeNumericParam("max_depth", 
                     lower = depth_low,                                     # lower limit of maximum depth of trees
                     upper = depth_up,                                      # upper limit of maximum depth of trees
                     trafo = function(x) ceiling(x)),
    makeNumericParam("eta", 
                     lower = rate_low,                                      
                     upper = rate_up,                                       
                     trafo = function(x) (x))
  )
### Define optimization algorithm #########################################
# Grid search is applied in this thesis
tuning_control <- makeTuneControlGrid(resolution = c(nrounds = 11, eta = 10, max_depth = 7))      # resolution picks tuning_resolution equally distanced parameter values from the continuous parameter space above



### Tuning results ########################################################
parallelMap::parallelStartSocket(cpus = 4, 
                                 show.info = TRUE)

set.seed(333)
finetuning_results_gb <- tuneParams(learner = learner_gb,
                                    task = task_training,
                                    resampling = cv_tuning,
                                    par.set = tuning_ps_gb, 
                                    control = tuning_control, 
                                    show.info = TRUE)

## Performance estimation =================================================
# Outer loop of nested resampling
# Note: Outer loop serves for measuring generalization performance on unseen data (observations in overall data (data_overall) 
# starting at index nrow(data_training)+1)


### Redefine learner given optimal parameters #############################


learner_tuned_gb <- setHyperPars(learner = learner_gb,
                                 nrounds = finetuning_results_gb$x[[1]], 
                                 max_depth = finetuning_results_gb$x[[2]],
                                 eta = finetuning_results_gb$x[[3]])


### Performance results ###################################################


set.seed(333)
performance_results_gb <- resample(learner = learner_tuned_gb,
                                   task = task_overall,
                                   resampling = cv_test,
                                   measures = rmse,
                                   keep.pred = TRUE,
                                   show.info = TRUE,
                                   models = TRUE)                      # save models for later assessment of variable importance

parallelMap::parallelStop()





}

beep(sound = 2)