# RANDOM FOREST
# Steering ----------------------------------------------------------------
setwd("J:\\Studium\\Master\\Masterthesis")

# Tuning stage
benchmarking_rf <- TRUE
finetuning_rf <- FALSE

# First-stage tuning setup
npar_rf <- 3         # Number of tuning parameters
mtry_factor <- 0.1     # how much percent of overall features shall be at least considered as splitting variables 
                     # (if = 1, then mtry is fix and all features are considered as possible splitters)

# Finetuning setup
# Final learner
learner_rf <- makeLearner(cl = "regr.randomForest",                   
                          id = "randomForest",                        
                          predict.type = "se",                           # special prediction in regression tasks (mean response AND standard errors)
                          #maxnodes = NULL,                               # maximum number of terminal nodes trees in the forest can have. This parameter is ignored. Steering of tree size via nodesize.
                          replace = FALSE,                               # no replacement of observations in training set (not valid for time series). Note: Argument sampsize defines the size(s) of sample to draw, default is .632 times the sample size.
                          #sampsize = 1,                                  # all observations in training data are used for training (i.e. no bootstrapping any more)
                          #na.action = na.omit                           # omit observations with missings (but data is balanced anyway, imputation takes place beforehand)
                          importance = TRUE
)                         

learner_rf  <- makeLearner(cl = "regr.ranger",                   
                           id = "ranger",                        
                           predict.type = "se",
                           #max.depth = NULL,                          # maximal tree depth. This parameter is ignored. Steering of tree size via min.node.size.
                           replace = FALSE,                           # no replacement of observations in training set (not valid for time series). Note: Argument sample.fraction is the fraction of observations to sample. Default is 1 for sampling with replacement and 0.632 for sampling without replacement.
                           #sample.fraction = 1                       # all observations in training data are used for training (i.e. no bootstrapping any more)
                           importance = "impurity"
                           )


# Name of tuning parameters
ntree_rf <- "num.trees" 
mtry_rf <- "mtry"
nodesize_rf <- "min.node.size"

# Search space boundaries
ntree_low <- 90
ntree_up <- 90
mtry_low <- 183
mtry_up <- 183
nodesize_low <- 95
nodesize_up <- 95

#--------------------------------------------------------------------------

if(benchmarking_rf){
# Algorithm benchmarking --------------------------------------------------
# List learners with 'support' in name suitbale for the respective task
listLearners(task_training) %>% 
  as_tibble() %>% 
  filter(str_detect(name, regex("forest", ignore_case = TRUE))) %>% 
  select(class, name, short.name, package, note, se, featimp)
  
## Learners ===============================================================

learner_rf.rf <- makeLearner(cl = "regr.randomForest",                   
                             id = "randomForest",                        
                             predict.type = "se",                           # special prediction in regression tasks (mean response AND standard errors)
                             #maxnodes = NULL,                               # maximum number of terminal nodes trees in the forest can have. This parameter is ignored. Steering of tree size via nodesize.
                             replace = FALSE,                               # no replacement of observations in training set (not valid for time series). Note: Argument sampsize defines the size(s) of sample to draw, default is .632 times the sample size.
                             #sampsize = 1,                                  # all observations in training data are used for training (i.e. no bootstrapping any more)
                             #na.action = na.omit                           # omit observations with missings (but data is balanced anyway, imputation takes place beforehand)
                             )

learner_rf.rfSRC <- makeLearner(cl = "regr.randomForestSRC",                   
                                id = "randomForestSRC",                        
                                predict.type = "response",                  # standard prediction in regression tasks (mean response)
                                #nodedepth = NULL,                           # maximum depth to which a tree should be grown. This parameter is ignored. Steering of tree size via nodesize.
                                samptype = "swor",                          # no replacement of observations in training set (not valid for time series). Note: argument sampsize is the function specifying size of bootstrap data when by.root is in effect. For sampling without replacement, it is the requested size of the sample, which by default is .632 times the sample size.
                                #sampsize = 1,                               # all observations in training data are used for training (i.e. no bootstrapping any more)
                                bootstrap = "by.root",                      # bootstraps the data by sampling (with or) without replacement. Note: here one could even turn off bootstrapping by setting bootstrap = "none"). 
                                na.action = "na.omit")                      # omit observations with missings (but data is balanced anyway, imputation takes place beforehand)
                                

learner_rf.ranger <- makeLearner(cl = "regr.ranger",                   
                                 id = "ranger",                        
                                 predict.type = "se",
                                 #max.depth = NULL,                          # maximal tree depth. This parameter is ignored. Steering of tree size via min.node.size.
                                 replace = FALSE,                           # no replacement of observations in training set (not valid for time series). Note: Argument sample.fraction is the fraction of observations to sample. Default is 1 for sampling with replacement and 0.632 for sampling without replacement.
                                 #sample.fraction = 1                       # all observations in training data are used for training (i.e. no bootstrapping any more)
                                )
# Notes:
# - for the result assessment later argument importance needs to adjusted accordingly (actually not as only the final model is used for variable importance assessment)
# - for randomForestSRC implementation there is an argument called nsplit which defines 
#   the number of random splits to consider for each candidate splitting variable (default = 10).
#   Is there a similar argument for the other implementations?
#   Apparently no equivalent for randomForest. However, ranger has arguments splitrule and and num.random.splits.
#   Splitrule defaults to NULL. So no special rule specification for each candidate splitting variable.
# - for ranger there is no handling of missings values implemented.

## Tuning =================================================================
# Inner loop of nested resampling
# Note: Hyperparameters are tuned only once on overall training data (data_training) using time series cross validation.
# The tuned hyperparameters are then held constant in the outer loop for performance estimation. Performance evaluation is
# also based on time series cross validation which means that in each iteration step the model is retrained given the hyperparameters
# from the inner loop.


### List tuning parameters ################################################

getParamSet("regr.randomForest")
# number of trees:                ntree
# fraction of features:           mtry
# minimum size of terminal nodes: nodesize

getParamSet("regr.randomForestSRC")
# number of trees:                ntree
# fraction of features:           mtry
# minimum size of terminal nodes: nodesize

getParamSet("regr.ranger")
# number of trees:                num.trees
# fraction of features:           mtry
# minimum size of terminal nodes: min.node.size



### Create hyperparameter set #############################################


tuning_ps_rf.rf <- makeParamSet(
  makeNumericParam("ntree",                                                # define tuning parameter
                   lower = 10,                                             # lower value of continuous parameter space
                   upper = 1000,                                            # upper value of parameter space
                   trafo = function(x) ceiling(x)),                        # function applied to parameter values (here ceiling to assure that parameters are integers)
  makeNumericParam("mtry",
                   lower = floor((ncol(data_training)-1)*mtry_factor),     # fraction of possible feature is rounded down
                   upper = ncol(data_training)-1,                          # maximum number is the number of columns of training data less the target variable
                   trafo = function(x) ceiling(x)),
  makeNumericParam("nodesize", 
                   lower = 1,                                              # lower bound for minim node size
                   upper = floor(nrow(data_training)/2),                   # upper bound of minimum node size is set to half of the observations (possibly resulting in stumps)
                   trafo = function(x) ceiling(x))
)

tuning_ps_rf.rfSRC <- tuning_ps_rf.rf

tuning_ps_rf.ranger <- makeParamSet(
  makeNumericParam("num.trees",                                            # define tuning parameter
                   lower = 10,                                             # lower value of continuous parameter space
                   upper = 1000,                                            # upper value of parameter space
                   trafo = function(x) ceiling(x)),                        # function applied to parameter values (here ceiling to assure that parameters are integers)
  makeNumericParam("mtry",
                   lower = floor((ncol(data_training)-1)*mtry_factor),     # fraction of possible feature is rounded down
                   upper = ncol(data_training)-1,
                   trafo = function(x) ceiling(x)),
  makeNumericParam("min.node.size", 
                   lower = 1,                                              # lower bound for minim node size
                   upper = floor(nrow(data_training)/2),                   # upper bound of minimum node size is set to half of the observations (possibly resulting in stumps)
                   trafo = function(x) ceiling(x))
)


### Define optimization algorithm #########################################
# Random search in first tuning stage is applied in this thesis
tuning_control <- makeTuneControlRandom(maxit = tuning_factor*npar_rf)     # random search

# Alternatively iterated F-racing could be applied as grid search turns out to be rather ineffective (spends too much time searching in areas of poor performance)
#tuning_control <- makeTuneControlIrace(maxExperiments = 200)              # promising tuning method (but not working on data in this thesis)

# Alternatively grid search
#tuning_control <- makeTuneControlGrid(resolution = tuning_resolution)     # resolution picks tuning_resolution equally distanced parameter values from the continuous parameter space above



### Tuning results ########################################################
parallelMap::parallelStartSocket(cpus = 4, 
                                 show.info = TRUE)

set.seed(333)
tuning_results_rf.rf <- tuneParams(learner = learner_rf.rf,
                                   task = task_training,
                                   resampling = cv_tuning,
                                   par.set = tuning_ps_rf.rf, 
                                   control = tuning_control,
                                   measures = rmse,
                                   show.info = TRUE)

set.seed(333)
tuning_results_rf.rfSRC <- tuneParams(learner = learner_rf.rfSRC,
                                      task = task_training,
                                      resampling = cv_tuning,
                                      par.set = tuning_ps_rf.rfSRC, 
                                      control = tuning_control, 
                                      measures = rmse,
                                      show.info = TRUE)

set.seed(333)
tuning_results_rf.ranger <- tuneParams(learner = learner_rf.ranger,
                                       task = task_training,
                                       resampling = cv_tuning,
                                       par.set = tuning_ps_rf.ranger, 
                                       control = tuning_control, 
                                       measures = rmse,
                                       show.info = TRUE)

## Performance estimation =================================================
# Outer loop of nested resampling
# Note: Outer loop serves for measuring generalization performance on unseen data (observations in overall data (data_overall) 
# starting at index nrow(data_training)+1)


### Redefine learner given optimal parameters #############################
learner_tuned_rf.rf <- setHyperPars(learner = learner_rf.rf,
                                    ntree = tuning_results_rf.rf$x$ntree, 
                                    mtry = tuning_results_rf.rf$x$mtry, 
                                    nodesize = tuning_results_rf.rf$x$nodesize)

learner_tuned_rf.rfSRC <- setHyperPars(learner = learner_rf.rfSRC,
                                       ntree = tuning_results_rf.rfSRC$x$ntree, 
                                       mtry = tuning_results_rf.rfSRC$x$mtry,
                                       nodesize = tuning_results_rf.rfSRC$x$nodesize)

learner_tuned_rf.ranger <- setHyperPars(learner = learner_rf.ranger,
                                       num.trees = tuning_results_rf.ranger$x$num.trees, 
                                       mtry = tuning_results_rf.ranger$x$mtry,
                                       min.node.size = tuning_results_rf.ranger$x$min.node.size)

learner_list <- list(learner_tuned_rf.rf, 
                     learner_tuned_rf.rfSRC,
                     learner_tuned_rf.ranger)

### Performance results ###################################################


set.seed(333)
performance_benchmark_rf <- benchmark(learners = learner_list, 
                                      tasks = task_overall, 
                                      resamplings = cv_test, 
                                      measures = list(rmse,medaeTestMedian),
                                      keep.pred = TRUE, 
                                      keep.extract = TRUE, 
                                      show.info = TRUE)
parallelMap::parallelStop()



  

# performance_results <- resample(learner = learner_tuned_rf.rf, 
#                                 task = task_overall, 
#                                 resampling = cv_test, 
#                                 measures = rmse, 
#                                 keep.pred = TRUE, 
#                                 show.info = TRUE)
# 
# performance_results$pred %>% 
#   as_tibble() %>% 
#   filter(set == "test") %>% 
#   mutate(ERROR = truth - response) %>%
#   summarise(ME = mean(ERROR),
#             MSE = mean(ERROR^2),
#             RMSE = sqrt(mean(ERROR^2)))

  
}


if(finetuning_rf){
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

tuning_ps_rf <- makeParamSet(
  makeNumericParam(ntree_rf,                                           
                     lower = ntree_low,                                            
                     upper = ntree_up,                                        
                     trafo = function(x) ceiling(x)),                      
  makeNumericParam(mtry_rf, 
                     lower = mtry_low, 
                     upper = mtry_up, 
                     trafo = function(x) ceiling(x)),
  makeNumericParam(nodesize_rf, 
                     lower = nodesize_low,                                              
                     upper = nodesize_up,                  
                     trafo = function(x) ceiling(x))
)  
  
### Define optimization algorithm #########################################
# Grid search is applied in this thesis
tuning_control <- makeTuneControlGrid(resolution = tuning_resolution)      # resolution picks tuning_resolution equally distanced parameter values from the continuous parameter space above



### Tuning results ########################################################
parallelMap::parallelStartSocket(cpus = 4, 
                                 show.info = TRUE)

if(tuning){
  set.seed(333)
finetuning_results_rf <- tuneParams(learner = learner_rf,
                                    task = task_training,
                                    resampling = cv_tuning,
                                    par.set = tuning_ps_rf, 
                                    control = tuning_control, 
                                    measures = rmse,
                                    show.info = TRUE)
}


## Performance estimation =================================================
# Outer loop of nested resampling
# Note: Outer loop serves for measuring generalization performance on unseen data (observations in overall data (data_overall) 
# starting at index nrow(data_training)+1)


### Redefine learner given optimal parameters #############################
if(tuning){
 learner_tuned_rf <- setHyperPars(learner = learner_rf,
                                 num.trees = finetuning_results_rf$x[[1]],
                                 mtry = finetuning_results_rf$x[[2]],
                                 min.node.size = finetuning_results_rf$x[[3]]) 
}
else{
learner_tuned_rf <- setHyperPars(learner = learner_rf,
                                 num.trees = ntree_low,
                                 mtry = mtry_low,
                                 min.node.size = nodesize_low)
}
### Performance results ###################################################


set.seed(333)
performance_results_rf <- resample(learner = learner_tuned_rf,
                                   task = task_overall,
                                   resampling = cv_test,
                                   measures = list(rmse,medaeTestMedian),
                                   keep.pred = TRUE,
                                   show.info = TRUE, 
                                   models = TRUE)                          # return all fitted models (required for later assessment of model importance)

parallelMap::parallelStop()





}

beep(sound = 2)

