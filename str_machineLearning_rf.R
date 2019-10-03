# Steering ----------------------------------------------------------------
setwd("J:\\Studium\\Master\\Masterthesis")

# Random Forest
benchmarking_rf <- FALSE
finetuning_rf <- FALSE

# Finetuning parameter threshholds
ntree_low <- 10
ntree_up <- 65
mtry_low <- 105
mtry_up <- 209
nodesize_low <- 1
nodesize_up <- 29

#--------------------------------------------------------------------------

if(benchmarking_rf){
# Algorithm benchmarking --------------------------------------------------
## Learners ===============================================================

learner_rf.rf <- makeLearner(cl = "regr.randomForest",                   
                             id = "randomForest",                        
                             predict.type = "se")                          # special prediction in regression tasks (mean response AND standard errors)

learner_rf.rfSRC <- makeLearner(cl = "regr.randomForestSRC",                   
                                id = "randomForestSRC",                        
                                predict.type = "response")                 # standard prediction in regression tasks (mean response)

learner_rf.ranger <- makeLearner(cl = "regr.ranger",                   
                                 id = "ranger",                        
                                 predict.type = "se") 




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
                   upper = 500,                                            # upper value of parameter space
                   trafo = function(x) ceiling(x)),                        # function applied to parameter values (here ceiling to assure that parameters are integers)
  makeNumericParam("mtry", 
                   lower = floor(ncol(data_training)*0.1),                 # fraction of possible feature is rounded down
                   upper = ncol(data_training)-1,                          # maximum number is the number of columns of training data less the target variable
                   trafo = function(x) ceiling(x)),
  makeNumericParam("nodesize", 
                   lower = 1,                                              # lower bound for minim node size
                   upper = floor(nrow(data_training))/2,                   # upper bound of minimum node size is set to half of the observations (possibly resulting in stumps)
                   trafo = function(x) ceiling(x))
)

tuning_ps_rf.rfSRC <- tuning_ps_rf.rf

tuning_ps_rf.ranger <- makeParamSet(
  makeNumericParam("num.trees",                                            # define tuning parameter
                   lower = 10,                                             # lower value of continuous parameter space
                   upper = 500,                                            # upper value of parameter space
                   trafo = function(x) ceiling(x)),                        # function applied to parameter values (here ceiling to assure that parameters are integers)
  makeNumericParam("mtry", 
                   lower = floor(ncol(data_training)*0.1), 
                   upper = ncol(data_training)-1, 
                   trafo = function(x) ceiling(x)),
  makeNumericParam("min.node.size", 
                   lower = 1,                                              # lower bound for minim node size
                   upper = floor(nrow(data_training))/2,                   # upper bound of minimum node size is set to half of the observations (possibly resulting in stumps)
                   trafo = function(x) ceiling(x))
)


### Define optimization algorithm #########################################
# Grid search is applied in this thesis
tuning_control <- makeTuneControlGrid(resolution = tuning_resolution)      # resolution picks tuning_resolution equally distanced parameter values from the continuous parameter space above



### Tuning results ########################################################
parallelMap::parallelStartSocket(cpus = 4, 
                                 show.info = TRUE)

set.seed(333)
tuning_results_rf.rf <- tuneParams(learner = learner_rf.rf,
                                   task = task_training,
                                   resampling = cv_tuning,
                                   par.set = tuning_ps_rf.rf, 
                                   control = tuning_control, 
                                   show.info = TRUE)

set.seed(333)
tuning_results_rf.rfSRC <- tuneParams(learner = learner_rf.rfSRC,
                                   task = task_training,
                                   resampling = cv_tuning,
                                   par.set = tuning_ps_rf.rfSRC, 
                                   control = tuning_control, 
                                   show.info = TRUE)

set.seed(333)
tuning_results_rf.ranger <- tuneParams(learner = learner_rf.ranger,
                                   task = task_training,
                                   resampling = cv_tuning,
                                   par.set = tuning_ps_rf.ranger, 
                                   control = tuning_control, 
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
                                      measures = rmse,
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

learner_rf.ranger <- makeLearner(cl = "regr.ranger",                   
                                   id = "ranger",                        
                                   predict.type = "se") 
  
  
  
  
## Tuning =================================================================
# Inner loop of nested resampling
# Note: Hyperparameters are tuned only once on overall training data (data_training) using time series cross validation.
# The tuned hyperparameters are then held constant in the outer loop for performance estimation. Performance evaluation is
# also based on time series cross validation which means that in each iteration step the model is retrained given the hyperparameters
# from the inner loop.
  
### Create hyperparameter set #############################################

tuning_ps_rf.ranger <- makeParamSet(
  makeNumericParam("num.trees",                                           
                     lower = ntree_low,                                            
                     upper = ntree_up,                                        
                     trafo = function(x) ceiling(x)),                      
  makeNumericParam("mtry", 
                     lower = mtry_low, 
                     upper = mtry_up, 
                     trafo = function(x) ceiling(x)),
  makeNumericParam("min.node.size", 
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

set.seed(333)
finetuning_results_rf.ranger <- tuneParams(learner = learner_rf.ranger,
                                           task = task_training,
                                           resampling = cv_tuning,
                                           par.set = tuning_ps_rf.ranger, 
                                           control = tuning_control, 
                                           show.info = TRUE)

## Performance estimation =================================================
# Outer loop of nested resampling
# Note: Outer loop serves for measuring generalization performance on unseen data (observations in overall data (data_overall) 
# starting at index nrow(data_training)+1)


### Redefine learner given optimal parameters #############################


learner_tuned_rf.ranger <- setHyperPars(learner = learner_rf.ranger,
                                        num.trees = finetuning_results_rf.ranger$x$num.trees, 
                                        mtry = finetuning_results_rf.ranger$x$mtry,
                                        min.node.size = finetuning_results_rf.ranger$x$min.node.size)


### Performance results ###################################################


set.seed(333)
performance_results_rf.ranger <- resample(learner = learner_tuned_rf.ranger,
                                          task = task_overall,
                                          resampling = cv_test,
                                          measures = rmse,
                                          keep.pred = TRUE,
                                          show.info = TRUE)

parallelMap::parallelStop()






# 
# performance_results$pred %>% 
#   as_tibble() %>% 
#   filter(set == "test") %>% 
#   mutate(ERROR = truth - response) %>%
#   summarise(ME = mean(ERROR),
#             MSE = mean(ERROR^2),
#             RMSE = sqrt(mean(ERROR^2)))



  
}


