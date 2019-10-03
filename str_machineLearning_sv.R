# Steering ----------------------------------------------------------------
setwd("J:\\Studium\\Master\\Masterthesis")

# Support Vector Regression
benchmarking_sv <- TRUE
finetuning_sv <- FALSE

# Finetuning parameter threshholds




#--------------------------------------------------------------------------
if(benchmarking_sv){
# Algorithm benchmarking --------------------------------------------------
# List learners with 'support' in name suitbale for the respective task
  listLearners(task_training) %>% 
    as_tibble() %>% 
    filter(str_detect(name, regex("support", ignore_case = TRUE))) %>% 
    select(class, name, short.name, package, note, se)


## Learners ===============================================================
  
  
  learner_sv.svm <- makeLearner(cl = "regr.svm", 
                                id = "svm", 
                                predict.type = "response",
                                type = "eps-regression",
                                fitted = FALSE) # not sure yet whether this is a good idea
  
  learner_sv.ksvm <- makeLearner(cl = "regr.ksvm", 
                                id = "ksvm", 
                                predict.type = "response", 
                                type = "eps-svr",
                                fit = FALSE) # not sure yet whether this is a good idea


  
## Tuning =================================================================
  # Inner loop of nested resampling
  # Note: Hyperparameters are tuned only once on overall training data (data_training) using time series cross validation.
  # The tuned hyperparameters are then held constant in the outer loop for performance estimation. Performance evaluation is
  # also based on time series cross validation which means that in each iteration step the model is retrained given the hyperparameters
  # from the inner loop.
  
  
### List tuning parameters ################################################
  
  getParamSet("regr.svm")
  # type of kernel:                 kernel (polynomial, radial, sigmoid)
    # polynomial degree:            degree (polynomial); default: 3
    # scale parameter in kernels:   gamma (polynomial, radial, sigmoid); default: 1/dimension
    # constant in kernels:          coef0 (polynomial, sigmoid); default: 0 <- leave out
  # regularization parameter:       cost
  # tube size:                      epsilon

  
  getParamSet("regr.ksvm")
  # type of kernel:                 kernel (polydot, rbfdot, tanhdot)
    # polynomial degree:            degree (polynomial)
    # scale parameter in kernels:   scale (polynomial, sigmoid)
    # constant in kernels:          offset (polynomial, sigmoid) <- leave out
    # sigma parameter in radial k   sigma (radial) [in svm this is called gamma]
  # regularization parameter:       C
  # tube size:                      epsilon
 
# See here for tuning ranges: http://mlrhyperopt.jakob-r.de/parconfigs
### Create hyperparameter set #############################################
# Continue here:
  # - check scaling in mlr pipeline
  # - include kernel parameters in tuning
  # - consider different tuning techniques
  tuning_ps_sv.svm <- makeParamSet(
    makeDiscreteLearnerParam("kernel", 
                             values = c("polynomial", "radial", "sigmoid"),
                             #values = c("polynomial"),
                             default = "polynomial"),
    # makeNumericParam("degree",                                              # define tuning parameter
    #                  lower = 1,                                             # lower value of continuous parameter space
    #                  upper = 10,                                            # upper value of parameter space
    #                  trafo = function(x) ceiling(x)),                        # function applied to parameter values (here ceiling to assure that parameters are integers)
    # makeNumericParam("gamma",                                               # General remark: in radial kernel gamma reflects the 
    #                  lower = 0,                                              # lower limit of maximum depth of trees, here set to 1 (= all trees are stumps)
    #                  upper = 1,                                             # upper limit of maximum depth of trees
    #                  trafo = function(x) x),
    makeNumericParam("cost", 
                     lower = -10,                                             # as recommended in package documentation (consider transformation function)
                     upper = 10,                                             # as recommended in package documentation (consider transformation function)
                     trafo = function(x) 2^x),
    makeNumericParam("epsilon", 
                     lower = -10,                                             # as recommended in package documentation (consider transformation function)
                     upper = 1,                                             # as recommended in package documentation (consider transformation function)
                     trafo = function(x) 2^x)
  )
  
  # tuning_ps_sv.ksvm <- makeParamSet(
  #   makeNumericParam("nrounds",                                              # define tuning parameter
  #                    lower = 10,                                             # lower value of continuous parameter space
  #                    upper = 500,                                            # upper value of parameter space
  #                    trafo = function(x) ceiling(x)),                        # function applied to parameter values (here ceiling to assure that parameters are integers)
  #   makeNumericParam("max_depth", 
  #                    lower = 1,                                              # lower limit of maximum depth of trees, here set to 1 (= all trees are stumps)
  #                    upper = 10,                                             # upper limit of maximum depth of trees
  #                    trafo = function(x) ceiling(x)),
  #   makeNumericParam("eta", 
  #                    lower = -3,                                              
  #                    upper = -1,                                              
  #                    trafo = function(x) 10^(x))
  # )
  # 
  
### Define optimization algorithm #########################################
  # Grid search is applied in this thesis
  tuning_control <- makeTuneControlGrid(resolution = tuning_resolution)      # resolution picks tuning_resolution equally distanced parameter values from the continuous parameter space above
  
  
  
### Tuning results ########################################################
  parallelMap::parallelStartSocket(cpus = 4, 
                                   show.info = TRUE)
  
  set.seed(333)
  tuning_results_sv.svm <- tuneParams(learner = learner_sv.svm,
                                      task = task_training,
                                      resampling = cv_tuning,
                                      par.set = tuning_ps_sv.svm, 
                                      control = tuning_control, 
                                      show.info = TRUE)
  
  # set.seed(333)
  # tuning_results_gb.xgb <- tuneParams(learner = learner_gb.xgb,
  #                                     task = task_training,
  #                                     resampling = cv_tuning,
  #                                     par.set = tuning_ps_gb.xgb, 
  #                                     control = tuning_control, 
  #                                     show.info = TRUE)
  
## Performance estimation =================================================
  # Outer loop of nested resampling
  # Note: Outer loop serves for measuring generalization performance on unseen data (observations in overall data (data_overall) 
  # starting at index nrow(data_training)+1)
  
  
### Redefine learner given optimal parameters #############################
  learner_tuned_sv.svm <- setHyperPars(learner = learner_sv.svm,
                                       kernel = tuning_results_sv.svm$x$kernel,
                                       cost = tuning_results_sv.svm$x$cost,
                                       epsilon = tuning_results_sv.svm$x$epsilon)

  # learner_tuned_gb.xgb <- setHyperPars(learner = learner_gb.xgb,
  #                                      nrounds = tuning_results_gb.xgb$x$nrounds,
  #                                      max_depth = tuning_results_gb.xgb$x$max_depth,
  #                                      eta = tuning_results_gb.xgb$x$eta)
  # 
  # 
  # 
  learner_list <- list(learner_tuned_sv.svm#,
                       #learner_tuned_sv.ksvm
                       )

### Performance results ###################################################
  
  
  # set.seed(333)
  performance_benchmark_sv <- benchmark(learners = learner_list,
                                        tasks = task_overall,
                                        resamplings = cv_test,
                                        measures = rmse,
                                        keep.pred = TRUE,
                                        keep.extract = TRUE,
                                        show.info = TRUE)
  parallelMap::parallelStop()
  
  
  
  
  
  
}

if(finetuning_sv){
  # Finetuning --------------------------------------------------------------
  ## Learner ================================================================
  
  
  learner_gb.gbm <- makeLearner(cl = "regr.gbm", 
                                id = "gbm", 
                                predict.type = "response",
                                distribution = "gaussian",
                                bag.fraction = 1,
                                keep.data = FALSE)
  ## Tuning =================================================================
  # Inner loop of nested resampling
  # Note: Hyperparameters are tuned only once on overall training data (data_training) using time series cross validation.
  # The tuned hyperparameters are then held constant in the outer loop for performance estimation. Performance evaluation is
  # also based on time series cross validation which means that in each iteration step the model is retrained given the hyperparameters
  # from the inner loop.
  
  ### Create hyperparameter set #############################################
  
  tuning_ps_gb.gbm <- makeParamSet(
    makeNumericParam("n.trees",                                             # define tuning parameter
                     lower = ntree_low,                                     # lower value of continuous parameter space
                     upper = ntree_up,                                      # upper value of parameter space
                     trafo = function(x) ceiling(x)),                       # function applied to parameter values (here ceiling to assure that parameters are integers)
    makeNumericParam("interaction.depth", 
                     lower = depth_low,                                     # lower limit of maximum depth of trees, here set to 1 (= all trees are stumps)
                     upper = depth_up,                                      # upper limit of maximum depth of trees
                     trafo = function(x) ceiling(x)),
    makeNumericParam("shrinkage", 
                     lower = rate_low,                                      # as recommended in package documentation (consider transformation function)
                     upper = rate_up,                                       # as recommended in package documentation (consider transformation function)
                     trafo = function(x) 10^(x))
  )
  ### Define optimization algorithm #########################################
  # Grid search is applied in this thesis
  tuning_control <- makeTuneControlGrid(resolution = tuning_resolution)      # resolution picks tuning_resolution equally distanced parameter values from the continuous parameter space above
  
  
  
  ### Tuning results ########################################################
  parallelMap::parallelStartSocket(cpus = 4, 
                                   show.info = TRUE)
  
  set.seed(333)
  finetuning_results_gb.gbm <- tuneParams(learner = learner_gb.gbm,
                                          task = task_training,
                                          resampling = cv_tuning,
                                          par.set = tuning_ps_gb.gbm, 
                                          control = tuning_control, 
                                          show.info = TRUE)
  
  ## Performance estimation =================================================
  # Outer loop of nested resampling
  # Note: Outer loop serves for measuring generalization performance on unseen data (observations in overall data (data_overall) 
  # starting at index nrow(data_training)+1)
  
  
  ### Redefine learner given optimal parameters #############################
  
  
  learner_tuned_gb.gbm <- setHyperPars(learner = learner_gb.gbm,
                                       n.trees = finetuning_results_gb.gbm$x$n.trees, 
                                       interaction.depth = finetuning_results_gb.gbm$x$interaction.depth,
                                       shrinkage = finetuning_results_gb.gbm$x$shrinkage)
  
  
  ### Performance results ###################################################
  
  
  set.seed(333)
  performance_results_gb.gbm <- resample(learner = learner_tuned_gb.gbm,
                                         task = task_overall,
                                         resampling = cv_test,
                                         measures = rmse,
                                         keep.pred = TRUE,
                                         show.info = TRUE)
  
  parallelMap::parallelStop()
  
  
  
  
  
}