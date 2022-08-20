# SUPPORT VECTOR REGRESSION
# Steering ----------------------------------------------------------------
setwd("J:\\Studium\\Master\\Masterthesis")

# Tuning stage
benchmarking_sv <- TRUE
finetuning_sv <- FALSE

# First-stage tuning setup
npar_sv <- 3           # number of tuning parameters
tuning_factor <- 100   # modifiable tuning factor for SVR as results are highly sensitive to tuning

# Finetuning setup
# Name of tuning parameters
cost_sv <- "cost" 
epsilon_sv <- "epsilon"

# Final kernel & search space boundaries
kern <- "sigmoid"
cost_low <- 0.06
cost_up <- 0.36
epsilon_low <- 0.0002
epsilon_up <- 0.068
#d_low <- 1
#d_up <- 10
gamma_fix <- 1/ncol(data_training)

# Final learner
learner_sv <- makeLearner(cl = "regr.svm", 
                          id = "svm", 
                          predict.type = "response",
                          type = "eps-regression",                         # epsilon-insensitive loss function
                          gamma = gamma_fix,                               # kernel parameter (comment this line if the parameter shall be tuned)
                          #degree = 3,                                      # kernel parameter for polynomial kernel only (default value = 3; comment this line if the parameter shall be tuned)
                          coef0 = 0,                                       # kernel parameter (default value = 0; comment this line if the parameter shall be tuned)
                          fitted = FALSE)


#--------------------------------------------------------------------------
if(benchmarking_sv){
# Algorithm benchmarking --------------------------------------------------
# List learners with 'support' in name suitbale for the respective task
listLearners(task_training) %>% 
  as_tibble() %>% 
  filter(str_detect(name, regex("support", ignore_case = TRUE))) %>% 
  select(class, name, short.name, package, note, se, featimp)


## Learners ===============================================================


learner_sv.svm <- makeLearner(cl = "regr.svm", 
                              id = "svm", 
                              predict.type = "response",
                              type = "eps-regression",                    # epsilon-insensitive loss function
                              gamma = 1/ncol(data_training),              # kernel parameter (comment this line if the parameter shall be tuned)
                              degree = 3,                                 # kernel parameter for polynomial kernel only (default value = 3; comment this line if the parameter shall be tuned)
                              coef0 = 0,                                  # kernel parameter (default value = 0; comment this line if the parameter shall be tuned)
                              fitted = FALSE,                             # not sure yet whether this is a good idea
                              scale = TRUE)                               # scale features before feeding into model                              

# learner_sv.svm <- makePreprocWrapperCaret(learner_sv.svm, 
#                                           ppc.center = TRUE,              # centers the features
#                                           ppc.scale = TRUE                # scales the features
#                                           )

learner_sv.ksvm <- makeLearner(cl = "regr.ksvm", 
                              id = "ksvm", 
                              predict.type = "response", 
                              type = "eps-svr",                           # epsilon-insensitive loss function
                              # sigma = 1/ncol(data_training),              # kernel parameter for radial kernel only (default value = 1; comment this line if the parameter shall be tuned; if tuned there is a possibility to tune it for radial kernels only: requires = quote(kernel == "rbfdot"))
                              # degree = 3,                                 # kernel parameter for polynomial kernel only (default value = 1; comment this line if the parameter shall be tuned)
                              # scale = 1/ncol(data_training),              # kernel parameter (default value = 1; comment this line if the parameter shall be tuned)
                              # offset = 0,                                 # kernel parameter (default value = 1; comment this line if the parameter shall be tuned)
                              fit = FALSE)                                # not sure yet whether this is a good idea



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
# Think about:
# - check scaling in mlr pipeline (normalization is possible, but for visualization and interpretation I want to stick on the original scale)
# - include kernel parameters in tuning (as for now do not tune kernel parameters but only SV parameters)
# - consider different tuning techniques (no stick with grid search and do finetuning in a second round but iterated F-racing highly interesting)




tuning_ps_sv.svm <- makeParamSet(
  makeDiscreteLearnerParam("kernel", 
                           #values = c("polynomial", "radial", "sigmoid"), # define different kernels to use in SVR
                           values = c("sigmoid"),                         # if only sigmoid
                           #default = "sigmoid"
                           ),
  # makeNumericParam("coef0",                                             # define tuning parameter for the polynomial degree for polynomial kernel
  #                  lower = 0,                                           # lower value is 1 which equals a linear kernel
  #                  upper = 10,                                          # upper value 10 is reasonable
  #                  trafo = function(x) ceiling(x),                      # function applied to parameter values (here ceiling to assure that parameters are integers)
  #                  requires = quote(kernel != "radial")),
  # makeNumericParam("degree",                                            # define tuning parameter for the polynomial degree for polynomial kernel
  #                  lower = 1,                                           # lower value is 1 which equals a linear kernel
  #                  upper = 10,                                          # upper value 10 is reasonable
  #                  trafo = function(x) ceiling(x),                      # function applied to parameter values (here ceiling to assure that parameters are integers)
  #                  requires = quote(kernel == "polynomial")),
  # makeNumericParam("gamma",                                               # General remark: in radial kernel gamma reflects how far the influence of a single training example reaches, with low values meaning 'far' and high values meaning 'close'. The gamma parameters can be seen as the inverse of the radius of influence of samples selected by the model as support vectors.
  #                  lower = 1/ncol(data_training),                         # gamma should capture the the inverse dimension of the input space. A good starting point could be 1/0.1*dimension
  #                  upper = 1000*1/ncol(data_training),                      # upper limit of gamma could the inverse of the dimension itself (1/dim)
  #                  trafo = function(x) x),
  makeNumericParam("cost", 
                   lower = -2,                                            # as recommended in package documentation (consider transformation function)
                   upper = 4,                                             # as recommended in package documentation (consider transformation function)
                   trafo = function(x) 10^x),
  makeNumericParam("epsilon", 
                   lower = -5,                                            # as recommended in package documentation (consider transformation function)
                   upper = 0,                                             # as recommended in package documentation (consider transformation function)
                   trafo = function(x) 10^x)
)

tuning_ps_sv.ksvm <- makeParamSet(
  makeDiscreteLearnerParam("kernel", 
                           values = c("polydot", "rbfdot", "tanhdot"),   # define different kernels to use in SVR
                           default = "rbfdot"),
  makeNumericParam("degree",                                             # define tuning parameter for the polynomial degree for polynomial kernel
                   lower = 3,                                            # lower value is 1 which equals a linear kernel
                   upper = 3,                                            # upper value 10 is reasonable
                   trafo = function(x) x,
                   tunable = FALSE,
                   requires = quote(kernel == "polydot")),               # function applied to parameter values (here ceiling to assure that parameters are integers)
  makeNumericParam("sigma",                                              # General remark: in radial kernel gamma reflects how far the influence of a single training example reaches, with low values meaning 'far' and high values meaning 'close'. The gamma parameters can be seen as the inverse of the radius of influence of samples selected by the model as support vectors.
                   lower = 1/ncol(data_training),                        # gamma should capture the the inverse dimension of the input space. A good starting point could be 1/0.1*dimension
                   upper = 1/ncol(data_training),                        # upper limit of gamma could the inverse of the dimension itself (1/dim)
                   trafo = function(x) x,
                   tunable = FALSE,
                   requires = quote(kernel == "rbfddot")),
  makeNumericParam("scale",                                              # General remark: in radial kernel gamma reflects how far the influence of a single training example reaches, with low values meaning 'far' and high values meaning 'close'. The gamma parameters can be seen as the inverse of the radius of influence of samples selected by the model as support vectors.
                   lower = 1/ncol(data_training),                        # gamma should capture the the inverse dimension of the input space. A good starting point could be 1/0.1*dimension
                   upper = 1/ncol(data_training),                        # upper limit of gamma could the inverse of the dimension itself (1/dim)
                   trafo = function(x) x,
                   tunable = FALSE,
                   requires = quote(kernel != c("rbfdot"))),
  makeNumericParam("offset",                                             # General remark: in radial kernel gamma reflects how far the influence of a single training example reaches, with low values meaning 'far' and high values meaning 'close'. The gamma parameters can be seen as the inverse of the radius of influence of samples selected by the model as support vectors.
                   lower = 0,                                            # gamma should capture the the inverse dimension of the input space. A good starting point could be 1/0.1*dimension
                   upper = 0,                                            # upper limit of gamma could the inverse of the dimension itself (1/dim)
                   trafo = function(x) x,
                   tunable = FALSE,
                   requires = quote(kernel != c("rbfdot"))),
  makeNumericParam("C", 
                   lower = -5,                                            # as recommended in package documentation (consider transformation function)
                   upper = 4,                                             # as recommended in package documentation (consider transformation function)
                   trafo = function(x) 10^x),
  makeNumericParam("epsilon", 
                   lower = -9,                                            # as recommended in package documentation (consider transformation function)
                   upper = 0,                                             # as recommended in package documentation (consider transformation function)
                   trafo = function(x) 10^x)
)



### Define optimization algorithm #########################################
# Random search in first tuning stage is applied in this thesis
tuning_control <- makeTuneControlRandom(maxit = tuning_factor*npar_sv)     # random search

# Alternatively iterated F-racing could be applied as grid search turns out to be rather ineffective (spends too much time searching in areas of poor performance)
#tuning_control <- makeTuneControlIrace(maxExperiments = 200)              # promising tuning method (but not working on data in this thesis)

# Alternatively grid search
#tuning_design <- generateGridDesign(tuning_ps_sv.svm, resolution = c(gamma = 10, cost = 5, epsilon = 4), trafo = TRUE)
#tuning_control <- makeTuneControlGrid(resolution = c(gamma = 10, cost = 5, epsilon = 4))     # resolution picks tuning_resolution equally distanced parameter values from the continuous parameter space above


### Tuning results ########################################################
parallelMap::parallelStartSocket(cpus = 4, 
                                 show.info = TRUE)

set.seed(333)
tuning_results_sv.svm <- tuneParams(learner = learner_sv.svm,
                                    task = task_training,
                                    resampling = cv_tuning,
                                    par.set = tuning_ps_sv.svm, 
                                    control = tuning_control, 
                                    measures = rmse,
                                    show.info = TRUE)

# set.seed(333)
# tuning_results_sv.ksvm <- tuneParams(learner = learner_sv.ksvm,
#                                      task = task_training,
#                                      resampling = cv_tuning,
#                                      par.set = tuning_ps_sv.ksvm,
#                                      control = tuning_control,
#                                      measures = rmse,
#                                      show.info = TRUE)

## Performance estimation =================================================
# Outer loop of nested resampling
# Note: Outer loop serves for measuring generalization performance on unseen data (observations in overall data (data_overall) 
# starting at index nrow(data_training)+1)


### Redefine learner given optimal parameters #############################
learner_tuned_sv.svm <- setHyperPars(learner = learner_sv.svm,
                                     kernel = tuning_results_sv.svm$x$kernel,
                                     cost = tuning_results_sv.svm$x$cost,
                                     epsilon = tuning_results_sv.svm$x$epsilon
                                     #gamma = tuning_results_sv.svm$x$gamma
                                     )

# learner_tuned_sv.ksvm <- setHyperPars(learner = learner_sv.ksvm,
#                                       kernel = tuning_results_sv.ksvm$x$kernel,
#                                       C = tuning_results_sv.ksvm$x$C,
#                                       epsilon = tuning_results_sv.ksvm$x$epsilon,
#                                       #degree = 3,
#                                       #sigma = 1/ncol(data_training),
#                                       scale = 1/ncol(data_training),
#                                       offset = 0
#                                       )



learner_list <- list(learner_tuned_sv.svm#,
                     #learner_tuned_sv.ksvm
)

### Performance results ###################################################


set.seed(333)
performance_benchmark_sv <- benchmark(learners = learner_list,
                                      tasks = task_overall,
                                      resamplings = cv_test,
                                      measures = list(rmse,medaeTestMedian),
                                      keep.pred = TRUE,
                                      keep.extract = TRUE,
                                      show.info = TRUE)
parallelMap::parallelStop()






}

if(finetuning_sv){
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

tuning_ps_sv <- makeParamSet(
  makeDiscreteLearnerParam("kernel", 
                           values = kern),                                # define best kernel to use in SVR
  # makeNumericParam("degree",                                            # define tuning parameter for the polynomial degree for polynomial kernel
  #                  lower = 1,                                           # lower value is 1 which equals a linear kernel
  #                  upper = 10,                                          # upper value 10 is reasonable
  #                  trafo = function(x) ceiling(x)),                     # function applied to parameter values (here ceiling to assure that parameters are integers)
  # makeNumericParam("gamma",                                             # General remark: in radial kernel gamma reflects how far the influence of a single training example reaches, with low values meaning 'far' and high values meaning 'close'. The gamma parameters can be seen as the inverse of the radius of influence of samples selected by the model as support vectors.
  #                  lower = gamma_low,                                   # gamma should capture the the inverse dimension of the input space. A good starting point could be 1/0.1*dimension
  #                  upper = gamma_up,                                    # upper limit of gamma could the inverse of the dimension itself (1/dim)
  #                  trafo = function(x) x),
  makeNumericParam("cost", 
                   lower = cost_low,                                      
                   upper = cost_up,                                       
                   trafo = function(x) x),
  makeNumericParam("epsilon", 
                   lower = epsilon_low,                                   
                   upper = epsilon_up,                                    
                   trafo = function(x) x)#,
  # makeNumericParam("degree", 
  #                  lower = d_low,                                      
  #                  upper = d_up,                                       
  #                  trafo = function(x) round(x))
)
### Define optimization algorithm #########################################
# Grid search is applied in this thesis
tuning_control <- makeTuneControlGrid(resolution = c(cost = 11, epsilon = 11))      # resolution picks tuning_resolution equally distanced parameter values from the continuous parameter space above
tuning_control <- makeTuneControlRandom(maxit = tuning_factor*npar_sv)     # random search

### Tuning results ########################################################
parallelMap::parallelStartSocket(cpus = 4, 
                                 show.info = TRUE)

set.seed(333)
finetuning_results_sv <- tuneParams(learner = learner_sv,
                                    task = task_training,
                                    resampling = cv_tuning,
                                    par.set = tuning_ps_sv, 
                                    control = tuning_control,
                                    measures = rmse,
                                    show.info = TRUE)

## Performance estimation =================================================
# Outer loop of nested resampling
# Note: Outer loop serves for measuring generalization performance on unseen data (observations in overall data (data_overall) 
# starting at index nrow(data_training)+1)


### Redefine learner given optimal parameters #############################

learner_tuned_sv <- setHyperPars(learner = learner_sv,
                                     kernel = finetuning_results_sv$x[[1]],
                                     cost = finetuning_results_sv$x[[2]],
                                     epsilon = finetuning_results_sv$x[[3]]#,
                                     #gamma = finetuning_results_sv$x$gamma
                                     #degree = finetuning_results_sv$x$degree
                                     )


### Performance results ###################################################


set.seed(333)
performance_results_sv <- resample(learner = learner_tuned_sv,
                                   task = task_overall,
                                   resampling = cv_test,
                                   measures = list(rmse,medaeTestMedian),
                                   keep.pred = TRUE,
                                   show.info = TRUE,
                                   models = TRUE)

parallelMap::parallelStop()





}



beep(sound = 2)                                                            # signalling that execution has been finished