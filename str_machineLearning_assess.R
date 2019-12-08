# Steering ----------------------------------------------------------------
setwd("J:\\Studium\\Master\\Masterthesis")

rf_assess <- FALSE
gb_assess <- FALSE
sv_assess <- TRUE

Qrtr <- TRUE
Yr <- FALSE


# Sourcing ----------------------------------------------------------------
source(file = file.path(getwd(), "Code", "src_setup.R"))


# Loading results ---------------------------------------------------------

if(rf_assess){
  if(Qrtr){
    load(file.path(getwd(), "Results", "Crisis", "RF", "performance_benchmark_rf_Q.RData"))
    load(file = file.path(getwd(), "Results", "Crisis", "RF", "tuning_results_rf.ranger_Q.RData"))
  }
  
  if(Yr){
    load(file.path(getwd(), "Results", "Crisis", "RF", "performance_benchmark_rf_Y.RData"))
    load(file = file.path(getwd(), "Results", "Crisis", "RF", "tuning_results_rf.rf_Y.RData"))
  }
}

if(gb_assess){
  if(Qrtr){
    load(file.path(getwd(), "Results", "Crisis", "GB", "performance_benchmark_gb_Q.RData"))
    load(file = file.path(getwd(), "Results", "Crisis", "GB", "tuning_results_gb.xgb_Q.RData"))
  }
  
  if(Yr){
    load(file.path(getwd(), "Results", "Crisis", "GB", "performance_benchmark_gb_Y.RData"))
    load(file = file.path(getwd(), "Results", "Crisis", "GB", "tuning_results_gb.gbm_Y.RData"))
  }
}

if(sv_assess){
  if(Qrtr){
    load(file.path(getwd(), "Results", "Crisis", "SVR", "performance_benchmark_sv_Q.RData"))
    load(file = file.path(getwd(), "Results", "Crisis", "SVR", "tuning_results_sv.svm_Q.RData"))
  }
  
  if(Yr){
    load(file.path(getwd(), "Results", "Crisis", "SVR", "performance_benchmark_sv_Y.RData"))
    load(file = file.path(getwd(), "Results", "Crisis", "SVR", "tuning_results_sv.svm_Y.RData"))
  }
}


# Assessing results -------------------------------------------------------


## RF =====================================================================
### Benchmarking analysis #################################################

# Best performer from set of same machine learning class
performance_benchmark_rf                                                  

# Hyperparameter constellations of different implementations
performance_benchmark_rf$learners$randomForest$par.vals %>% 
  as_tibble() 
performance_benchmark_rf$learners$randomForestSRC$par.vals %>% 
  as_tibble()
performance_benchmark_rf$learners$ranger$par.vals %>% 
  as_tibble()



# Hyperparameters of best learner
best_learner_rf <- generateHyperParsEffectData(tuning_results_rf.rf, 
                                               include.diagnostics = FALSE, 
                                               trafo = TRUE, 
                                               partial.dep = TRUE) %>% 
  .$data %>% 
  as_tibble() 

# Define number of tuning parameters
npar_rf <- 3

# Top parameter constellations
best_learner_rf10 <- best_learner_rf %>% 
  top_n(10*npar_rf, -rmse.test.rmse) %>% 
  arrange(rmse.test.rmse)

# Extract 25% and 75% quantiles of top 10 results of all hyperparameters for lower and upper bound in finetuning
quantile(best_learner_rf10$ntree, probs = 0.25)
quantile(best_learner_rf10$ntree, probs = 0.75)
quantile(best_learner_rf10$mtry, probs = 0.25)
quantile(best_learner_rf10$mtry, probs = 0.75)
quantile(best_learner_rf10$nodesize, probs = 0.25)
quantile(best_learner_rf10$nodesize, probs = 0.75)



# Check indices
tuning_results_rf.ranger$resampling$train.inds
tuning_results_rf.ranger$resampling$test.inds

# Check test results (gives forecasting results on both each training data point and test data)
performance_benchmark_rf$results$gdp_forecast$ranger$pred$data %>% 
  as_tibble() 

# Summarise results in fashion of econometric models
performance_benchmark_rf$results$gdp_forecast$ranger$pred$data %>% 
  as_tibble() %>% 
  filter(set == "test") %>% 
  mutate(MODEL_ID = "RF.rf") %>% 
  bind_rows(performance_benchmark_rf$results$gdp_forecast$randomForestSRC$pred$data %>% 
              as_tibble() %>% 
              filter(set == "test") %>% 
              mutate(MODEL_ID = "RF.rfSRC")) %>% 
  bind_rows(performance_benchmark_rf$results$gdp_forecast$ranger$pred$data %>% 
              as_tibble() %>% 
              filter(set == "test") %>% 
              mutate(MODEL_ID = "RF.ranger")) %>% 
  mutate(ERROR = truth - response) %>%
  group_by(MODEL_ID) %>% 
  summarise(ME = mean(ERROR),
            MSE = mean(ERROR^2),
            RMSE = sqrt(mean(ERROR^2)))



### Finetuning analysis ###################################################

hyperparameters_rf <- generateHyperParsEffectData(finetuning_results_rf, 
                                                  include.diagnostics = FALSE, 
                                                  trafo = TRUE, 
                                                  partial.dep = TRUE) %>% 
  .$data %>% 
  as_tibble()

### Visualizations ########################################################
# Line graph of number of trees from random search
best_learner_rf %>% 
  mutate(INT_NODESIZE = cut(nodesize, breaks = seq(1,83,length.out = 9))) %>%  
  group_by(INT_NODESIZE) %>% 
  summarise(MEAN_MSE = mean(mse.test.mean)) %>% 
  ggplot(aes(x = INT_NODESIZE, y = MEAN_MSE, group = 1)) + 
  geom_point() + 
  geom_path() +
  theme_thesis
# Interpretation: algorithm clearly favoring small trees




# Heat map from random search
best_learner_rf %>% 
  mutate(INT_TREE = cut(ntree, breaks = seq(0,1000,length.out = 11)),
         INT_NODESIZE = cut(nodesize, breaks = seq(1,83,length.out = 9))) %>%  
  group_by(INT_TREE, INT_NODESIZE) %>% 
  summarise(MEAN_MSE = mean(mse.test.mean)) %>% 
  ungroup() %>% 
  ggplot(aes(x = INT_NODESIZE, y = INT_TREE, fill = MEAN_MSE, group = 1)) +
  geom_tile() + 
  scale_fill_gradient(low = "green", high = "red") +
  theme_thesis
# Interpretation: algorithm clearly favoring small trees and rather indifferent regarding tree size




# Heat map from grid search
hyperparameters_rf %>% 
  filter(mtry == finetuning_results_rf$x$mtry) %>% 
  ggplot() +
  scale_x_continuous(breaks=ceiling(seq(finetuning_results_rf$opt.path$par.set$pars$ntree$lower,
                                        finetuning_results_rf$opt.path$par.set$pars$ntree$upper,length.out = 11)), 
                     labels=ceiling(seq(finetuning_results_rf$opt.path$par.set$pars$ntree$lower,
                                        finetuning_results_rf$opt.path$par.set$pars$ntree$upper,length.out = 11))) +
  scale_y_continuous(breaks=seq(finetuning_results_rf$opt.path$par.set$pars$nodesize$lower,
                                finetuning_results_rf$opt.path$par.set$pars$nodesize$upper,length.out = 11), 
                     labels=seq(finetuning_results_rf$opt.path$par.set$pars$nodesize$lower,
                                finetuning_results_rf$opt.path$par.set$pars$nodesize$upper,length.out = 11)) +
  geom_tile(aes(x = ntree, y = nodesize, fill = mse.test.mean)) + 
  scale_fill_gradient(low = "green", high = "red") +
  theme_thesis
# Interpretation: no insight



# Line graph of number of trees from grid search
hyperparameters_rf %>% 
  group_by(ntree) %>% 
  summarise(MEAN_MSE = mean(mse.test.mean)) %>% 
  ggplot(aes(x = ntree, y = MEAN_MSE)) + 
  geom_point() + 
  geom_path() + 
  theme_thesis
# Interpretation: no insight


# Variable importance measure
vim_rf <- matrix(data = NA, 
                 nrow = length(performance_results_rf$models), 
                 ncol = length(performance_results_rf$models[[1]]$features),
                 dimnames = list(1:length(performance_results_rf$models),
                                 performance_results_rf$models[[1]]$features)) %>% 
  as_tibble()
for (i in 1:length(performance_results_rf$models)){
vim_rf[i,] <- bind_rows(getFeatureImportance(performance_results_rf$models[[i]],
                                 type = 2)$res) 
}

sapply(vim_rf, mean) %>% 
  enframe(name = "VARIABLE", value = "MEAN_DECREASE_RSS") %>% 
  arrange(-MEAN_DECREASE_RSS) %>% 
  top_n(10) %>% 
  ggplot() +
  geom_col(aes(y = MEAN_DECREASE_RSS, x = reorder(VARIABLE, MEAN_DECREASE_RSS))) +
  coord_flip() + 
  theme_thesis
# Interpretation:  



## GB =====================================================================
### Benchmarking analysis #################################################

# Best performer from set of same machine learning class
performance_benchmark_gb      

# Hyperparameter constellations of different implementations
performance_benchmark_gb$learners$gbm$par.vals %>% 
  as_tibble() 
performance_benchmark_gb$learners$xgboost$par.vals %>% 
  as_tibble()



# Hyperparameters of best learner
best_learner_gb <- generateHyperParsEffectData(tuning_results_gb.gbm, 
                                               include.diagnostics = FALSE, 
                                               trafo = TRUE, 
                                               partial.dep = TRUE) %>% 
  .$data %>% 
  as_tibble() 

# Define number of tuning parameters
npar_gb <- 3

# Top parameter constellations
best_learner_gb10 <- best_learner_gb %>% 
  top_n(10*npar_gb, -rmse.test.rmse) %>% 
  arrange(rmse.test.rmse)

# Extract 25% and 75% quantiles of top 10 results of all hyperparameters for lower and upper bound in finetuning
quantile(best_learner_gb10$n.trees, probs = 0.25)
quantile(best_learner_gb10$n.trees, probs = 0.75)
range(best_learner_gb10$n.trees)

quantile(best_learner_gb10$shrinkage, probs = 0.25)
quantile(best_learner_gb10$shrinkage, probs = 0.75)
range(best_learner_gb10$shrinkage)

quantile(best_learner_gb10$interaction.depth, probs = 0.25)
quantile(best_learner_gb10$interaction.depth, probs = 0.75)
range(best_learner_gb10$interaction.depth)


# Check indices
tuning_results_rf.rf$resampling$train.inds
tuning_results_rf.rf$resampling$test.inds

# Check test results (gives forecasting results on both each training data point and test data)
performance_benchmark_rf$results$gdp_forecast$randomForest$pred$data %>% 
  as_tibble() 

# Summarise results in fashion of econometric models
performance_benchmark_rf$results$gdp_forecast$randomForest$pred$data %>% 
  as_tibble() %>% 
  filter(set == "test") %>% 
  mutate(MODEL_ID = "RF.rf") %>% 
  bind_rows(performance_benchmark_rf$results$gdp_forecast$randomForestSRC$pred$data %>% 
              as_tibble() %>% 
              filter(set == "test") %>% 
              mutate(MODEL_ID = "RF.rfSRC")) %>% 
  bind_rows(performance_benchmark_rf$results$gdp_forecast$ranger$pred$data %>% 
              as_tibble() %>% 
              filter(set == "test") %>% 
              mutate(MODEL_ID = "RF.ranger")) %>% 
  mutate(ERROR = truth - response) %>%
  group_by(MODEL_ID) %>% 
  summarise(ME = mean(ERROR),
            MSE = mean(ERROR^2),
            RMSE = sqrt(mean(ERROR^2)))


### Finetuning analysis ###################################################


hyperparameters_sv <- generateHyperParsEffectData(finetuning_results_gb, 
                                                  include.diagnostics = FALSE, 
                                                  trafo = TRUE, 
                                                  partial.dep = TRUE) %>% 
  .$data %>% 
  as_tibble() %>% 
  arrange(rmse.test.rmse)





## SV =====================================================================
### Benchmarking analysis #################################################
# Best performer from set of same machine learning class
performance_benchmark_sv

# Hyperparameter constellations of different implementations
performance_benchmark_sv$learners$svm$par.vals %>% 
  as_tibble() 
performance_benchmark_sv$learners$ksvm$par.vals %>% 
  as_tibble()



# Hyperparameters of best learner
best_learner_sv <- generateHyperParsEffectData(tuning_results_sv.svm, 
                                               include.diagnostics = FALSE, 
                                               trafo = TRUE, 
                                               partial.dep = TRUE) %>% 
  .$data %>% 
  as_tibble() %>% 
  arrange(rmse.test.rmse)

# Define number of tuning parameters
npar_sv <- 3


# Top parameter constellations
best_learner_sv10 <- best_learner_sv %>% 
  top_n(10*npar_sv, -rmse.test.rmse) %>% 
  arrange(rmse.test.rmse)

# Extract 25% and 75% quantiles of top 10 results of all hyperparameters for lower and upper bound in finetuning
table(best_learner_sv10$kernel) # radial kernel most often in top 10
best_learner_sv10 <- best_learner_sv %>% 
  filter(kernel == "sigmoid") %>% 
  top_n(10*npar_sv, -rmse.test.rmse) %>% 
  arrange(rmse.test.rmse)

format(range(best_learner_sv10$cost), scientific = FALSE)
format(range(best_learner_sv10$epsilon), scientific = FALSE)
format(range(best_learner_sv10$gamma), scientific = FALSE)
quantile(best_learner_sv10$cost, probs = 0.25)
quantile(best_learner_sv10$cost, probs = 0.75)
format(quantile(best_learner_sv10$epsilon, probs = 0.25), scientific=FALSE)
quantile(best_learner_sv10$epsilon, probs = 0.75)
quantile(best_learner_sv10$gamma, probs = 0.25)
quantile(best_learner_sv10$gamma, probs = 0.75)

# Check indices
tuning_results_sv.svm$resampling$train.inds
tuning_results_sv.svm$resampling$test.inds

### Finetuning analysis ###################################################


performance_results_sv.svm

finetuning_results_sv.svm

hyperparameters_sv <- generateHyperParsEffectData(finetuning_results_sv, 
                                                  include.diagnostics = FALSE, 
                                                  trafo = TRUE, 
                                                  partial.dep = TRUE) %>% 
  .$data %>% 
  as_tibble() %>% 
  arrange(rmse.test.rmse)






### Visualizations ########################################################




# HERE: good but how is the value for the raster calculated? mean? Problem: Many observations which fall in the same raster. So which value for coloring raster is tken?
best_learner_sv %>% 
  filter(kernel == "sigmoid") %>% 
  mutate(int_cost = cut(cost, breaks = 10^seq(-5,4,length.out = 10)),
         int_epsilon = cut(epsilon, breaks = 10^seq(-9,0,length.out = 10))) %>% 
  ggplot() +
  #scale_y_log10(breaks=10^seq(-5,4,length.out = 10),labels=10^seq(-5,4,length.out = 10)) +
  #scale_x_log10(breaks=10^seq(-9,0,length.out = 10),labels=10^seq(-9,0,length.out = 10)) +
  geom_tile(aes(x = int_epsilon, y = int_cost, fill = mse.test.mean)) + 
  scale_fill_gradient(low = "green", high = "red", trans = "log")


best_learner_sv %>% 
  filter(kernel == "sigmoid") %>% 
  group_by(epsilon, cost) %>% 
  summarise(mse.test.mean = mean(mse.test.mean)) %>% 
  #filter(gamma == performance_benchmark_sv$learners$svm$par.vals$gamma) %>% 
  ggplot() +
  scale_x_log10(breaks=10^seq(-3,0,length.out = 4), labels=10^seq(-3,0,length.out = 4)) +
  scale_y_log10(breaks=10^seq(-3,1,length.out = 5), labels=10^seq(-3,1,length.out = 5)) +
  geom_tile(aes(x = epsilon, y = cost, fill = mse.test.mean)) + 
  scale_fill_gradient(low = "green", high = "red", trans = "log")



shyperparameters_sv %>% 
  filter(kernel == "sigmoid") %>% 
  ggplot() +
  scale_x_continuous(breaks=seq(epsilon_low,epsilon_up,length.out = 11), labels=seq(epsilon_low,epsilon_up,length.out = 11)) +
  scale_y_continuous(breaks=seq(cost_low,cost_up,length.out = 11), labels=seq(cost_low,cost_up,length.out = 11)) +
  geom_tile(aes(x = epsilon, y = cost, fill = mse.test.mean)) + 
  scale_fill_gradient(low = "green", high = "red")



# -------------------------------------------------------------------------