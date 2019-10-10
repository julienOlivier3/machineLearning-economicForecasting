# Steering ----------------------------------------------------------------
setwd("J:\\Studium\\Master\\Masterthesis")

rf_assess <- TRUE
gb_assess <- FALSE
sv_assess <- FALSE

# Sourcing ----------------------------------------------------------------
source(file = file.path(getwd(), "Code", "src_setup.R"))


# Loading results ---------------------------------------------------------

if(rf_assess){
  load(file.path(getwd(), "Results", "Random_Forest", "performance_benchmark_rf.RData"))
  load(file.path(getwd(), "Results", "Random_Forest", "tuning_results_rf.rf.RData"))
  load(file.path(getwd(), "Results", "Random_Forest", "tuning_results_rf.rfSRC.RData"))
  load(file.path(getwd(), "Results", "Random_Forest", "tuning_results_rf.ranger.RData"))
  load(file.path(getwd(), "Results", "Random_Forest", "finetuning_results_rf.RData"))
  load(file.path(getwd(), "Results", "Random_Forest", "performance_results_rf.RData"))
}

if(gb_assess){
  
}

if(sv_assess){
  #load(file.path(getwd(), "Results", "Support_Vector_Regression", "tuning_results_sv.svm.RData"))
  #load(file.path(getwd(), "Results", "Support_Vector_Regression", "finetuning_results_sv.svm.RData"))
  #load(file.path(getwd(), "Results", "Support_Vector_Regression", "performance_results_sv.svm.RData"))
  
}


# Assessing results -------------------------------------------------------

## RF =====================================================================
### Benchmarking analysis #################################################

# Best performer from set of same machine learning class
performance_benchmark_rf                                                   # randomForest yields best results

# Hyperparameter constellations of different implementations
performance_benchmark_rf$learners$randomForest$par.vals %>% 
  as_tibble() 
performance_benchmark_rf$learners$randomForestSRC$par.vals %>% 
  as_tibble()
performance_benchmark_rf$learners$ranger$par.vals %>% 
  as_tibble()

# Small number of trees and small tree size (minimum node size relatively large which implies a small tree size) for all implementations

# Hyperparameters of best learner
best_learner_rf <- generateHyperParsEffectData(tuning_results_rf.rf, 
                                               include.diagnostics = FALSE, 
                                               trafo = TRUE, 
                                               partial.dep = TRUE) %>% 
  .$data %>% 
  as_tibble() 

# Top parameter constellations
best_learner_rf10 <- best_learner_rf %>% 
  top_n(10*npar_rf, -mse.test.mean) %>% 
  arrange(mse.test.mean)

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

# Check test results
performance_benchmark$results$gdp_forecast$ranger$pred$data %>% 
  as_tibble() 

# Summarise results in fashion of econometric models
performance_benchmark$results$gdp_forecast$randomForest$pred$data %>% 
  as_tibble() %>% 
  filter(set == "test") %>% 
  mutate(MODEL_ID = "RF.rf") %>% 
  bind_rows(performance_benchmark$results$gdp_forecast$randomForestSRC$pred$data %>% 
              as_tibble() %>% 
              filter(set == "test") %>% 
              mutate(MODEL_ID = "RF.rfSRC")) %>% 
  bind_rows(performance_benchmark$results$gdp_forecast$ranger$pred$data %>% 
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
  mutate(INT_TREE = cut(ntree, breaks = seq(0,1000,length.out = 11))) %>%  
  group_by(INT_TREE) %>% 
  summarise(MEAN_MSE = mean(mse.test.mean)) %>% 
  ggplot(aes(x = INT_TREE, y = MEAN_MSE, group = 1)) + 
  geom_point() + 
  geom_path()

# Heat map
hyperparameters_rf %>% 
  filter(nodesize == 70) %>% 
  ggplot() +
  scale_x_continuous(breaks=ceiling(seq(ntree_low,ntree_up,length.out = 11)), labels=ceiling(seq(ntree_low,ntree_up,length.out = 11))) +
  scale_y_continuous(breaks=seq(mtry_low,mtry_up,length.out = 11), labels=seq(mtry_low,mtry_up,length.out = 11)) +
  geom_tile(aes(x = ntree, y = mtry, fill = mse.test.mean)) + 
  scale_fill_gradient(low = "green", high = "red")

# Line graph of number of trees from grid search
hyperparameters_rf %>% 
  group_by(ntree) %>% 
  summarise(MEAN_MSE = mean(mse.test.mean)) %>% 
  ggplot(aes(x = ntree, y = MEAN_MSE)) + 
  geom_point() + 
  geom_path()







## SV =====================================================================
### Benchmarking analysis #################################################
# Best performer from set of same machine learning class
performance_benchmark_sv

# Get hyperparameters from benchmark object
performance_benchmark_sv$learners


# Hyperparameters of best learner
best_learner_sv <- generateHyperParsEffectData(tuning_results_sv.svm, 
                                               include.diagnostics = FALSE, 
                                               trafo = TRUE, 
                                               partial.dep = TRUE) %>% 
  .$data %>% 
  as_tibble() 

# Top parameter constellations
best_learner_sv10 <- best_learner_sv %>% 
  top_n(10*npar_sv, -mse.test.mean) %>% 
  arrange(mse.test.mean)

# Extract 25% and 75% quantiles of top 10 results of all hyperparameters for lower and upper bound in finetuning
table(best_learner_sv10$kernel) # radial kernel most often in top 10
best_learner_sv10 <- best_learner_sv %>% 
  filter(kernel == "sigmoid") %>% 
  top_n(10*npar_sv, -mse.test.mean) %>% 
  arrange(mse.test.mean)

quantile(best_learner_sv10$cost, probs = 0.25)
quantile(best_learner_sv10$cost, probs = 0.75)
quantile(best_learner_sv10$epsilon, probs = 0.25)
quantile(best_learner_sv10$epsilon, probs = 0.75)




### Finetuning analysis ###################################################


performance_results_sv.svm

finetuning_results_sv.svm

hyperparameters_sv <- generateHyperParsEffectData(finetuning_results_sv.svm, 
                                                  include.diagnostics = FALSE, 
                                                  trafo = TRUE, 
                                                  partial.dep = TRUE) %>% 
  .$data %>% 
  as_tibble()






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




hyperparameters_sv %>% 
  filter(kernel == "sigmoid") %>% 
  ggplot() +
  scale_x_continuous(breaks=seq(epsilon_low,epsilon_up,length.out = 11), labels=seq(epsilon_low,epsilon_up,length.out = 11)) +
  scale_y_continuous(breaks=seq(cost_low,cost_up,length.out = 11), labels=seq(cost_low,cost_up,length.out = 11)) +
  geom_tile(aes(x = epsilon, y = cost, fill = mse.test.mean)) + 
  scale_fill_gradient(low = "green", high = "red")



# -------------------------------------------------------------------------