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
    load(file.path(getwd(), "Results", "Crisis", "RF", "performance_results_rf_Q.RData"))
    load(file = file.path(getwd(), "Results", "Crisis", "RF", "finetuning_results_rf_Q.RData"))
  }
  
  if(Yr){
    load(file.path(getwd(), "Results", "Crisis", "RF", "performance_benchmark_rf_Y.RData"))
    load(file = file.path(getwd(), "Results", "Crisis", "RF", "tuning_results_rf.rf_Y.RData"))
    load(file.path(getwd(), "Results", "Crisis", "RF", "performance_results_rf_Y.RData"))
    load(file = file.path(getwd(), "Results", "Crisis", "RF", "finetuning_results_rf_Y.RData"))
  }
}

if(gb_assess){
  if(Qrtr){
    load(file.path(getwd(), "Results", "Crisis", "GB", "performance_benchmark_gb_Q.RData"))
    load(file = file.path(getwd(), "Results", "Crisis", "GB", "tuning_results_gb.xgb_Q.RData"))
    load(file.path(getwd(), "Results", "Crisis", "GB", "performance_results_gb_Q.RData"))
    load(file = file.path(getwd(), "Results", "Crisis", "GB", "finetuning_results_gb_Q.RData"))
  }
  
  if(Yr){
    load(file.path(getwd(), "Results", "Crisis", "GB", "performance_benchmark_gb_Y.RData"))
    load(file = file.path(getwd(), "Results", "Crisis", "GB", "tuning_results_gb.gbm_Y.RData"))
    load(file.path(getwd(), "Results", "Crisis", "GB", "performance_results_gb_Y.RData"))
    load(file = file.path(getwd(), "Results", "Crisis", "GB", "finetuning_results_gb_Y.RData"))
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



if(rf_assess){
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
best_learner_rf <- generateHyperParsEffectData(tuning_results_rf.ranger, 
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
# Hyperparameters of best learner

best_learner_rf <- generateHyperParsEffectData(tuning_results_rf.ranger, 
                                               include.diagnostics = FALSE, 
                                               trafo = TRUE, 
                                               partial.dep = TRUE) %>% 
  .$data %>% 
  as_tibble() 



# best_learner_rf %>% 
#   mutate(INT_NODESIZE = cut(min.node.size, breaks = seq(1,95,length.out = 9))) %>%  
#   group_by(INT_NODESIZE) %>% 
#   summarise(MEAN_MSE = mean(rmse.test.rmse)) %>% 
#   ggplot(aes(x = INT_NODESIZE, y = MEAN_MSE, group = 1)) + 
#   geom_point() + 
#   geom_path() +
#   theme_thesis
# # Interpretation: algorithm clearly favoring small trees




# Heat map from random search
tikz("plot_spaceRF.tex",
     height = 4,
     width = 6)

best_learner_rf %>% 
  mutate(INT_TREE = cut(num.trees, breaks = c(1,seq(100,1000,length.out = 10))),
         INT_NODESIZE = cut(min.node.size, breaks = c(1,15,25,35,45,55,65,75,85,95))) %>%  
  group_by(INT_TREE, INT_NODESIZE) %>% 
  summarise(MEAN_MSE = mean(rmse.test.rmse)) %>% 
  ungroup() %>% 
  ggplot(aes(x = INT_NODESIZE, y = INT_TREE, fill = MEAN_MSE, group = 1)) +
  geom_tile() + 
  scale_fill_gradient(name = "RMSE",
                      low = ggplot2::alpha(ml_green_medium,1), 
                      high = ggplot2::alpha(bb_red_medium,1)) +
  theme_thesis +
  theme(panel.grid.major.x = element_line(color = "grey90", size = 0.5)) +
  #guides(fill=guide_legend(title = "RMSE")) +
  xlab("Minimum Terminal Nodesize") +
  ylab("Number of Trees")
dev.off()
# Interpretation: algorithm clearly favoring small trees and rather indifferent regarding tree size




# # Heat map from grid search
# hyperparameters_rf %>% 
#   filter(mtry == finetuning_results_rf$x$mtry) %>% 
#   ggplot() +
#   scale_x_continuous(breaks=ceiling(seq(finetuning_results_rf$opt.path$par.set$pars$ntree$lower,
#                                         finetuning_results_rf$opt.path$par.set$pars$ntree$upper,length.out = 11)), 
#                      labels=ceiling(seq(finetuning_results_rf$opt.path$par.set$pars$ntree$lower,
#                                         finetuning_results_rf$opt.path$par.set$pars$ntree$upper,length.out = 11))) +
#   scale_y_continuous(breaks=seq(finetuning_results_rf$opt.path$par.set$pars$nodesize$lower,
#                                 finetuning_results_rf$opt.path$par.set$pars$nodesize$upper,length.out = 11), 
#                      labels=seq(finetuning_results_rf$opt.path$par.set$pars$nodesize$lower,
#                                 finetuning_results_rf$opt.path$par.set$pars$nodesize$upper,length.out = 11)) +
#   geom_tile(aes(x = ntree, y = nodesize, fill = mse.test.mean)) + 
#   scale_fill_gradient(low = "green", high = "red") +
#   theme_thesis
# # Interpretation: no insight



# # Line graph of number of trees from grid search
# hyperparameters_rf %>% 
#   group_by(num.trees) %>% 
#   summarise(MEAN_RMSE = mean(rmse.test.rmse)) %>% 
#   ggplot(aes(x = num.trees, y = MEAN_RMSE)) + 
#   geom_point() + 
#   geom_path() + 
#   theme_thesis
# # Interpretation: no insight





# Variable importance measure
# Load results
load(file.path(getwd(), "Results", "Crisis", "RF", "performance_results_rf_Q.RData"))
performance_results_rf_Q <- performance_results_rf
load(file.path(getwd(), "Results", "Crisis", "RF", "performance_results_rf_Y.RData"))
performance_results_rf_Y <- performance_results_rf

# Quarter
vim_rf <- matrix(data = NA, 
                 nrow = length(performance_results_rf_Q$models), 
                 ncol = length(performance_results_rf_Q$models[[1]]$features),
                 dimnames = list(1:length(performance_results_rf_Q$models),
                                 performance_results_rf_Q$models[[1]]$features)) %>% 
  as_tibble()
for (i in 1:length(performance_results_rf_Q$models)){
vim_rf[i,] <- bind_rows(getFeatureImportance(performance_results_rf_Q$models[[i]],
                                 type = 2)$res) 
}

imp_rf_Q <- sapply(vim_rf, mean) %>% 
  enframe(name = "VARIABLE", value = "MEAN_DECREASE_RSS") %>% 
  mutate(VARIABLE = map(VARIABLE, ~ gsub("_", "\\\\_", .)),
         VARIABLE = map(VARIABLE, ~ gsub("\\.\\.1", "\\\\textsubscript\\{\\$t-1\\$\\}", .))) %>% 
  unnest() %>% 
  arrange(-MEAN_DECREASE_RSS) %>% 
  top_n(10,MEAN_DECREASE_RSS) %>% 
  mutate(HORIZON = "1-quarter-ahead")


# Year
vim_rf <- matrix(data = NA, 
                 nrow = length(performance_results_rf_Y$models), 
                 ncol = length(performance_results_rf_Y$models[[1]]$features),
                 dimnames = list(1:length(performance_results_rf_Y$models),
                                 performance_results_rf_Y$models[[1]]$features)) %>% 
  as_tibble()
for (i in 1:length(performance_results_rf_Y$models)){
  vim_rf[i,] <- bind_rows(getFeatureImportance(performance_results_rf_Y$models[[i]],
                                               type = 2)$res) 
}

imp_rf_Y <- sapply(vim_rf, mean) %>% 
  enframe(name = "VARIABLE", value = "MEAN_DECREASE_RSS") %>% 
  mutate(VARIABLE = map(VARIABLE, ~ gsub("_", "\\\\_", .)),
         VARIABLE = map(VARIABLE, ~ gsub("\\.\\.1", "\\\\textsubscript\\{\\$t-4\\$\\}", .))) %>% 
  unnest() %>% 
  arrange(-MEAN_DECREASE_RSS) %>% 
  top_n(10,MEAN_DECREASE_RSS) %>% 
  mutate(HORIZON = "1-year-ahead")





tikz("plot_varimpRF.tex",
     height = 4,
     width = 6)

imp_rf_Q %>% 
  bind_rows(imp_rf_Y) %>% 
  ggplot() +
  geom_col(aes(y = MEAN_DECREASE_RSS, x = reorder_within(VARIABLE, MEAN_DECREASE_RSS, HORIZON)), 
           fill = ml_green_dark) +
  coord_flip() + 
  scale_x_reordered() +
  facet_wrap(~HORIZON, 
             scales = "free",
             nrow = 2) +
  theme_thesis +
  theme(panel.grid.major.x = element_line(color = "grey90", size = 0.5)) +
  xlab("Feature") +
  ylab("Mean Decrease in Impurity")

dev.off()

 
}

if(gb_assess){
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



### Visualizations ########################################################
best_learner_gb <- generateHyperParsEffectData(tuning_results_gb.xgb, 
                                               include.diagnostics = FALSE, 
                                               trafo = TRUE, 
                                               partial.dep = TRUE) %>% 
  .$data %>% 
  as_tibble() 



# Heat map from random search
tikz("plot_spaceGB.tex",
     height = 4,
     width = 6)

best_learner_gb %>% 
  mutate(INT_TREE = cut(nrounds, breaks = c(1,seq(100,1000,length.out = 10))),
         #ETA = cut(eta, breaks = c(0.001,0.005,0.01,0.05,0.1))
         ETA = cut(max_depth, breaks = c(1,2,3,4,5,6,7,8,9,10))) %>%  
  group_by(INT_TREE, ETA) %>% 
  summarise(MEAN_MSE = mean(rmse.test.rmse)) %>% 
  ungroup() %>% 
  ggplot(aes(x = ETA, y = INT_TREE, fill = MEAN_MSE, group = 1)) +
  geom_tile() + 
  scale_fill_gradient(name = "RMSE",
                      low = ggplot2::alpha(ml_green_medium,1), 
                      high = ggplot2::alpha(bb_red_medium,1)) +
  theme_thesis +
  theme(panel.grid.major.x = element_line(color = "grey90", size = 0.5)) +
  #guides(fill=guide_legend(title = "RMSE")) +
  xlab("Maximum Tree Depth") +
  ylab("Number of Trees")
dev.off()
# Interpretation: algorithm clearly favoring larger trees and rather indifferent regarding tree size



# Variable importance measure
# Load results
load(file.path(getwd(), "Results", "Crisis", "GB", "performance_results_gb_Q.RData"))
performance_results_gb_Q <- performance_results_gb
load(file.path(getwd(), "Results", "Crisis", "GB", "performance_results_gb_Y.RData"))
performance_results_gb_Y <- performance_results_gb

# Quarter
vim_gb <- matrix(data = NA, 
                 nrow = length(performance_results_gb_Q$models), 
                 ncol = length(performance_results_gb_Q$models[[1]]$features),
                 dimnames = list(1:length(performance_results_gb_Q$models),
                                 performance_results_gb_Q$models[[1]]$features)) %>% 
  as_tibble()
for (i in 1:length(performance_results_gb_Q$models)){
  vim_gb[i,] <- bind_rows(getFeatureImportance(performance_results_gb_Q$models[[i]])$res) 
}

imp_gb_Q <- sapply(vim_gb, mean) %>% 
  enframe(name = "VARIABLE", value = "MEAN_DECREASE_RSS") %>% 
  mutate(VARIABLE = map(VARIABLE, ~ gsub("_", "\\\\_", .)),
         VARIABLE = map(VARIABLE, ~ gsub("\\.\\.1", "\\\\textsubscript\\{\\$t-1\\$\\}", .))) %>% 
  unnest() %>% 
  arrange(-MEAN_DECREASE_RSS) %>% 
  mutate(MEAN_DECREASE_RSS = rescale(MEAN_DECREASE_RSS)) %>% 
  top_n(10,MEAN_DECREASE_RSS) %>% 
  mutate(HORIZON = "1-quarter-ahead")
  


# Year
vim_gb <- matrix(data = NA, 
                 nrow = length(performance_results_gb_Y$models), 
                 ncol = length(performance_results_gb_Y$models[[1]]$features),
                 dimnames = list(1:length(performance_results_gb_Y$models),
                                 performance_results_gb_Y$models[[1]]$features)) %>% 
  as_tibble()
for (i in 1:length(performance_results_gb_Y$models)){
  vim_gb[i,] <- bind_rows(getFeatureImportance(performance_results_gb_Y$models[[i]])$res) 
}

imp_gb_Y <- sapply(vim_gb, mean) %>% 
  enframe(name = "VARIABLE", value = "MEAN_DECREASE_RSS") %>% 
  mutate(VARIABLE = map(VARIABLE, ~ gsub("_", "\\\\_", .)),
         VARIABLE = map(VARIABLE, ~ gsub("\\.\\.1", "\\\\textsubscript\\{\\$t-4\\$\\}", .))) %>% 
  unnest() %>% 
  arrange(-MEAN_DECREASE_RSS) %>% 
  mutate(MEAN_DECREASE_RSS = rescale(MEAN_DECREASE_RSS)) %>% 
  top_n(10,MEAN_DECREASE_RSS) %>% 
  mutate(HORIZON = "1-year-ahead") 





tikz("plot_varimpGB.tex",
     height = 4,
     width = 6)

imp_gb_Q %>% 
  bind_rows(imp_gb_Y) %>% 
  ggplot() +
  geom_col(aes(y = MEAN_DECREASE_RSS, x = reorder_within(VARIABLE, MEAN_DECREASE_RSS, HORIZON)), 
           fill = ml_green_dark) +
  coord_flip() + 
  scale_x_reordered() +
  facet_wrap(~HORIZON, 
             scales = "free",
             nrow = 2) +
  theme_thesis +
  theme(panel.grid.major.x = element_line(color = "grey90", size = 0.5)) +
  xlab("Feature") +
  ylab("Relative Feature Influence")

dev.off()

  
}

if(sv_assess){
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



best_learner_svr <- generateHyperParsEffectData(tuning_results_sv.svm, 
                                               include.diagnostics = FALSE, 
                                               trafo = TRUE, 
                                               partial.dep = TRUE) %>% 
  .$data %>% 
  as_tibble() 



# Heat map from random search
tikz("plot_spaceSVR.tex",
     height = 4,
     width = 6)

best_learner_svr %>% 
  filter(kernel=="sigmoid") %>% 
  mutate(COST = cut(cost, breaks = c(0.01,0.1,1,10,100,1000,10000)),
         #ETA = cut(eta, breaks = c(0.001,0.005,0.01,0.05,0.1))
         EPS = cut(epsilon, breaks = c(0.00001,0.0001,0.001,0.01,0.1,1))) %>%  
  group_by(COST, EPS) %>% 
  summarise(MEAN_MSE = mean(rmse.test.rmse)) %>% 
  ungroup() %>% 
  ggplot(aes(x = EPS, y = COST, fill = MEAN_MSE, group = 1)) +
  geom_tile() + 
  scale_fill_gradient(name = "RMSE",
                      low = ggplot2::alpha(ml_green_medium,1), 
                      high = ggplot2::alpha(bb_red_medium,1),
                      trans = "log",
                      labels=trans_format("identity", function(x) round(x,0))) +
  #scale_fill_continuous(labels=trans_format("identity", function(x) round(x,0))) +
  theme_thesis +
  theme(panel.grid.major.x = element_line(color = "grey90", size = 0.5)) +
  #guides(fill=guide_legend(title = "RMSE")) +
  xlab("Epsilon") +
  ylab("Regularization Parameter")
dev.off()
# Interpretation: algorithm clearly favoring larger trees and rather indifferent regarding tree size


  
}

# -------------------------------------------------------------------------