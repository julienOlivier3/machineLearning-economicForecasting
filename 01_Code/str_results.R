# Steering ----------------------------------------------------------------
setwd("J:\\Studium\\Master\\Masterthesis")
#rm(list = ls())

# Generalization performance
inf_set <- 3                  # define which information set shall be considered
rel_errorM <- "FAVAR_FULL"    # define against which model shall be benchmarked

# Generalization performance DM-Test
hypo <- "greater"           # define the alternative hypothesis in DM-Test ("two.sided": method 1 and method 2 have different levels of accuracy, "greater": method 2 is more accurate than method 1)
power <- 2                    # choose power in loss function 
adjusted <- FALSE              # small sample adjustment to the original DM-Test
dm_implementation <- 2        # define which of the two DM-Test implementations shall be used for visualization (1: dm.test(), 2: DM.test())

# Crisis visualization
first_plot_date <- "2007 Q3"  # define first date which shall be plotted
last_plot_date <- "2010 Q2"   # define last date which shall be plotted
forecast_horizon <- "Y"       # define which forecast horizon shall be plotted 

#--------------------------------------------------------------------------
# Sourcing ----------------------------------------------------------------
source(file = file.path(getwd(), "Code", "src_setup.R"))


# Read data ---------------------------------------------------------------
## Econometrics ===========================================================

load(file = file.path(getwd(), "Results", "Crisis", "forecasting_models_Q.RData"))
ECO_Q <- forecasting_models

load(file = file.path(getwd(), "Results", "Crisis", "forecasting_models_Y.RData"))
ECO_Y <- forecasting_models
remove(forecasting_models)





## RF =====================================================================
# Target
load(file = file.path(getwd(), "Results", "Crisis", "RF", "TARGET", "performance_benchmark_rf_Q.RData"))
RF_Q_TARGET <- performance_benchmark_rf$results$gdp_forecast$randomForestSRC

load(file = file.path(getwd(), "Results", "Crisis", "RF", "TARGET", "performance_benchmark_rf_Y.RData"))
RF_Y_TARGET <- performance_benchmark_rf$results$gdp_forecast$randomForestSRC
remove(performance_benchmark_rf)

# Lead Indicators
load(file = file.path(getwd(), "Results", "Crisis", "RF", "LI", "performance_benchmark_rf_Q.RData"))
RF_Q_LI <- performance_benchmark_rf$results$gdp_forecast$randomForest

load(file = file.path(getwd(), "Results", "Crisis", "RF", "LI", "performance_benchmark_rf_Y.RData"))
RF_Y_LI <- performance_benchmark_rf$results$gdp_forecast$randomForestSRC
remove(performance_benchmark_rf)

# Full Feature Space
load(file = file.path(getwd(), "Results", "Crisis", "RF", "performance_benchmark_rf_Q.RData"))
RF_Q <- performance_benchmark_rf$results$gdp_forecast$ranger
remove(performance_benchmark_rf)

load(file = file.path(getwd(), "Results", "Crisis", "RF", "performance_results_rf_Y.RData"))
RF_Y <- performance_results_rf
remove(performance_results_rf)


## GB =====================================================================
# Target
load(file = file.path(getwd(), "Results", "Crisis", "GB", "TARGET", "performance_benchmark_gb_Q.RData"))
GB_Q_TARGET <- performance_benchmark_gb$results$gdp_forecast$xgboost

load(file = file.path(getwd(), "Results", "Crisis", "GB", "TARGET", "performance_benchmark_gb_Y.RData"))
GB_Y_TARGET <- performance_benchmark_gb$results$gdp_forecast$xgboost
remove(performance_benchmark_gb)

# Lead Indicators
load(file = file.path(getwd(), "Results", "Crisis", "GB", "LI", "performance_benchmark_gb_Q.RData"))
GB_Q_LI <- performance_benchmark_gb$results$gdp_forecast$xgboost

load(file = file.path(getwd(), "Results", "Crisis", "GB", "LI", "performance_benchmark_gb_Y.RData"))
GB_Y_LI <- performance_benchmark_gb$results$gdp_forecast$xgboost
remove(performance_benchmark_gb)

# Full Feature Space
load(file = file.path(getwd(), "Results", "Crisis", "GB", "performance_results_gb_Q.RData"))
GB_Q <- performance_results_gb

load(file = file.path(getwd(), "Results", "Crisis", "GB", "performance_results_gb_Y.RData"))
GB_Y <- performance_results_gb
remove(performance_results_gb)


## SVR ====================================================================
# Target
load(file = file.path(getwd(), "Results", "Crisis", "SVR", "TARGET", "performance_benchmark_sv_Q.RData"))
SVR_Q_TARGET <- performance_benchmark_sv$results$gdp_forecast$svm

load(file = file.path(getwd(), "Results", "Crisis", "SVR", "TARGET", "performance_benchmark_sv_Y.RData"))
SVR_Y_TARGET <- performance_benchmark_sv$results$gdp_forecast$svm
remove(performance_benchmark_sv)

# Lead Indicators
load(file = file.path(getwd(), "Results", "Crisis", "SVR", "LI", "performance_benchmark_sv_Q.RData"))
SVR_Q_LI <- performance_benchmark_sv$results$gdp_forecast$svm

load(file = file.path(getwd(), "Results", "Crisis", "SVR", "LI", "performance_benchmark_sv_Y.RData"))
SVR_Y_LI <- performance_benchmark_sv$results$gdp_forecast$svm
remove(performance_benchmark_sv)

# Full Feature Space
load(file = file.path(getwd(), "Results", "Crisis", "SVR", "performance_results_sv_Q.RData"))
SVR_Q <- performance_results_sv
remove(performance_results_sv)

load(file = file.path(getwd(), "Results", "Crisis", "SVR", "performance_benchmark_sv_Y.RData"))
SVR_Y <- performance_benchmark_sv$results$gdp_forecast$svm
remove(performance_benchmark_sv)







## Prepare data ===========================================================
# First create table with starting and ending dates
start_recession <- c("1948 Q4",
                     "1953 Q3",
                     "1957 Q3",
                     "1960 Q2",
                     "1970 Q1",
                     "1974 Q1",
                     "1980 Q1",
                     "1981 Q3",
                     "1990 Q3",
                     "2001 Q2",
                     "2008 Q1")


end_recession <- c("1949 Q3",
                   "1954 Q2",
                   "1958 Q1",
                   "1961 Q1",
                   "1970 Q4",
                   "1975 Q1",
                   "1980 Q2",
                   "1982 Q4",
                   "1991 Q1",
                   "2001 Q4",
                   "2009 Q2")

# Now put it in a table which is used in geom_rect()
data_recessions <- tibble(start_recession = start_recession, end_recession = end_recession) %>% 
  mutate(start_recession = as.yearqtr(start_recession),
         end_recession = as.yearqtr(end_recession))

# Information sets --------------------------------------------------------

if(inf_set==1){
# Information set 1 \ Quarter-ahead
models_I_Q <- c("RW_Q", "ARIMA_Q", "RF_Q_I1", "GB_Q_I1", "SVR_Q_I1")
# Information set 1 \ Year-ahead
models_I_Y <- c("RW_Y", "ARIMA_Y", "RF_Y_I1", "GB_Y_I1", "SVR_Y_I1")
}


if(inf_set==2){
# Information set 2 \ Quarter-ahead
models_I_Q <- c("RW_Q", "ARIMA_Q", "VAR_LI_Q", "VAR_NK_Q", "RF_Q_I2", "GB_Q_I2", "SVR_Q_I2")
# Information set 2 \ Year-ahead
models_I_Y <- c("RW_Y", "ARIMA_Y", "VAR_LI_Y", "VAR_NK_Y", "RF_Y_I2", "GB_Y_I2", "SVR_Y_I2")
}


if(inf_set==3){
# Information set 3 \ Quarter-ahead
models_I_Q <- c("RW_Q", "ARIMA_Q", "VAR_LI_Q", "VAR_NK_Q", "FAVAR_FULL_Q", "FAVAR_SW_Q", "RF_Q_I3", "GB_Q_I3", "SVR_Q_I3")
# Information set 3 \ Year-ahead
models_I_Y <- c("RW_Y", "ARIMA_Y", "VAR_LI_Y", "VAR_NK_Y", "FAVAR_FULL_Y", "FAVAR_SW_Y", "RF_Y_I3", "GB_Y_I3", "SVR_Y_I3")
}


# Error data --------------------------------------------------------------

data_results <- ECO_Q %>% 
  mutate(MODEL_ID = str_c(MODEL_ID, "_Q")) %>% 
  bind_rows(ECO_Y %>% 
              mutate(MODEL_ID = str_c(MODEL_ID, "_Y"))) %>% 
  bind_rows(help_function(df=RF_Q_TARGET, id_name = "RF_Q_I1", flag_se = FALSE, horizon = "Q"),
            help_function(df=RF_Y_TARGET, id_name = "RF_Y_I1", flag_se = FALSE, horizon = "Y"),
            help_function(df=RF_Q_LI, id_name = "RF_Q_I2", flag_se = TRUE, horizon = "Q"),
            help_function(df=RF_Y_LI, id_name = "RF_Y_I2", flag_se = FALSE, horizon = "Y"),
            help_function(df=RF_Q, id_name = "RF_Q_I3", flag_se = TRUE, horizon = "Q"),
            help_function(df=RF_Y, id_name = "RF_Y_I3", flag_se = TRUE, horizon = "Y"),
            help_function(df=GB_Q_TARGET, id_name = "GB_Q_I1", flag_se = FALSE, horizon = "Q"),
            help_function(df=GB_Y_TARGET, id_name = "GB_Y_I1", flag_se = FALSE, horizon = "Y"),
            help_function(df=GB_Q_LI, id_name = "GB_Q_I2", flag_se = FALSE, horizon = "Q"),
            help_function(df=GB_Y_LI, id_name = "GB_Y_I2", flag_se = FALSE, horizon = "Y"),
            help_function(df=GB_Q, id_name = "GB_Q_I3", flag_se = FALSE, horizon = "Q"),
            help_function(df=GB_Y, id_name = "GB_Y_I3", flag_se = FALSE, horizon = "Y"),
            help_function(df=SVR_Q_TARGET, id_name = "SVR_Q_I1", flag_se = FALSE, horizon = "Q"),
            help_function(df=SVR_Y_TARGET, id_name = "SVR_Y_I1", flag_se = FALSE, horizon = "Y"),
            help_function(df=SVR_Q_LI, id_name = "SVR_Q_I2", flag_se = FALSE, horizon = "Q"),
            help_function(df=SVR_Y_LI, id_name = "SVR_Y_I2", flag_se = FALSE, horizon = "Y"),
            help_function(df=SVR_Q, id_name = "SVR_Q_I3", flag_se = FALSE, horizon = "Q"),
            help_function(df=SVR_Y, id_name = "SVR_Y_I3", flag_se = FALSE, horizon = "Y"))
  
  

# Calculate relative error ------------------------------------------------

rel_error_Q <- data_results %>%                                            # from results extract ...
  filter(MODEL_ID==str_c(rel_errorM,"_Q")) %>%                             # ... model against which benchmarks shall be calculated
  select(ERROR)                                                            # select forecast error

rel_error_Y <- data_results %>%                                            # from results extract ...
  filter(MODEL_ID==str_c(rel_errorM,"_Y")) %>%                             # ... model against which benchmarks shall be calculated
  select(ERROR)                                                            # select forecast error

# Generalization performance ----------------------------------------------

options(pillar.sigfig = 4)
 
data_acc <- data_results %>%
  { if(exists("models_I_Q"))                                               # select models wich are relevant given the information set
    filter(.,MODEL_ID %in% models_I_Q)
    else . } %>%
  mutate(REL_ERROR = ERROR/as_vector(rel_error_Q)) %>%                       # calculate relative error
  group_by(MODEL_ID) %>%  
  summarise(RMSE = sqrt(mean(ERROR^2))                                     # calculate root mean squared error (RMSE)
            #,MdAE = median(abs(ERROR))                                     # calculate median absolute error (MdAE)
            #,MAE = mean(abs(ERROR))                                        # calculate mean absolute error (MdAE)
            ,MdRAE = median(abs(REL_ERROR))                                # calculate Median Relative Absolute Error (MdRAE)
  ) %>%                            
  ungroup() %>%  
  mutate(RelRMSE = RMSE/(.[.$MODEL_ID==str_c(rel_errorM,"_Q"), "RMSE"] %>% pull())     # calculate relative RMSE (RelRMSE); for this purpose extract RMSE for Random Walk and pull it as numeric value from the tibble
         #,RelMdAE = MdAE/(.[.$MODEL_ID==rel_errorM, "MdAE"] %>% pull())
         #,RelMAE = MAE/(.[.$MODEL_ID==rel_errorM, "MAE"] %>% pull())
  ) %>%
  { if(exists("models_I_Y"))                                           
    arrange(.,match(MODEL_ID, models_I_Q))                                 # order rows
    else . } %>% 
  bind_cols(data_results %>%
              { if(exists("models_I_Y"))                                               # select models wich are relevant given the information set
                filter(.,MODEL_ID %in% models_I_Y)
                else . } %>%
              mutate(REL_ERROR = ERROR/as_vector(rel_error_Y)) %>%                       # calculate relative error
              group_by(MODEL_ID) %>%  
              summarise(RMSE = sqrt(mean(ERROR^2))                                     # calculate root mean squared error (RMSE)
                        #,MdAE = median(abs(ERROR))                                     # calculate median absolute error (MdAE)
                        #,MAE = mean(abs(ERROR))                                        # calculate mean absolute error (MdAE)
                        ,MdRAE = median(abs(REL_ERROR))                                # calculate Median Relative Absolute Error (MdRAE)
              ) %>%                            
              ungroup() %>%  
              mutate(RelRMSE = RMSE/(.[.$MODEL_ID==str_c(rel_errorM,"_Y"), "RMSE"] %>% pull())     # calculate relative RMSE (RelRMSE); for this purpose extract RMSE for Random Walk and pull it as numeric value from the tibble
                     #,RelMdAE = MdAE/(.[.$MODEL_ID==rel_errorM, "MdAE"] %>% pull())
                     #,RelMAE = MAE/(.[.$MODEL_ID==rel_errorM, "MAE"] %>% pull())
              ) %>%
              { if(exists("models_I_Y"))                                           
                arrange(.,match(MODEL_ID, models_I_Y))                                 # order rows
                else . })

data_acc


# Diebold-Mariano test for predictive accuracy ----------------------------

## All models in information set against benchmark ========================


models_DM_Q <- models_I_Q[-which(models_I_Q == str_c(rel_errorM, "_Q"))]
models_DM_Y <- models_I_Y[-which(models_I_Y == str_c(rel_errorM, "_Y"))]

data_DM <- matrix(nrow = length(models_DM_Q), 
                  ncol = 2, 
                  dimnames = list(c(models_DM_Q), c("Q", "Y"))) %>% 
  as_tibble(rownames = "MODELS")


for (m in 1:length(models_DM_Q)){
  data_DM[m,"Q"] <- dm.test(e1 = as_vector(rel_error_Q),
                          e2 = as_vector(data_results %>% filter(MODEL_ID==models_DM_Q[m]) %>% select(ERROR)),
                          alternative = hypo,                               # if p < 0.01 one can reject the H_0 in favor of the the alternative hypothesis which states that method 2 is more accurate than method 1
                          h = 1,                                            # forecasting horizon to produce these forecasts
                          power = power) %>%                                # power used in the loss function is 2 (RMSE)
    .$p.value
  
  data_DM[m,"Y"] <- dm.test(e1 = as_vector(rel_error_Y),
                          e2 = as_vector(data_results %>% filter(MODEL_ID==models_DM_Y[m]) %>% select(ERROR)),
                          alternative = hypo,                               # if p < 0.01 one can reject the H_0 in favor of the the alternative hypothesis which states that method 2 is more accurate than method 1
                          h = 4,                                            # forecasting horizon to produce these forecasts
                          power = power) %>%                                # power used in the loss function is 2 (RMSE)
    .$p.value
  
  
}

data_DM


## All ML models against all other models =================================



models_DM_Q <- models_I_Q[-which(models_I_Q %in% c("RF_Q_I3", "GB_Q_I3", "SVR_Q_I3"))]
models_DM_Y <- models_I_Y[-which(models_I_Y %in% c("RF_Y_I3", "GB_Y_I3", "SVR_Y_I3"))]


### DM-Test 1 #############################################################

data_DM <- matrix(nrow = length(models_DM_Q), 
                  ncol = 6, 
                  dimnames = list(c(models_DM_Q), 
                                  c("RF_Q_I3", "GB_Q_I3", "SVR_Q_I3", 
                                    "RF_Y_I3", "GB_Y_I3", "SVR_Y_I3"))) %>% 
  as_tibble(rownames = "MODELS")


for (m in 1:length(models_DM_Q)){
  data_DM[m,"RF_Q_I3"] <- dm.test(e1 = as_vector(data_results %>% filter(MODEL_ID==models_DM_Q[m]) %>% select(ERROR)),
                            e2 = as_vector(data_results %>% filter(MODEL_ID=="RF_Q_I3") %>% select(ERROR)),
                            alternative = hypo,                               # if p < 0.01 one can reject the H_0 in favor of the the alternative hypothesis which states that method 2 is more accurate than method 1
                            h = 1,                                            # forecasting horizon to produce these forecasts
                            power = power) %>%                                # power used in the loss function is 2 (RMSE)
    .$p.value
  
  data_DM[m,"GB_Q_I3"] <- dm.test(e1 = as_vector(data_results %>% filter(MODEL_ID==models_DM_Q[m]) %>% select(ERROR)),
                                  e2 = as_vector(data_results %>% filter(MODEL_ID=="GB_Q_I3") %>% select(ERROR)),
                                  alternative = hypo,                               # if p < 0.01 one can reject the H_0 in favor of the the alternative hypothesis which states that method 2 is more accurate than method 1
                                  h = 1,                                            # forecasting horizon to produce these forecasts
                                  power = power) %>%                                # power used in the loss function is 2 (RMSE)
    .$p.value
  
  data_DM[m,"SVR_Q_I3"] <- dm.test(e1 = as_vector(data_results %>% filter(MODEL_ID==models_DM_Q[m]) %>% select(ERROR)),
                                  e2 = as_vector(data_results %>% filter(MODEL_ID=="SVR_Q_I3") %>% select(ERROR)),
                                  alternative = hypo,                               # if p < 0.01 one can reject the H_0 in favor of the the alternative hypothesis which states that method 2 is more accurate than method 1
                                  h = 1,                                            # forecasting horizon to produce these forecasts
                                  power = power) %>%                                # power used in the loss function is 2 (RMSE)
    .$p.value
  

  data_DM[m,"RF_Y_I3"] <- dm.test(e1 = as_vector(data_results %>% filter(MODEL_ID==models_DM_Y[m]) %>% select(ERROR)),
                                   e2 = as_vector(data_results %>% filter(MODEL_ID=="RF_Y_I3") %>% select(ERROR)),
                                   alternative = hypo,                               # if p < 0.01 one can reject the H_0 in favor of the the alternative hypothesis which states that method 2 is more accurate than method 1
                                   h = 4,                                            # forecasting horizon to produce these forecasts
                                   power = power) %>%                                # power used in the loss function is 2 (RMSE)
    .$p.value
  
  data_DM[m,"GB_Y_I3"] <- dm.test(e1 = as_vector(data_results %>% filter(MODEL_ID==models_DM_Y[m]) %>% select(ERROR)),
                                  e2 = as_vector(data_results %>% filter(MODEL_ID=="GB_Y_I3") %>% select(ERROR)),
                                  alternative = hypo,                               # if p < 0.01 one can reject the H_0 in favor of the the alternative hypothesis which states that method 2 is more accurate than method 1
                                  h = 4,                                            # forecasting horizon to produce these forecasts
                                  power = power) %>%                                # power used in the loss function is 2 (RMSE)
    .$p.value
  
  data_DM[m,"SVR_Y_I3"] <- dm.test(e1 = as_vector(data_results %>% filter(MODEL_ID==models_DM_Y[m]) %>% select(ERROR)),
                                   e2 = as_vector(data_results %>% filter(MODEL_ID=="SVR_Y_I3") %>% select(ERROR)),
                                   alternative = hypo,                               # if p < 0.01 one can reject the H_0 in favor of the the alternative hypothesis which states that method 2 is more accurate than method 1
                                   h = 4,                                            # forecasting horizon to produce these forecasts
                                   power = power) %>%                                # power used in the loss function is 2 (RMSE)
    .$p.value
  
  
}

data_DM





### DM-Test 2 #############################################################
data_DM2 <- matrix(nrow = length(models_DM_Q), 
                  ncol = 6, 
                  dimnames = list(c(models_DM_Q), 
                                  c("RF_Q_I3", "GB_Q_I3", "SVR_Q_I3", 
                                    "RF_Y_I3", "GB_Y_I3", "SVR_Y_I3"))) %>% 
  as_tibble(rownames = "MODELS")


for (m in 1:length(models_DM_Q)){
  data_DM2[m,"RF_Q_I3"] <- DM.test(f2 = as_vector(data_results %>% filter(MODEL_ID==models_DM_Q[m]) %>% select(MEAN)),
                                  f1 = as_vector(data_results %>% filter(MODEL_ID=="RF_Q_I3") %>% select(MEAN)),
                                  y = as_vector(data_results %>% filter(MODEL_ID=="RF_Q_I3") %>% select(TRUE_VALUE)),
                                  H1 = ifelse(hypo=="greater", "more", "same"),     # if p < 0.01 one can reject the H_0 in favor of the the alternative hypothesis which states that method 2 is more accurate than method 1
                                  h = 1,                                            # forecasting horizon to produce these forecasts
                                  loss.type = ifelse(power==1, "AE", "SE"),         # power used in the loss function is 2 (RMSE)
                                  c = adjusted) %>%                                 # defines possible adjustment to original DM-Test
    .$p.value
  
  data_DM2[m,"GB_Q_I3"] <- DM.test(f2 = as_vector(data_results %>% filter(MODEL_ID==models_DM_Q[m]) %>% select(MEAN)),
                                   f1 = as_vector(data_results %>% filter(MODEL_ID=="GB_Q_I3") %>% select(MEAN)),
                                   y = as_vector(data_results %>% filter(MODEL_ID=="GB_Q_I3") %>% select(TRUE_VALUE)),
                                   H1 = ifelse(hypo=="greater", "more", "same"),     # if p < 0.01 one can reject the H_0 in favor of the the alternative hypothesis which states that method 2 is more accurate than method 1
                                   h = 1,                                            # forecasting horizon to produce these forecasts
                                   loss.type = ifelse(power==1, "AE", "SE"),         # power used in the loss function is 2 (RMSE)
                                   c = adjusted) %>%                                 # defines possible adjustment to original DM-Test
    .$p.value
  
  data_DM2[m,"SVR_Q_I3"] <- DM.test(f2 = as_vector(data_results %>% filter(MODEL_ID==models_DM_Q[m]) %>% select(MEAN)),
                                    f1 = as_vector(data_results %>% filter(MODEL_ID=="SVR_Q_I3") %>% select(MEAN)),
                                    y = as_vector(data_results %>% filter(MODEL_ID=="SVR_Q_I3") %>% select(TRUE_VALUE)),
                                    H1 = ifelse(hypo=="greater", "more", "same"),     # if p < 0.01 one can reject the H_0 in favor of the the alternative hypothesis which states that method 2 is more accurate than method 1
                                    h = 1,                                            # forecasting horizon to produce these forecasts
                                    loss.type = ifelse(power==1, "AE", "SE"),         # power used in the loss function is 2 (RMSE)
                                    c = adjusted) %>%                                 # defines possible adjustment to original DM-Test
    .$p.value
  
  
  data_DM2[m,"RF_Y_I3"] <- DM.test(f2 = as_vector(data_results %>% filter(MODEL_ID==models_DM_Q[m]) %>% select(MEAN)),
                                   f1 = as_vector(data_results %>% filter(MODEL_ID=="RF_Q_I3") %>% select(MEAN)),
                                   y = as_vector(data_results %>% filter(MODEL_ID=="RF_Q_I3") %>% select(TRUE_VALUE)),
                                   H1 = ifelse(hypo=="greater", "more", "same"),     # if p < 0.01 one can reject the H_0 in favor of the the alternative hypothesis which states that method 2 is more accurate than method 1
                                   h = 4,                                            # forecasting horizon to produce these forecasts
                                   loss.type = ifelse(power==1, "AE", "SE"),         # power used in the loss function is 2 (RMSE)
                                   c = adjusted) %>%                                 # defines possible adjustment to original DM-Test
    .$p.value
  
  data_DM2[m,"GB_Y_I3"] <- DM.test(f2 = as_vector(data_results %>% filter(MODEL_ID==models_DM_Q[m]) %>% select(MEAN)),
                                   f1 = as_vector(data_results %>% filter(MODEL_ID=="GB_Q_I3") %>% select(MEAN)),
                                   y = as_vector(data_results %>% filter(MODEL_ID=="GB_Q_I3") %>% select(TRUE_VALUE)),
                                   H1 = ifelse(hypo=="greater", "more", "same"),     # if p < 0.01 one can reject the H_0 in favor of the the alternative hypothesis which states that method 2 is more accurate than method 1
                                   h = 4,                                            # forecasting horizon to produce these forecasts
                                   loss.type = ifelse(power==1, "AE", "SE"),         # power used in the loss function is 2 (RMSE)
                                   c = adjusted) %>%                                 # defines possible adjustment to original DM-Test
    .$p.value
  
  data_DM2[m,"SVR_Y_I3"] <- DM.test(f2 = as_vector(data_results %>% filter(MODEL_ID==models_DM_Q[m]) %>% select(MEAN)),
                                    f1 = as_vector(data_results %>% filter(MODEL_ID=="SVR_Q_I3") %>% select(MEAN)),
                                    y = as_vector(data_results %>% filter(MODEL_ID=="SVR_Q_I3") %>% select(TRUE_VALUE)),
                                    H1 = ifelse(hypo=="greater", "more", "same"),     # if p < 0.01 one can reject the H_0 in favor of the the alternative hypothesis which states that method 2 is more accurate than method 1
                                    h = 4,                                            # forecasting horizon to produce these forecasts
                                    loss.type = ifelse(power==1, "AE", "SE"),         # power used in the loss function is 2 (RMSE)
                                    c = adjusted) %>%                                 # defines possible adjustment to original DM-Test
    .$p.value
  
  
}

data_DM2


### Accuracy increase #####################################################

data_DM_acc <- data_results %>%
  { if(exists("models_I_Q"))                                               # select models wich are relevant given the information set
    filter(.,MODEL_ID %in% models_I_Q)
    else . } %>%
  group_by(MODEL_ID) %>%  
  summarise(RMSE = sqrt(mean(ERROR^2))                                     # calculate root mean squared error (RMSE)
            #,MdAE = median(abs(ERROR))                                     # calculate median absolute error (MdAE)
            #,MAE = mean(abs(ERROR))                                        # calculate mean absolute error (MdAE)
            #,MdRAE = median(abs(REL_ERROR))                                # calculate Median Relative Absolute Error (MdRAE)
  ) %>%                            
  ungroup() %>% 
  mutate(RF_Q_I3 = c(.[.["MODEL_ID"]=="RF_Q_I3", "RMSE"]),
         GB_Q_I3 = c(.[.["MODEL_ID"]=="GB_Q_I3", "RMSE"]),
         SVR_Q_I3 = c(.[.["MODEL_ID"]=="SVR_Q_I3", "RMSE"])) %>% 
  unnest() %>% 
  mutate(RF_Q_I3 = 1-(RF_Q_I3/RMSE),
         GB_Q_I3 = 1-(GB_Q_I3/RMSE),
         SVR_Q_I3 = 1-(SVR_Q_I3/RMSE)) %>% 
  filter(MODEL_ID %in% models_DM_Q) %>% 
  rename(MODELS=MODEL_ID) %>% 
  arrange(match(MODELS, models_I_Q)) %>% 
  select(-RMSE) %>% 
  bind_cols(data_results %>%                                      # select models wich are relevant given the information set
                filter(MODEL_ID %in% models_I_Y) %>%
              group_by(MODEL_ID) %>%  
              summarise(RMSE = sqrt(mean(ERROR^2))                                     # calculate root mean squared error (RMSE)
                        #,MdAE = median(abs(ERROR))                                     # calculate median absolute error (MdAE)
                        #,MAE = mean(abs(ERROR))                                        # calculate mean absolute error (MdAE)
                        #,MdRAE = median(abs(REL_ERROR))                                # calculate Median Relative Absolute Error (MdRAE)
              ) %>%                            
              ungroup() %>% 
              mutate(RF_Y_I3 = c(.[.["MODEL_ID"]=="RF_Y_I3", "RMSE"]),
                     GB_Y_I3 = c(.[.["MODEL_ID"]=="GB_Y_I3", "RMSE"]),
                     SVR_Y_I3 = c(.[.["MODEL_ID"]=="SVR_Y_I3", "RMSE"])) %>% 
              unnest() %>% 
              mutate(RF_Y_I3 = 1-(RF_Y_I3/RMSE),
                     GB_Y_I3 = 1-(GB_Y_I3/RMSE),
                     SVR_Y_I3 = 1-(SVR_Y_I3/RMSE)) %>% 
              filter(MODEL_ID %in% models_DM_Y) %>% 
              rename(MODELS=MODEL_ID) %>% 
              arrange(match(MODELS, models_I_Y)) %>% 
              select(-c(RMSE, MODELS)))

data_DM_acc


### Visualization #########################################################

tikz("plot_dm.tex",
     height = 4,
     width = 6)

# { if(dm_implementation==1)
#   data_DM
#   else
#     data_DM2 } %>%
data_DM2 %>% 
  rename(BENCHMARK=MODELS) %>%
  gather(key = "MODEL", value = "PVALUE", -BENCHMARK) %>%
  mutate(PVALUE_RANGE = cut(PVALUE,
                            breaks = c(-Inf, 0.01, 0.05, 0.1, 1),
                            right = FALSE)) %>%
  mutate(HORIZON = ifelse(str_detect(MODEL, "Q"), "1-quarter ahead", "1-year ahead")) %>%
  left_join(data_DM_acc %>%
              rename(BENCHMARK=MODELS) %>%
              gather(key = "MODEL", value = "ACCURACY_INCREASE", -BENCHMARK),
            by = c("BENCHMARK", "MODEL")) %>%
  mutate(ACCURACY_INCREASE = round(ACCURACY_INCREASE, digits=2),
         INCREASE_LABEL = ifelse(ACCURACY_INCREASE>0,"Increase","Worsening"),
         ACCURACY_INCREASE = paste0(ACCURACY_INCREASE*100, "\\%"),
         MODEL = factor(MODEL, levels=c("RF_Q_I3",
                                        "GB_Q_I3",
                                        "SVR_Q_I3",
                                        "RF_Y_I3",
                                        "GB_Y_I3",
                                        "SVR_Y_I3")),
         BENCHMARK = factor(BENCHMARK, levels=c("RW_Q",
                                            "ARIMA_Q",
                                            "VAR_LI_Q",
                                            "VAR_NK_Q",
                                            "FAVAR_FULL_Q",
                                            "FAVAR_SW_Q",
                                            "RW_Y",
                                            "ARIMA_Y",
                                            "VAR_LI_Y",
                                            "VAR_NK_Y",
                                            "FAVAR_FULL_Y",
                                            "FAVAR_SW_Y"))) %>%
  ggplot() +
  geom_tile(aes(x = MODEL, y = BENCHMARK, fill = PVALUE_RANGE)) +
  facet_wrap(~HORIZON, scales = "free_x") +
  scale_fill_manual(breaks=c("[-Inf,0.01)", "[0.01,0.05)", "[0.05,0.1)", "[0.1,1)"),
                    labels=c("$<$ 0.01", "$<$ 0.05", "$<$ 0.1", "$\\geq$ 0.1"),
                    values=c(ml_green_dark, ml_green_medium, ml_green_light, ggplot2::alpha("lightgrey", 1))) +
  geom_text(aes(x = MODEL, y = BENCHMARK, label = ACCURACY_INCREASE, color = INCREASE_LABEL), size = 3) +
  scale_color_manual(values = c("black", "red")) +
  scale_x_discrete(labels = c("RF", "GB", "SVR")) +
  scale_y_discrete(labels = c("RW", "ARIMA", "LI VAR", "NK VAR", "Full FAVAR", "S\\&W FAVAR")) +
  xlab("Machine Learning Methods") +
  ylab("Econometric Models") +
  guides(fill=guide_legend(title = "$p$-value"),
         color=FALSE) +
  theme_thesis +
  theme(panel.grid.major.x = element_line(color = "grey90", size = 0.5))

dev.off()


# Crisis analysis ---------------------------------------------------------

## Periods forecasted correctly ===========================================
data_results %>% 
  select(MODEL_ID, FORECAST_PERIOD, TRUE_VALUE, MEAN) %>% 
  filter(.,MODEL_ID %in% models_I_Q) %>% 
  as_tsibble(key="MODEL_ID", index = "FORECAST_PERIOD") %>% 
  filter_index("2008-01-01" ~ "2009-04-01") %>% 
  as_tibble() %>% 
  mutate(MODEL_ID = factor(MODEL_ID, levels = c("RW_Q",
                                                "ARIMA_Q",
                                                "VAR_LI_Q",
                                                "VAR_NK_Q",
                                                "FAVAR_FULL_Q",
                                                "FAVAR_SW_Q",
                                                "RF_Q_I3",
                                                "GB_Q_I3",
                                                "SVR_Q_I3"),
                           labels = c("RW",
                                      "ARIMA",
                                      "LI VAR",
                                      "NK VAR",
                                      "Full FAVAR",
                                      "S&W FAVAR",
                                      "RF",
                                      "GB",
                                      "SVR"))) %>% 
  mutate(SIGN_FIT = ifelse(sign(TRUE_VALUE)==sign(MEAN), 1, 0)) %>% 
  group_by(MODEL_ID) %>% 
  summarise(NUMBER_PERIODS = n(),
            RIGHT_SIGN = sum(SIGN_FIT)) 

data_results %>% 
  select(MODEL_ID, FORECAST_PERIOD, TRUE_VALUE, MEAN) %>% 
  filter(.,MODEL_ID %in% models_I_Y) %>% 
  as_tsibble(key="MODEL_ID", index = "FORECAST_PERIOD") %>% 
  filter_index("2008-01-01" ~ "2009-04-01") %>% 
  as_tibble() %>% 
  mutate(MODEL_ID = factor(MODEL_ID, levels = c("RW_Y",
                                                "ARIMA_Y",
                                                "VAR_LI_Y",
                                                "VAR_NK_Y",
                                                "FAVAR_FULL_Y",
                                                "FAVAR_SW_Y",
                                                "RF_Y_I3",
                                                "GB_Y_I3",
                                                "SVR_Y_I3"),
                           labels = c("RW",
                                      "ARIMA",
                                      "LI VAR",
                                      "NK VAR",
                                      "Full FAVAR",
                                      "S&W FAVAR",
                                      "RF",
                                      "GB",
                                      "SVR"))) %>% 
  mutate(SIGN_FIT = ifelse(sign(TRUE_VALUE)==sign(MEAN), 1, 0)) %>% 
  group_by(MODEL_ID) %>% 
  summarise(NUMBER_PERIODS = n(),
            RIGHT_SIGN = sum(SIGN_FIT))


## Forecast visualizations ================================================

# Quarter
tikz("plot_crisisQ.tex", 
     height = 6,
     width = 6)


data_results %>% 
  select(MODEL_ID, FORECAST_PERIOD, TRUE_VALUE, MEAN, LOWER, UPPER) %>% 
  mutate(PERIOD = yearquarter(FORECAST_PERIOD)) %>% 
  as_tsibble(key="MODEL_ID", index = "PERIOD") %>% 
  filter_index(first_plot_date ~ last_plot_date) %>% 
  as_tibble() %>% 
  mutate(PERIOD = as.yearqtr(FORECAST_PERIOD)) %>% 
  filter(str_detect(MODEL_ID, forecast_horizon)) %>% 
  filter(!str_detect(MODEL_ID, "I1")) %>% 
  filter(!str_detect(MODEL_ID, "I2")) %>% 
  #filter(!str_detect(MODEL_ID, "RW")) %>% 
  mutate(MODEL_ID = factor(MODEL_ID, levels = c(str_c("RW_", forecast_horizon),
                                                str_c("ARIMA_", forecast_horizon),
                                                str_c("VAR_LI_", forecast_horizon),
                                                str_c("VAR_NK_", forecast_horizon),
                                                str_c("FAVAR_FULL_", forecast_horizon),
                                                str_c("FAVAR_SW_", forecast_horizon),
                                                str_c("RF_", forecast_horizon, "_I3"),
                                                str_c("GB_", forecast_horizon, "_I3"),
                                                str_c("SVR_", forecast_horizon, "_I3")),
                           labels = c("RW",
                                      "ARIMA",
                                      "LI VAR",
                                      "NK VAR",
                                      "Full FAVAR",
                                      "S\\&W FAVAR",
                                      "RF",
                                      "GB",
                                      "SVR"))) %>% 
  mutate(LOWER = ifelse(MODEL_ID=="RF", NA, LOWER),
         UPPER = ifelse(MODEL_ID=="RF", NA, UPPER)) %>% 
  ggplot() +
  geom_rect(data = data_recessions %>% filter(row.names(.)==11),
            aes(xmin = start_recession,
                xmax = end_recession,
                ymin = -Inf,
                ymax = Inf),
            fill = "grey60",
            alpha = 0.3) +
  geom_hline(yintercept = 0, lty = 2, color = "grey", size = 0.5) +
  geom_line(aes(x = PERIOD, y = TRUE_VALUE, color = "Actual"), linetype = 5) +
  geom_ribbon(aes(x = PERIOD, ymin = LOWER, ymax = UPPER), fill = ml_green_medium, alpha = .25) +
  geom_line(aes(x = PERIOD, y = MEAN, color = "Forecast")) +
  facet_wrap(~ MODEL_ID, dir = "h") +
  scale_x_yearqtr(format = "Q%q %y", n = 4) +
  xlab("Period") +
  ylab("Real GDP Growth\n (in \\%)") +
  scale_color_manual(name = "",
                    values = c("Forecast" = ml_green_dark, "Actual" = "black")
                    )+
  theme_thesis +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major.x = element_line(color = "grey90", size = 0.5),
        legend.position="top") 

dev.off()


# Year
tikz("plot_crisisY.tex", 
     height = 6,
     width = 6)


data_results %>% 
  select(MODEL_ID, FORECAST_PERIOD, TRUE_VALUE, MEAN, LOWER, UPPER) %>% 
  mutate(PERIOD = yearquarter(FORECAST_PERIOD)) %>% 
  as_tsibble(key="MODEL_ID", index = "PERIOD") %>% 
  filter_index(first_plot_date ~ last_plot_date) %>% 
  as_tibble() %>% 
  mutate(PERIOD = as.yearqtr(FORECAST_PERIOD)) %>% 
  filter(str_detect(MODEL_ID, forecast_horizon)) %>% 
  filter(!str_detect(MODEL_ID, "I1")) %>% 
  filter(!str_detect(MODEL_ID, "I2")) %>% 
  #filter(!str_detect(MODEL_ID, "RW")) %>% 
  mutate(MODEL_ID = factor(MODEL_ID, levels = c(str_c("RW_", forecast_horizon),
                                                str_c("ARIMA_", forecast_horizon),
                                                str_c("VAR_LI_", forecast_horizon),
                                                str_c("VAR_NK_", forecast_horizon),
                                                str_c("FAVAR_FULL_", forecast_horizon),
                                                str_c("FAVAR_SW_", forecast_horizon),
                                                str_c("RF_", forecast_horizon, "_I3"),
                                                str_c("GB_", forecast_horizon, "_I3"),
                                                str_c("SVR_", forecast_horizon, "_I3")),
                           labels = c("RW",
                                      "ARIMA",
                                      "LI VAR",
                                      "NK VAR",
                                      "Full FAVAR",
                                      "S\\&W FAVAR",
                                      "RF",
                                      "GB",
                                      "SVR"))) %>% 
  mutate(LOWER = ifelse(MODEL_ID=="RF", NA, LOWER),
         UPPER = ifelse(MODEL_ID=="RF", NA, UPPER)) %>% 
  ggplot() +
  geom_rect(data = data_recessions %>% filter(row.names(.)==11),
            aes(xmin = start_recession,
                xmax = end_recession,
                ymin = -Inf,
                ymax = Inf),
            fill = "grey60",
            alpha = 0.3) +
  geom_hline(yintercept = 0, lty = 2, color = "grey", size = 0.5) +
  geom_line(aes(x = PERIOD, y = TRUE_VALUE, color = "Actual"), linetype = 5) +
  geom_ribbon(aes(x = PERIOD, ymin = LOWER, ymax = UPPER), fill = ml_green_medium, alpha = .25) +
  geom_line(aes(x = PERIOD, y = MEAN, color = "Forecast")) +
  facet_wrap(~ MODEL_ID, dir = "h") +
  scale_x_yearqtr(format = "Q%q %y", n = 4) +
  xlab("Period") +
  ylab("Real GDP Growth\n (in \\%)") +
  scale_color_manual(name = "",
                     values = c("Forecast" = ml_green_dark, "Actual" = "black")
  )+
  theme_thesis +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major.x = element_line(color = "grey90", size = 0.5),
        legend.position="top") 

dev.off()



# Others ------------------------------------------------------------------


## Time series cross validation ===========================================

# Define some sample data
tikz("plot_tscv.tex",
     height = 3,
     width = 6)
par(mar=c(0,0,0,0))
plot(0,0,xlim=c(-1,34),ylim=c(0,1),
     xaxt="n",yaxt="n",bty="n",xlab="",ylab="",type="n")

arrows(x0 = 0, y0 = 1, x1 = 32, y1 = 1, 
       lwd = 1.5, length = 0.05)
points(x = 1:21, y = rep(1, 21), pch = 19, col = ml_green_dark)
points(x = 22:31, y = rep(1, 10), pch = 19, col = bb_red_dark)
brackets(x1 = 1, x2 = 21, y1 = 0.98, y2 = 0.98, h = -0.025, 
         curvature = 1, ticks = 0.5, type = 1)
text(x = 11, y = 0.93, labels = "Training set", cex = 0.75)
brackets(x1 = 22, x2 = 31, y1 = 0.98, y2 = 0.98, h = -0.025, 
         curvature = 1, ticks = 0.5, type = 1)
text(x = 26.5, y = 0.93, labels = "Test set", cex = 0.75)

start_y <- 0.8
gap_y <- 0.05 
n_arrow <- 5

brackets(x1 = 1, x2 = 4, y1 = start_y+0.02, y2 = start_y+0.02, h = 0.015, 
         curvature = 1, ticks = 0.5, type = 1)
text(x = 2.5, y = start_y+0.06, labels = "Training subset", cex = 0.75)

brackets(x1 = 4.5, x2 = 5.5, y1 = start_y+0.02, y2 = start_y+0.02, h = 0.015, 
         curvature = 1, ticks = 0.5, type = 1)
text(x = 7, y = start_y+0.06, labels = "Validation set", cex = 0.75)
arrows(x0 = 5, x1 = 7, y0 = start_y + 0.039, y1 = start_y + 0.045, 
       lwd = 0.25, length = 0.00)

for(j in 1:n_arrow)
{
  train <- 1:(20/n_arrow*j)
  val <- (20/n_arrow*j)+1
  rest <- ((20/n_arrow*j)+2):21

  arrows(x0 = 0, y0 = start_y-gap_y*(j-1), x1 = 22, y1 = start_y-gap_y*(j-1), 
         lwd = 1.5, length = 0.05)
  points(x = train, y = rep(start_y-gap_y*(j-1), length(train)), pch = 19, col = ml_green_dark)
  points(x = val, y = rep(start_y-gap_y*(j-1), 1), pch = 19, col = ml_green_medium, cex = 1.5)
  #points(x = val, y = rep(start_y-gap_y*(j-1), 1), pch = "F", col = ml_green_dark, cex = 0.5)
  if(j < n_arrow){
    points(x = rest, y = rep(start_y-gap_y*(j-1), length(rest)), pch = 19, col = ggplot2::alpha("grey60", 0.2))
  }
}
brackets(x1 = -0.2, x2 = -0.2, y1 = start_y, y2 = start_y-(gap_y*(n_arrow-1)), h = -0.5, 
         curvature = 1, ticks = 0.5, type = 1)
text(x = -1.2, y = start_y-((gap_y*(n_arrow-1))/2), labels = "Inner loop", srt = 90, cex = 0.75)



start_y <- 0.5
gap_y <- 0.05 
n_arrow <- 10
j <- 1

for(j in 1:n_arrow)
{
  green <- 1:21
  train <- 22:(22+10/n_arrow*(j-1))
  val <- 21+(10/n_arrow*j)
  rest <- (23+(j-1)):31
  
  arrows(x0 = 0, y0 = start_y-gap_y*(j-1), x1 = 32, y1 = start_y-gap_y*(j-1), 
         lwd = 1.5, length = 0.05)
  points(x = green, y = rep(start_y-gap_y*(j-1), length(green)), pch = 19, col = ml_green_dark)
  if(j != 1){
    points(x = train, y = rep(start_y-gap_y*(j-1), length(train)), pch = 19, col = bb_red_dark)
  } 
  
  points(x = val, y = rep(start_y-gap_y*(j-1), 1), pch = 19, col = bb_red_light, cex = 1.5)
  if(j < n_arrow){
    points(x = rest, y = rep(start_y-gap_y*(j-1), length(rest)), pch = 19, col = ggplot2::alpha("grey60", 0.2))
  }

  
}
brackets(x1 = -0.2, x2 = -0.2, y1 = start_y, y2 = start_y-(gap_y*(n_arrow-1)), h = -0.5, 
         curvature = 1, ticks = 0.5, type = 1)
text(x = -1.2, y = start_y-((gap_y*(n_arrow-1))/2), labels = "Outer loop", srt = 90, cex = 0.75)

text(33.5,1,"time", cex = 0.75)

dev.off()

## Flow chart =============================================================

# Define some sample data
tikz("plot_flow.tex",
     height = 3,
     width = 6)
grViz("
    digraph boxes_and_circles {
    graph [splines=ortho, nodesep=1]

        # left 'node' statements
        node [shape = box,
              fontname = ModernComputer,
              width = 2, 
              penwidth = 2,
              color = '#CCDAE5']
        a [label = 'Hyperparameters']
        b [label = 'Cross-validation']
        c [label = 'Best hyperparameters']

        # right node statements
        node [shape = box,
              fontname = ModernComputer,
              width = 2,
              penwidth = 2,
              color = '#00457D']
        d [label = 'Dataset']
        e [label = 'Training set']
        f [label = 'Test set']
        g [label = 'Tuned model']
        h [label = 'Final evaluation']

        # edge statements
        a->b->c
        c->g
        d->e->g->h
        d->f->h


        # define ranks
        subgraph {
            rank = same; a; d
        }

        subgraph {
            rank = same; b; e; f
        }
        subgraph {
            rank = same; c; g
        }
        
        
    }
")
dev.off()


