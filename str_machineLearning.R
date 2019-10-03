# Steering ----------------------------------------------------------------
setwd("J:\\Studium\\Master\\Masterthesis")

# GENERAL
#training_start <- "1987 Q2"       # Set the first quarter of training data
training_end <- "2007 Q1"         # Set the last quarter of training data, the subsequent quarter is the first quarter to be forecasted
forecasting_periods <- 1          # Set number of forecasting periods
max_lag <- 1                      # Set the highest lag order considered in the feature space


# mlr-SPECIFIC
# Cross Validation
growing <- TRUE                   # If TRUE, cross validation follows a growing window strategy, if FALSE a sliding window strategy
horizon <- 1                      # Set the number of observations in the test set
initial.window <- 0.8             # Set the fraction of observations of the overall trainig data in the first training subset
skip <- 0.2                       # Set the fraction of training/validation sets which should be skipped

# improve the above parameters to have some sort of cyclicality (e.g. skip captures a complete average business cycle)

# Tuning
tuning_resolution <- 10           # Set the number of equally spaced parameter values picked form the grid of each parameter


# SPECIAL
forecasting_intervals <- 95       # Set forecasting confidence intervals
#testing_end <- "2016 Q4"          # Set the last period in which test data starts  
                                  # in the iterative process of model estimation 
                                  # (e.g. training_end = "2007 Q1", testing_end = "2007 Q2",
                                  # forecasting_periods = 10 forecasts 10 periods ahead with
                                  # only based on training data training_start ~ training_end.
                                  # It does not extend the training sample iteratively.)


#--------------------------------------------------------------------------
# Sourcing ----------------------------------------------------------------
source(file = file.path(getwd(), "Code", "src_setup.R"))

# Read data ---------------------------------------------------------------
load(file = file.path(getwd(), "Data", "tidy_data", "tidy_yx_yq_stat.RData"))




# Preprocessing data ------------------------------------------------------
# Note: In the long run this should be a seperate R-script. All preprocessing steps should be at one place (stationarizing, imputation etc.)

# Change data type of last observation of training data
training_end <- training_end %>% 
  yearquarter()


# Data cleaning
tidy_yx_yq_stat <- tidy_yx_yq_stat %>% 
  mutate(FIRST = ifelse(is.na(FIRST), SECOND, FIRST),                      # simple imputation for missing values of different gdq figures
         SECOND = ifelse(is.na(SECOND), (FIRST+THIRD)/2, SECOND),
         THIRD = ifelse(is.na(THIRD), SECOND, THIRD)) %>% 
  drop_na(FIRST:MOST_RECENT) %>%                                           # drop all rows with NA values for GDP
  drop_cols_any_na() %>%                                                   # drop all columns with any NA
  #mutate(DATE_QUARTER = as.Date(DATE_QUARTER)) %>% 
  as_tsibble(index = DATE_QUARTER) %>%                                    # create tsibble with DATE_QUARTER as time index variable
  { if(exists("training_start"))                                           # filter dates if training starts at a later date as compared to existing history 
    filter_index(., yearquarter(training_start) ~ .)
    else . } %>%
  select(DATE_QUARTER, everything())

# Create tsibble with dependent data (GDP Data). Splitting data into features and target is required for the introduction of lags
# in the feature space.
tidy_y_yq_stat <- tidy_yx_yq_stat %>%  
  select(DATE_QUARTER, FIRST, SECOND, THIRD, MOST_RECENT, 
         MOST_RECENT_ANNUALIZED, GDPC1, GDPC1_ANNUALIZED) %>% 
  as_tibble() %>%
  mutate(DATE_QUARTER = as.Date(DATE_QUARTER)) %>%                         # coercing as type Date required for subsequent lag.xts operation
  tq_mutate(select = NULL,                                                 # create lagged variables on all columns 
            mutate_fun = lag.xts,                                          # by means of lag.xts function from package timetk
            k = 1:max_lag) %>%                                             # number of lags to be calculated
  #select(c(DATE_QUARTER, matches("\\.\\d"))) %>%                          # select only column DATE_QUARTER and all columns which end with ".digit" (these are lagged variables). note: "..1" = lag 1, ".1" = lag 2, ".2" = lag 3 and so on so forth
  mutate(DATE_QUARTER = yearquarter(DATE_QUARTER)) %>%                     # recoerce DATE_QUARTER as qtr data type
  select(c(DATE_QUARTER, MOST_RECENT, MOST_RECENT..1))                     # only select one GDP series (the one of interest, needs to be defined beforehand)


# Create tsibble with independent variables and only keep lags, i.e. drop contemporaneous events (forecasting!)
tidy_x_yq_stat <- tidy_yx_yq_stat %>%  
  select(-c(FIRST, SECOND, THIRD, MOST_RECENT, 
            MOST_RECENT_ANNUALIZED, GDPC1, GDPC1_ANNUALIZED)) %>% 
  select(-contains("GDP", ignore.case = FALSE)) %>%                        # drop all GDP related variables
  as_tibble() %>%
  mutate(DATE_QUARTER = as.Date(DATE_QUARTER)) %>%                         # coercing as type Date required for subsequent lag.xts operation
  tq_mutate(select = NULL,                                                 # create lagged variables on all columns 
            mutate_fun = lag.xts,                                          # by means of lag.xts function from package timetk
            k = 1:max_lag) %>%                                             # number of lags to be calculated
  select(c(DATE_QUARTER, matches("\\.\\d"))) %>%                           # select only column DATE_QUARTER and all columns which end with ".digit" (these are lagged variables). note: "..1" = lag 1, ".1" = lag 2, ".2" = lag 3 and so on so forth
  mutate(DATE_QUARTER = yearquarter(DATE_QUARTER))                         # recoerce DATE_QUARTER as qtr data type

# Bind tsibble with dependent data and tsibble with independent variables
tidy_yx_yq_stat <- tidy_y_yq_stat %>% 
  bind_cols(tidy_x_yq_stat) %>%                                            # bind columns
  select(-DATE_QUARTER1) %>%                                               # drop DATE_QUARTER1 which results from binding
  na.omit() %>%                                                            # drop the first row as it contains missing values for the predictors (lagging!)
  as_tsibble(index = DATE_QUARTER)

# Model estimation & forecasting ------------------------------------------
## Training and Testing split =============================================

#MAKE SURE THAT THE NUMBER OF LAGS OF THE TARGET VALUE USED AS FEATURE VARIABLES IS DROPPED IN VALIDATION AND 
#TESTING SETS. THERE NEEDS ALWAYS BE A GAP OF SIZE EQUAL TO THE NUMBER OF LAGS BETWEEN TRAINING SUBSET AND 
#VALIDATION SET AND VALIDATION SET AND TESTING SET RESPECTIVELY. ONLY IN THIS WAY IT IS ENSURED THAT NO 
#TRAINING DATA IS USED IN MODEL CALIBRATION AND NO VALIDATION DATA IS USED IN GENERAL MODEL EVALUATION.

# Overall data
data_overall <- tidy_yx_yq_stat %>% 
  as_tibble() %>%                                                          # reconvert to tibble in order to drop date variable
  select(-DATE_QUARTER)

# Training data
# Target and features
data_training <- tidy_yx_yq_stat %>% 
  filter_index(~training_end) %>% 
  as_tibble() %>%                                                          # reconvert to tibble in order to drop date variable
  select(-DATE_QUARTER)


# Test data
# Target and features
data_test <- tidy_yx_yq_stat %>% 
  filter_index((training_end+1)~.) %>% 
  as_tibble() %>%                                                          # reconvert to tibble in order to drop date variable
  select(-DATE_QUARTER)



## Task ===================================================================
task_overall <- makeRegrTask(id = "gdp_forecast",                          # id as identifier for the respective task
                     data = as.data.frame(data_overall),                   # data available for modelling (here only training data). Define as data.frame as mlr does not work with tsibbles
                     target = "MOST_RECENT")                               # define target variable 

task_training <- makeRegrTask(id = "gdp_forecast_training",                # id as identifier for the respective task
                     data = as.data.frame(data_training),                  # data available for modelling (here only training data). Define as data.frame as mlr does not work with tsibbles
                     target = "MOST_RECENT")                               # define target variable 



## Resampling (Growing Window CV) =========================================
cv_tuning <- makeResampleDesc(method = ifelse(growing,                     # steering section allows to choose between.. 
                                              "GrowingWindowCV",           # ..growing window (= training data gets sequentially bigger)..
                                              "FixedWindowCV"              # ..and fixed window (= size of training data remains same while still rolling forward)
                                              ), 
                              horizon = horizon,                           # number of observations in the test set
                              initial.window = initial.window,             # fraction of observations from overall data included in the first training set
                              skip = skip,                                 # fraction defining how many training/test pairs to skip in the time series cv (the more one skips, the faster becomes execution of cv)
                              predict = "both")                            # predict test AND training observations (once everything works fine this can be changed to test only)

cv_test <- makeResampleDesc(method = ifelse(growing, 
                                            "GrowingWindowCV", 
                                            "FixedWindowCV"
                                            ), 
                            horizon = 1,                                   # in the outer loop (= performance evaluation) only 1 observations is in the test set since we are doing one-step-ahead forecasts
                            initial.window = nrow(data_training),          # given the optimal parameters from the inner loop, the growing window strategy in the outer loop starts with training on the overall training data. Then moving sequentially forward
                            skip = 1,                                      # no forecasts shall be skipped. forecasting of ALL observations in the test set
                            predict = "both")



## Models =================================================================

### Random Forest #########################################################
#source(file = file.path(getwd(), "Code", "str_machineLearning_rf.R"))


### Gradient Boosting #####################################################
#source(file = file.path(getwd(), "Code", "str_machineLearning_gb.R"))


### Support Vector Regression #############################################
source(file = file.path(getwd(), "Code", "str_machineLearning_sv.R"))


# -------------------------------------------------------------------------

# Model asssessment -------------------------------------------------------

## Loading results ========================================================

#load(file.path(getwd(), "Results", "Random_Forest", "performance_benchmark.RData"))


## Benchmarking analysis ==================================================

# Best performer from set of same machine learning class
performance_benchmark

# Hyperparameters of best learner
tuning_results_gb.gbm
temp <- generateHyperParsEffectData(tuning_results_gb.gbm, 
                            include.diagnostics = FALSE, 
                            trafo = TRUE, 
                            partial.dep = TRUE) %>% 
  .$data %>% 
  as_tibble()

# Top 10 parameter constellations
temp %>% 
  top_n(nrow(.)*0.1, -mse.test.mean) %>% 
  arrange(mse.test.mean)

# Visualizations
temp %>% 
  group_by(num.trees) %>% 
  summarise(MEDIAN_trees = median(mse.test.mean),
            MEAN_trees = mean(mse.test.mean)) %>% 
  ungroup() %>% 
  ggplot() +
  geom_line(aes(x = num.trees, y = MEDIAN_trees)) +
  geom_line(aes(x = num.trees, y = MEAN_trees), color = "red")

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


# Get hyperparameters from benchmark object
performance_benchmark_sv$learners








## Finetuning analysis ====================================================
performance_results_rf.ranger

finetuning_results_rf.ranger
temp2 <- generateHyperParsEffectData(finetuning_results_rf.ranger, 
                                             include.diagnostics = FALSE, 
                                             trafo = TRUE, 
                                             partial.dep = TRUE) %>% 
  .$data %>% 
  as_tibble()

temp2 %>%   
  top_n(20, -mse.test.mean) %>% 
  arrange(mse.test.mean)


## Visualizations =========================================================

### SV ####################################################################
plot_sv_heat1 <- generateHyperParsEffectData(tuning_results_sv.svm, 
                            include.diagnostics = FALSE, 
                            trafo = TRUE, 
                            partial.dep = TRUE) %>% 
  .$data %>% 
  as_tibble()


plot_sv_heat1 %>% 
  filter(kernel == "sigmoid") %>% 
  ggplot() +
  scale_y_log10(breaks=10^seq(-5,4,length.out = 10),labels=10^seq(-5,4,length.out = 10)) +
  scale_x_log10(breaks=10^seq(-9,0,length.out = 10),labels=10^seq(-9,0,length.out = 10)) +
  geom_tile(aes(x = epsilon, y = cost, fill = mse.test.mean)) + 
  scale_fill_gradient(low = "green", high = "red", trans = "log")

plot_sv_heat2 <- generateHyperParsEffectData(finetuning_results_sv.svm, 
                                             include.diagnostics = FALSE, 
                                             trafo = TRUE, 
                                             partial.dep = TRUE) %>% 
  .$data %>% 
  as_tibble()


plot_sv_heat2 %>% 
  filter(kernel == "sigmoid") %>% 
  ggplot() +
  scale_x_continuous(breaks=seq(0.2,0.3,length.out = 11), labels=seq(0.2,0.3,length.out = 11)) +
  scale_y_continuous(breaks=seq(0.4,0.5,length.out = 11), labels=seq(0.4,0.5,length.out = 11)) +
  geom_tile(aes(x = epsilon, y = cost, fill = mse.test.mean)) + 
  scale_fill_gradient(low = "green", high = "red")
  
# -------------------------------------------------------------------------


# - continue tuning with blocked cv: check
# - check how Hyndman is doing resampling: check (one step growing window)
# - depending on this, use caret or try to use mlr-forecasting some way: check (mlr has this option!!! both growing window and sliding window)
# - worst case: proceed with wrong cv: check (not necessary)
# - check for more efficient tuning techniques: check advanced tuning (stick with grid search)
# - prepare machine learning pipeline for randomForest and gradient boosting
# - store results (all results econometrics and ml) as an R file 
# - consider fine tuning
# - think about further ML algorithms (SVM, lasso (for time series!?), recurrent neural network???)
# - implement additional ml models
# - start with FAVAR and implement it
# - 

