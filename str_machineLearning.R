# Steering ----------------------------------------------------------------
setwd("J:\\Studium\\Master\\Masterthesis")

# GENERAL
#training_start <- "1987 Q2"       # Set the first quarter of training data
training_end <- "2007 Q1"         # Set the last quarter of training data, the subsequent quarter is the first quarter to be forecasted (Note: Go to "2007 Q1" once the latest observation is "2019 Q2")
forecasting_periods <- 1          # Set number of forecasting periods (n-ahead forecast, need to coincide with max_lag)
max_lag <- 1                      # Set the highest lag order considered in the feature space


# mlr-SPECIFIC
# Cross Validation
growing <- TRUE                   # If TRUE, cross validation follows a growing window strategy, if FALSE a sliding window strategy
horizon <- 1                      # Set the number of observations in the test set (default = 1, since we are doing one-step ahead forecasts)
skip <- 0.2                       # Set the fraction of training/validation sets which should be skipped (choose one of: 0.1, 0.2 & 0.5; the higher the less CVs)

# Note: possibly better to choose 0.1 as then after each 5 month (50*0.1 = 5 (see initial.window below)) forecast in the validation phase is made. This is quite acyclical ensuring that periods of expansion and periods of contraction are considered during validation

# TUNING
tuning_resolution <- 11           # Set the number of equally spaced parameter values picked form the grid of each parameter in grid search (finetuning)
tuning_factor <-  100             # Determine the number of iterations in random search (tuning_factor*npar (first-stage tuning))

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
initial.window <- ncol(data_training) - 50                                 # Set the number of observations of the overall trainig data in the first training subset. 

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
source(file = file.path(getwd(), "Code", "str_machineLearning_rf.R"))


### Gradient Boosting #####################################################
#source(file = file.path(getwd(), "Code", "str_machineLearning_gb.R"))


### Support Vector Regression #############################################
#source(file = file.path(getwd(), "Code", "str_machineLearning_sv.R"))


# -------------------------------------------------------------------------




# - continue tuning with blocked cv: check
# - check how Hyndman is doing resampling: check (one step growing window)
# - depending on this, use caret or try to use mlr-forecasting some way: check (mlr has this option!!! both growing window and sliding window)
# - worst case: proceed with wrong cv: check (not necessary)
# - check for more efficient tuning techniques: check advanced tuning (combination of two tuning methods. tried grid search, random search and iterated F-racing)
# - prepare machine learning pipeline for randomForest and gradient boosting (check)
# - store results (all results econometrics and ml) as an R file (check)
# - consider fine tuning (check)
# - think about further ML algorithms (SVM, lasso (for time series!?), recurrent neural network???) (check: worked out SVR)
# - implement additional ml models (maybe if time permits)
# - improve coding in ml R-files 
# - clean up steering parameters in ml, make sure no observations are left out in cv (IMPORTANT: in rf sampling is with replacement as for now. This is not valid for ts; also look at bootstrap parameter in SRC)
#   # + done for rf
# - write rf, tune rf
# - reread gb, tune gb
# - start with FAVAR and implement it
# - 

