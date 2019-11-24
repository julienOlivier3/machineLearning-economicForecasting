# Steering ----------------------------------------------------------------
setwd("J:\\Studium\\Master\\Masterthesis")

# General
#training_start <- "1987 Q2"       # Set the first quarter of training data
training_end <- "2007 Q1"         # Set the last quarter of training data
forecasting_periods <- 1          # Set number of forecasting periods (number of steps-ahead forecasting)
forecasting_intervals <- 95       # Set forecasting confidence intervals
#testing_end <- "2016 Q4"          # Set the last period in which test data starts  
                                  # in the iterative process of model estimation 
                                  # (e.g. training_end = "2007 Q1", testing_end = "2007 Q2",
                                  # forecasting_periods = 10 forecasts 10 periods ahead with
                                  # only based on training data training_start ~ training_end.
                                  # It does not extend the training sample iteratively.)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ARIMA
ARIMA_order <- c(2, 0, 0)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# VAR
# Keynesian structural VAR
VAR_variables <- c("REAL_GDP_GROWTH", 
                   "FEDFUNDS", 
                   "UNRATE", 
                   "CPIAUCSL")

# Leading indicator VAR
VAR_variables <- c("REAL_GDP_GROWTH",
                   "HOUST",
                   "AMDMN_OX",
                   "S_P_500",
                   "UMCSEN_TX",
                   "AWHMAN",
                   "T5YFFM")

VAR_order <- 2


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# FAVAR
FAVAR_order <- 3
FAVAR_factor_n <- 1


#--------------------------------------------------------------------------
# Sourcing ----------------------------------------------------------------
source(file = file.path(getwd(), "Code", "src_setup.R"))

# Read data ---------------------------------------------------------------
load(file = file.path(getwd(), "Results", "tidy_data", "tidy_yx_yq_stat.RData"))





# Filter data -------------------------------------------------------------
# Drop GDP related data other than the target
tidy_yx_yq_stat <- tidy_yx_yq_stat %>% 
  select(-c(NOMINAL_GDP,                                                   # drop all GDP figures except target variable
            REAL_GDP,
            REAL_GDP_GROWTH_A,
            GDPC1,
            GDPC1_GROWTH, 
            GDPC1_GROWTH_ANNUALIZED))

# Change data type of last observation of training data
training_end <- training_end %>% 
  yearquarter() - (forecasting_periods-1)

# Data filtering
tidy_yx_yq_stat <- tidy_yx_yq_stat %>% 
  as_tsibble(index = DATE_QUARTER) %>%                                     # create tsibble with DATE_QUARTER as time index variable
  { if(exists("training_start"))                                           # filter dates if training starts at a later date as compared to existing history 
           filter_index(., yearquarter(training_start) ~ .)
           else . } %>%
  select(DATE_QUARTER, everything())


# Extract last observation period of overall data set
if(!exists("testing_end")){                                                # ..either the last quarter from data set
  testing_end <- tidy_yx_yq_stat %>% 
  select(DATE_QUARTER) %>% 
  filter(rownames(.) == nrow(.)) %>% 
  as_tibble() %>% 
  deframe() %>% 
  yearquarter() - (forecasting_periods-1)
} else {                                                                   # ..or according to the input in the steering fold
  testing_end <- testing_end %>% 
    yearquarter() - (forecasting_periods-1)
}

# Calculate number of different training iterations
trainings <- as.numeric(testing_end - training_end)#/forecasting_periods




# Model estimation & forecasting ------------------------------------------
# Initialize empty lists
RW_temp <- list()
ARIMA_temp <- list()
VAR_temp <- list()
FAVAR_temp <- list()

for (i in 1:trainings){

# Calculate first quarter of test data  
testing_start <- training_end + i

# Calculate last forecasting quarter
last_forecast <- training_end + i + (forecasting_periods - 1)



## RW =====================================================================
# Random Walk as simple benchmark
RW_temp[[i]] <- tidy_yx_yq_stat %>% 
  nest(.key = DATA) %>% 
  mutate(MODEL_ID = "RW",                                                  # define model identifier
         TRAINING_DATA = map(DATA, ~ filter_index(., ~ training_end + (i-1)) %>% 
                               as_tibble() %>% 
                               select(REAL_GDP_GROWTH)),                   # define data required for training the model 
         TRAINING_END = training_end + (i-1),                              # define last quarter of training data
         MODEL = map(TRAINING_DATA,                                        # estimate model
                     ~ arima(., order = c(0,1,0),                          # radom walk setting: y_t - y_(t-1) = eps_t 
                             #include.constant = FALSE,                     # no drifting paramter which would make the series trending and thus non-stationary
                             include.mean = FALSE)),                       # see package description (mean does not affext fit nor prediction if series is differenced)
         FORECAST_OBJECT = map(MODEL,                                      # create own variable for the forecasting object
                               ~ predict(., n.ahead = forecasting_periods,      
                                          level = forecasting_intervals,
                                         interval = "confidence")),
         FORECAST_PERIOD = map(DATA,                                       
                               ~ yearquarter(seq(as.Date(testing_start),   # define forecasted period
                                                 as.Date(last_forecast), 
                                                 by = "quarter"))),
         PERIODS_AHEAD = map(., ~ 1:forecasting_periods),                  # define indicator for the number of periods ahead forecasted by the model
         TRUE_VALUE = map(DATA, ~ select(.,DATE_QUARTER, REAL_GDP_GROWTH) %>%  # extract the realized GDP growth in that period
                            filter_index(testing_start ~ last_forecast) %>% 
                            as_tibble(.) %>% 
                            select(.,REAL_GDP_GROWTH) %>% 
                            { if(nrow(.) == forecasting_periods) . else add_row(., REAL_GDP_GROWTH=rep(NA, forecasting_periods-nrow(.))) } %>%        
                            # if forecast is in the future (i.e. there is no actual realization yet), fill with NA
                            unlist()),
         MEAN = map(FORECAST_OBJECT, ~ .$pred[1:forecasting_periods]),     # extract point forecast
         LOWER = map(FORECAST_OBJECT,                                      # calculate lower confidence level
                     ~ .$pred[1:forecasting_periods] 
                     + qnorm(p = (100-forecasting_intervals)/2/100)*.$se[1:forecasting_periods]),    
         UPPER = map(FORECAST_OBJECT,                                      # calculate upper confidence level
                     ~ .$pred[1:forecasting_periods] 
                     + qnorm(p = forecasting_intervals/100 + (100-forecasting_intervals)/2/100)*.$se[1:forecasting_periods])) %>% 
  select(TRAINING_END, everything())




## ARIMA ==================================================================
ARIMA_temp[[i]] <- tidy_yx_yq_stat %>% 
  nest(.key = DATA) %>% 
  mutate(MODEL_ID = "ARIMA",                                               # define model identifier
         TRAINING_DATA = map(DATA, ~ filter_index(., ~ training_end + (i-1)) %>% 
                               as_tibble() %>% 
                               select(REAL_GDP_GROWTH)),                       # define data required for training the model 
         TRAINING_END = training_end + (i-1),                              # define last quarter of training data
         MODEL = map(TRAINING_DATA,                                        # estimate model
                     ~ Arima(., order = ARIMA_order, 
                             include.constant = TRUE)),                    # include.constant=TRUE sets include.mean=TRUE if d=0 and include.drift=TRUE if d=1. Note that include.drift allows mu!=0 if d=1. This is the case here since we are looking GDP growth rates, i.e. a time series which has already been differenced. Therefore, in Arima d=0 but starting from GDP in levels, the calculation of GDP growth figures involves first differencing. Note also that if d=0 and include.drift=TRUE Arima estimates a drift parameter b*t.
         FORECAST_OBJECT = map(MODEL,                                      # create own variable for the forecasting object
                               ~ forecast(., h = forecasting_periods,      
                                          level = forecasting_intervals)),
         FORECAST_PERIOD = map(DATA,                                       
                               ~ yearquarter(seq(as.Date(testing_start),   # define forecasted period
                                                       as.Date(last_forecast), 
                                                       by = "quarter"))),
         PERIODS_AHEAD = map(., ~ 1:forecasting_periods),                  # define indicator for the number of periods ahead forecasted by the model
         TRUE_VALUE = map(DATA, ~ select(.,DATE_QUARTER, REAL_GDP_GROWTH) %>%  # extract the realized GDP growth in that period
                            filter_index(testing_start ~ last_forecast) %>% 
                            as_tibble(.) %>% 
                            select(.,REAL_GDP_GROWTH) %>% 
                            { if(nrow(.) == forecasting_periods) . else add_row(., REAL_GDP_GROWTH=rep(NA, forecasting_periods-nrow(.))) } %>%        
                            # if forecast is in the future (i.e. there is no actual realization yet), fill with NA
                            unlist()),
         MEAN = map(FORECAST_OBJECT, ~ .$mean[1:forecasting_periods]),     # extract point forecast
         LOWER = map(FORECAST_OBJECT, ~ .$lower[1:forecasting_periods]),   # extract lower confidence level
         UPPER = map(FORECAST_OBJECT, ~ .$upper[1:forecasting_periods])) %>%# extract upper confidence level 
  select(TRAINING_END, everything())




## VAR ====================================================================
VAR_temp[[i]] <- tidy_yx_yq_stat %>% 
  nest(.key = DATA) %>% 
  mutate(MODEL_ID = "VAR",
         TRAINING_DATA = map(DATA, ~ filter_index(., ~ training_end + (i-1)) %>% 
                               as_tibble() %>% 
                               select(VAR_variables)),
         TRAINING_END = training_end + (i-1),
         MODEL = map(TRAINING_DATA, 
                     ~ as.ts(.) %>% 
                       VAR(., p = VAR_order, 
                           type = "const")),
         FORECAST_OBJECT = map(MODEL, 
                               ~ forecast(., h = forecasting_periods, 
                                          level = forecasting_intervals)),
         FORECAST_PERIOD = map(DATA, ~ yearquarter(seq(as.Date(testing_start), 
                                                       as.Date(last_forecast), 
                                                       by = "quarter"))),
         PERIODS_AHEAD = map(., ~ 1:forecasting_periods),
         TRUE_VALUE = map(DATA, ~ select(.,DATE_QUARTER, REAL_GDP_GROWTH) %>%  # extract the realized GDP growth in that period
                            filter_index(testing_start ~ last_forecast) %>% 
                            as_tibble(.) %>% 
                            select(.,REAL_GDP_GROWTH) %>% 
                            { if(nrow(.) == forecasting_periods) . else add_row(., REAL_GDP_GROWTH=rep(NA, forecasting_periods-nrow(.))) } %>%        
                            # if forecast is in the future (i.e. there is no actual realization yet), fill with NA
                            unlist()),
         MEAN = map(FORECAST_OBJECT, ~ .$forecast$REAL_GDP_GROWTH$mean[1:forecasting_periods]),
         LOWER = map(FORECAST_OBJECT, ~ .$forecast$REAL_GDP_GROWTH$lower[1:forecasting_periods]),
         UPPER = map(FORECAST_OBJECT, ~ .$forecast$REAL_GDP_GROWTH$upper[1:forecasting_periods])) %>% 
  select(TRAINING_END, everything())




## FAVAR ==================================================================
FAVAR_temp[[i]] <- tidy_yx_yq_stat %>% 
  nest(.key = DATA) %>% 
  mutate(MODEL_ID = "FAVAR",
         FEATURE_SPACE = map(DATA, ~ select(., -contains("REAL_GDP_GROWTH")) %>%  
                                     as_tibble() %>% 
                                     select(-"DATE_QUARTER")),
         PCA = map(FEATURE_SPACE, ~ prcomp(.,
                                           center = TRUE,  
                                           scale. = TRUE, 
                                           rank. = FAVAR_factor_n)),
         PCA_SCORE = map(PCA, ~ .$x %>% 
                                as_tibble()),
         TRAINING_DATA = map(DATA, ~ select(., -c(colnames(FEATURE_SPACE[[1]]))) %>%                                  # drop all variables which have been considered for PCA
                               bind_cols(PCA_SCORE) %>% 
                               filter_index(~ training_end + (i-1)) %>% 
                               as_tibble()),
         TRAINING_END = training_end + (i-1),
         MODEL = map(TRAINING_DATA, 
                     ~ as.ts(.) %>% 
                       VAR(., p = FAVAR_order, 
                           type = "const")),
         FORECAST_OBJECT = map(MODEL, 
                               ~ forecast(., h = forecasting_periods, 
                                          level = forecasting_intervals)),
         FORECAST_PERIOD = map(DATA, ~ yearquarter(seq(as.Date(testing_start), 
                                                       as.Date(last_forecast), 
                                                       by = "quarter"))),
         PERIODS_AHEAD = map(., ~ 1:forecasting_periods),
         TRUE_VALUE = map(DATA, ~ select(.,DATE_QUARTER, REAL_GDP_GROWTH) %>%  # extract the realized GDP growth in that period
                            filter_index(testing_start ~ last_forecast) %>% 
                            as_tibble(.) %>% 
                            select(.,REAL_GDP_GROWTH) %>% 
                            { if(nrow(.) == forecasting_periods) . else add_row(., REAL_GDP_GROWTH=rep(NA, forecasting_periods-nrow(.))) } %>%        
                            # if forecast is in the future (i.e. there is no actual realization yet), fill with NA
                            unlist()),
         MEAN = map(FORECAST_OBJECT, ~ .$forecast$REAL_GDP_GROWTH$mean[1:forecasting_periods]),
         LOWER = map(FORECAST_OBJECT, ~ .$forecast$REAL_GDP_GROWTH$lower[1:forecasting_periods]),
         UPPER = map(FORECAST_OBJECT, ~ .$forecast$REAL_GDP_GROWTH$upper[1:forecasting_periods])) %>% 
  select(TRAINING_END, everything())

}


# Bind list elements rowwise
RW_model <- RW_temp %>%
  bind_rows() %>% 
  mutate(TRAINING_END = yearquarter(TRAINING_END))
ARIMA_model <- ARIMA_temp %>%
  bind_rows() %>% 
  mutate(TRAINING_END = yearquarter(TRAINING_END))
VAR_model <- VAR_temp %>%
  bind_rows() %>% 
  mutate(TRAINING_END = yearquarter(TRAINING_END))
FAVAR_model <- FAVAR_temp %>% 
  bind_rows() %>% 
  mutate(TRAINING_END = yearquarter(TRAINING_END))


# Extract forecast error of RW model for relative error measures
rw_error <- RW_model %>% 
  select(PERIODS_AHEAD, MEAN) %>% 
  as_tibble() %>% 
  unnest() %>% 
  filter(PERIODS_AHEAD == forecasting_periods) %>% 
  select(MEAN)


# Unnest results and calculate error measures
forecasting_models <- ARIMA_model %>% 
  bind_rows(VAR_model) %>% 
  bind_rows(RW_model) %>% 
  bind_rows(FAVAR_model %>% select(-c(FEATURE_SPACE,
                                   PCA,
                                   PCA_SCORE))) %>% 
  select(MODEL_ID, TRAINING_END, PERIODS_AHEAD, FORECAST_PERIOD, 
         TRUE_VALUE, MEAN, LOWER, UPPER) %>% 
  as_tibble() %>% 
  unnest() %>% 
  filter(PERIODS_AHEAD == forecasting_periods) %>%                        # only extract the forecast which equals the stepsize in the one-step-ahead forecast (e.g. for a one year one-step-ahead forecast the forecast of interest is the 4-step ahead forecast in time series models (based on quarterly data))
  mutate(TRAINING_END = yearquarter(TRAINING_END),
         FORECAST_PERIOD = yearquarter(FORECAST_PERIOD),
         ERROR = TRUE_VALUE - MEAN,
         REL_ERROR = MEAN/as_vector(rw_error))



# Generalization model performance
forecasting_models %>%
  group_by(MODEL_ID) %>%             
  summarise(MSE = mean(ERROR^2),                                           # calculate mean squared error (MSE)
            RMSE = sqrt(mean(ERROR^2)),                                    # calculate root mean squared error (RMSE)
            MdRAE = median(abs(REL_ERROR))) %>%                            # calculate Median Relative Absolute Error (MdRAE)
  ungroup() %>%  
  mutate(RelRMSE = RMSE/(.[.$MODEL_ID=="RW", "RMSE"] %>% pull()))          # calculate relative RMSE (RelRMSE); for this purpose extract RMSE for Random Walk and pull it as numeric value from the tibble





# Test for significance ---------------------------------------------------
# Diebold-Mariano test for predictive accuracy
dm.test(e1 = as_vector(forecasting_models[forecasting_models$MODEL_ID == "RW", "ERROR"]),
        e2 = as_vector(forecasting_models[forecasting_models$MODEL_ID == "ARIMA", "ERROR"]),
        alternative = "greater",                                           # if p < 0.1 one can reject the H_0 in favor of the the alternative hypothesis which states that method 2 is more accurate than method 1
        h = 1,                                                             # forecasting horizon to produce these forecasts is 1
        power = 2)                                                         # power used in the loss function is 2 (MSE)

dm.test(e1 = as_vector(forecasting_models[forecasting_models$MODEL_ID == "RW", "ERROR"]),
        e2 = as_vector(forecasting_models[forecasting_models$MODEL_ID == "VAR", "ERROR"]),
        alternative = "greater",
        h = 1,
        power = 2)



# Visualization -----------------------------------------------------------
tidy_yx_yq_stat %>% 
  select(DATE_QUARTER, REAL_GDP_GROWTH) %>% 
  mutate(RW = NA,
         ARIMA = NA,
         VAR = NA) %>% 
  melt(id = c("DATE_QUARTER", "REAL_GDP_GROWTH"), variable.name = "MODEL_ID") %>% 
  as_tibble() %>% 
  mutate(MODEL_ID = as.character(MODEL_ID)) %>% 
  select(-value) %>% 
  left_join(forecasting_models, by = c("DATE_QUARTER" = "FORECAST_PERIOD", "MODEL_ID")) %>% 
  select(MODEL_ID, DATE_QUARTER, REAL_GDP_GROWTH, MEAN, LOWER, UPPER) %>%
  mutate(DATE_QUARTER = yearquarter(DATE_QUARTER)) %>% 
  ggplot() +
  geom_line(aes(x = DATE_QUARTER, y = REAL_GDP_GROWTH), linetype = 5) +
  geom_line(aes(x = DATE_QUARTER, y = MEAN), color = "red") +
  geom_ribbon(aes(x = DATE_QUARTER, ymin = LOWER, ymax = UPPER), fill = "blue", alpha = .25) +
  facet_wrap(~ MODEL_ID, dir = "v") +
  theme_thesis


#--------------------------------------------------------------------------
# Miscellaneous -----------------------------------------------------------

# Forecast checking
psi_1 <- ARIMA_model$MODEL[1][[1]]$coef[1]
mu <- ARIMA_model$MODEL[1][[1]]$coef[2]
y_t_1 <- ARIMA_model$MODEL[[1]]$x[nrow(ARIMA_model$MODEL[[1]]$x),]
c <- (1-psi_1)*mu
sample_mean <- mean(as_vector(ARIMA_model$MODEL[1][[1]]$x))

mu - sample_mean # difference between sample mean and mu due to maximum likelihhod estimation of mu
psi_1*y_t_1 + c # forecast value
ARIMA_model$FORECAST_OBJECT[1][[1]]$mean[1]

# Accuracy checking
accuracy(forecast(Arima(tidy_yx_yq_stat$REAL_GDP_GROWTH[1:nrow(ARIMA_model$TRAINING_DATA[[1]])], order = c(1,0,0)), h = 52), 
         x = ARIMA_model$DATA[[1]]$REAL_GDP_GROWTH[(nrow(ARIMA_model$TRAINING_DATA[[1]])+1):nrow(ARIMA_model$DATA[[1]])])

tidy_yx_yq_stat %>% 
  select(DATE_QUARTER, REAL_GDP_GROWTH) %>% 
  nest(.key = DATA) %>% 
  mutate(MODEL = map(DATA, ~ filter_index(., ~ training_end) %>%
                       as_tibble() %>% 
                       select(REAL_GDP_GROWTH) %>% 
                          Arima(., order = ar_order, include.constant = TRUE)), 
         FORECAST_OBJECT = map(MODEL, ~ forecast(., h = 52, level = forecasting_intervals)),
         MEAN = map(FORECAST_OBJECT, ~ .$mean[1:52]),
         TRUE_VALUE = )
