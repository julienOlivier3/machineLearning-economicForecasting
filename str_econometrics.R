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
# ARIMA
ARIMA_order <- c(1, 0, 0)

# VAR
VAR_variables <- c("MOST_RECENT", 
                   "FEDFUNDS", 
                   "UNRATE", 
                   "CPIAUCSL")
VAR_order <- 2

#--------------------------------------------------------------------------
# Sourcing ----------------------------------------------------------------
source(file = file.path(getwd(), "Code", "src_setup.R"))

# Read data ---------------------------------------------------------------
load(file = file.path(getwd(), "Data", "tidy_data", "tidy_yx_yq.RData"))



# Model estimation & forecasting ------------------------------------------

# Change data type of last observation of training data
training_end <- training_end %>% 
  yearquarter()

# Data cleaning
tidy_yx_yq <- tidy_yx_yq %>% 
  mutate(FIRST = ifelse(is.na(FIRST), SECOND, FIRST),                      # simple imputation for missing values of different gdq figures
         SECOND = ifelse(is.na(SECOND), (FIRST+THIRD)/2, SECOND),
         THIRD = ifelse(is.na(THIRD), SECOND, THIRD)) %>% 
  drop_na(FIRST:MOST_RECENT) %>%                                           # drop all rows with NA values for GDP
  drop_cols_any_na() %>%                                                   # drop all columns with any NA
  as_tsibble(index = DATE_QUARTER) %>%                                     # create tsibble with DATE_QUARTER as time index variable
  { if(exists("training_start"))                                           # filter dates if training starts at a later date as compared to existing history 
           filter_index(., yearquarter(training_start) ~ .)
           else . } %>%
  select(DATE_QUARTER, everything())


# Extract last observation period of overall data set
if(!exists("testing_end")){                                                # ..either the last quarter from data set
  testing_end <- tidy_yx_yq %>% 
  select(DATE_QUARTER) %>% 
  filter(rownames(.) == nrow(.)) %>% 
  as_tibble() %>% 
  deframe() %>% 
  yearquarter()
} else {                                                                   # ..or according to the input in the steering fold
  testing_end <- testing_end %>% 
    yearquarter()
}

# Calculate number of different training iterations
trainings <- as.numeric(testing_end - training_end)/forecasting_periods




#--------------------------------------------------------------------------
# Initialize empty lists
ARIMA_temp <- list()
VAR_temp <- list()

for (i in 1:trainings){

# Calculate first quarter of test data  
testing_start <- training_end + i

# Calculate last forecasting quarter
last_forecast <- training_end + i + (forecasting_periods - 1)

# ARIMA ===================================================================
ARIMA_temp[[i]] <- tidy_yx_yq %>% 
  nest(.key = DATA) %>% 
  mutate(MODEL_ID = "ARIMA",                                               # define model identifier
         TRAINING_DATA = map(DATA, ~ filter_index(., ~ training_end + (i-1)) %>% 
                               as_tibble() %>% 
                               select(MOST_RECENT)),                       # define data required for training the model 
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
         TRUE_VALUE = map(DATA, ~ select(.,DATE_QUARTER, MOST_RECENT) %>%  # extract the realized GDP growth in that period
                            filter_index(testing_start ~ last_forecast) %>% 
                            as_tibble(.) %>% 
                            select(.,MOST_RECENT) %>% 
                            { if(nrow(.) == forecasting_periods) . else add_row(., MOST_RECENT=rep(NA, forecasting_periods-nrow(.))) } %>%        
                            # if forecast is in the future (i.e. there is no actual realization yet), fill with NA
                            unlist()),
         MEAN = map(FORECAST_OBJECT, ~ .$mean[1:forecasting_periods]),     # extract point forecast
         LOWER = map(FORECAST_OBJECT, ~ .$lower[1:forecasting_periods]),   # extract lower confidence level
         UPPER = map(FORECAST_OBJECT, ~ .$upper[1:forecasting_periods])) %>%# extract upper confidence level 
  select(TRAINING_END, everything())


# VAR =====================================================================
VAR_temp[[i]] <- tidy_yx_yq %>% 
  nest(.key = DATA) %>% 
  mutate(MODEL_ID = "VAR",
         TRAINING_DATA = map(DATA, ~ filter_index(., ~ training_end + (i-1)) %>% 
                               as_tibble() %>% 
                               select(VAR_variables) %>% 
                               mutate(FEDFUNDS_STAT = c(NA, diff(FEDFUNDS)),                         # first difference of interest rate
                                      UNRATE_STAT = c(NA, diff(UNRATE)),                             # first difference of unemployment rate
                                      CPIAUCSL_STAT = c(NA, NA, diff(log(CPIAUCSL),                  # second difference of log of inflation rate
                                                                     differences = 2)*100)) %>% 
                               select(MOST_RECENT, FEDFUNDS_STAT, UNRATE_STAT, CPIAUCSL_STAT) %>% 
                               na.omit(.)),
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
         TRUE_VALUE = map(DATA, ~ select(.,DATE_QUARTER, MOST_RECENT) %>%  # extract the realized GDP growth in that period
                            filter_index(testing_start ~ last_forecast) %>% 
                            as_tibble(.) %>% 
                            select(.,MOST_RECENT) %>% 
                            { if(nrow(.) == forecasting_periods) . else add_row(., MOST_RECENT=rep(NA, forecasting_periods-nrow(.))) } %>%        
                            # if forecast is in the future (i.e. there is no actual realization yet), fill with NA
                            unlist()),
         MEAN = map(FORECAST_OBJECT, ~ .$forecast$MOST_RECENT$mean[1:forecasting_periods]),
         LOWER = map(FORECAST_OBJECT, ~ .$forecast$MOST_RECENT$lower[1:forecasting_periods]),
         UPPER = map(FORECAST_OBJECT, ~ .$forecast$MOST_RECENT$upper[1:forecasting_periods])) %>% 
  select(TRAINING_END, everything())

}

# Bind list elements rowwise
ARIMA_model <- ARIMA_temp %>%
  bind_rows() %>% 
  mutate(TRAINING_END = yearquarter(TRAINING_END))
VAR_model <- VAR_temp %>%
  bind_rows() %>% 
  mutate(TRAINING_END = yearquarter(TRAINING_END))


forecasting_models <- ARIMA_model %>% 
  bind_rows(VAR_model) %>% 
  select(MODEL_ID, TRAINING_END, PERIODS_AHEAD, FORECAST_PERIOD, TRUE_VALUE, MEAN, LOWER, UPPER) %>% 
  unnest() %>% 
  mutate(TRAINING_END = yearquarter(TRAINING_END),
         FORECAST_PERIOD = yearquarter(FORECAST_PERIOD))

# Model performance
forecasting_models %>%
  mutate(ERROR = TRUE_VALUE - MEAN) %>%
  group_by(MODEL_ID) %>% 
  summarise(ME = mean(ERROR),
            MSE = mean(ERROR^2),
            RMSE = sqrt(mean(ERROR^2)))



# Visualization
tidy_yx_yq %>% 
  select(DATE_QUARTER, MOST_RECENT) %>% 
  mutate(ARIMA = NA,
         VAR = NA) %>% 
  melt(id = c("DATE_QUARTER", "MOST_RECENT"), variable.name = "MODEL_ID") %>% 
  as_tibble() %>% 
  mutate(MODEL_ID = as.character(MODEL_ID)) %>% 
  select(-value) %>% 
  left_join(forecasting_models, by = c("DATE_QUARTER" = "FORECAST_PERIOD", "MODEL_ID")) %>% 
  select(MODEL_ID, DATE_QUARTER, MOST_RECENT, MEAN, LOWER, UPPER) %>%
  mutate(DATE_QUARTER = yearquarter(DATE_QUARTER)) %>% 
  ggplot() +
  geom_line(aes(x = DATE_QUARTER, y = MOST_RECENT), linetype = 5) +
  geom_line(aes(x = DATE_QUARTER, y = MEAN), color = "red") +
  geom_ribbon(aes(x = DATE_QUARTER, ymin = LOWER, ymax = UPPER), fill = "blue", alpha = .25) +
  facet_wrap(~ MODEL_ID, dir = "v") +
  theme_thesis


# Temporary ---------------------------------------------------------------
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
accuracy(forecast(Arima(tidy_yx_yq$MOST_RECENT[1:nrow(ARIMA_model$TRAINING_DATA[[1]])], order = c(1,0,0)), h = 52), 
         x = ARIMA_model$DATA[[1]]$MOST_RECENT[(nrow(ARIMA_model$TRAINING_DATA[[1]])+1):nrow(ARIMA_model$DATA[[1]])])

tidy_yx_yq %>% 
  select(DATE_QUARTER, MOST_RECENT) %>% 
  nest(.key = DATA) %>% 
  mutate(MODEL = map(DATA, ~ filter_index(., ~ training_end) %>%
                       as_tibble() %>% 
                       select(MOST_RECENT) %>% 
                          Arima(., order = ar_order, include.constant = TRUE)), 
         FORECAST_OBJECT = map(MODEL, ~ forecast(., h = 52, level = forecasting_intervals)),
         MEAN = map(FORECAST_OBJECT, ~ .$mean[1:52]),
         TRUE_VALUE = )
