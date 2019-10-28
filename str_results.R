# Steering ----------------------------------------------------------------
setwd("J:\\Studium\\Master\\Masterthesis")

#--------------------------------------------------------------------------
# Sourcing ----------------------------------------------------------------
source(file = file.path(getwd(), "Code", "src_setup.R"))


# Read data ---------------------------------------------------------------
load(file = file.path(getwd(), "Results", "tidy_data", "tidy_yx_yq_stat.RData"))
load(file = file.path(getwd(), "Results", "tidy_data", "tidy_yx_yq.RData"))


# Prepare data ------------------------------------------------------------
# REAL_GDP and REAL_GDP_GROWTH as time series
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
recessions <- tibble(start_recession = start_recession, end_recession = end_recession) %>% 
  mutate(start_recession = yearquarter(as.yearqtr(format(start_recession),       # change data types: convert first to lubridate::yearqtr and the to tsibble::yearquarter as the tsibble format is required for later operations
                                                  "%Y Q%q")),
         end_recession = yearquarter(as.yearqtr(format(end_recession),       # change data types: convert first to lubridate::yearqtr and the to tsibble::yearquarter as the tsibble format is required for later operations
                                                "%Y Q%q")))



# Leading Indicators ------------------------------------------------------
leading_i <- c(                   # define list of leading indicators
  "HOUST",
  "AMDMN_OX",
  "S_P_500",
  "UMCSEN_TX",
  "AWHMAN",
  "CPF3MTB3MX"
)

tidy_yx_yq %>% 
  #as_tsibble(index = DATE_QUARTER) %>% 
  #filter_index(yearquarter("2007 Q2") ~ .) %>% 
  select(DATE_QUARTER, leading_i) %>% 
  melt(id = c("DATE_QUARTER"), variable.name = "LEADING_INDICATOR") %>% 
  ggplot() +
  geom_line(aes(x = DATE_QUARTER, y = value, linetype = LEADING_INDICATOR), color = "#E7B800") +
  #geom_line(aes(x = DATE_QUARTER, y = MEAN), color = "#E7B800") +
  #geom_ribbon(aes(x = DATE_QUARTER, ymin = LOWER, ymax = UPPER), fill = "blue", alpha = .25) +
  facet_wrap(~ LEADING_INDICATOR, dir = "v", scales = "free_y") +
  geom_rect(data = recessions %>% filter(!(row.names(.) %in% c(1,2,3)))
            ,
            aes(xmin = start_recession,
                xmax = end_recession,
                ymin = -Inf,
                ymax = Inf),
            fill = "grey",
            alpha = 0.3) +
  xlab("Date") +
  ylab("") +
  theme_thesis



# 1Q ----------------------------------------------------------------------
# Read Data
load(file = file.path(getwd(), "Results", "Backup", "ZEW", "1Q", "forecasting_models.RData"))
load(file = file.path(getwd(), "Results", "Backup", "ZEW", "1Q", "performance_benchmark_rf.RData"))
load(file = file.path(getwd(), "Results", "Backup", "ZEW", "1Q", "performance_benchmark_gb.RData"))
load(file = file.path(getwd(), "Results", "Backup", "ZEW", "1Q", "performance_benchmark_sv.RData"))

# Define RW errors
rw_error <- forecasting_models %>% 
  filter(MODEL_ID == "RW") %>% 
  select(MEAN)

# Calculate error measures
forecasting_models <- performance_benchmark_rf$results$gdp_forecast$ranger$pred$data %>% 
  as_tibble() %>% 
  filter(set == "test") %>% 
  mutate(MODEL_ID = "RF") %>% 
  bind_rows(performance_benchmark_gb$results$gdp_forecast$gbm$pred$data %>% 
              as_tibble() %>% 
              filter(set == "test") %>% 
              mutate(MODEL_ID = "GB")) %>% 
  bind_rows(performance_benchmark_sv$results$gdp_forecast$svm$pred$data %>% 
              as_tibble() %>% 
              filter(set == "test") %>% 
              mutate(MODEL_ID = "SVR")) %>% 
  mutate(ERROR = truth - response) %>%
  rename(TRUE_VALUE=truth, MEAN = response) %>%
  mutate(FORECAST_PERIOD = rep(unique(forecasting_models$FORECAST_PERIOD), 3)) %>% 
  select(MODEL_ID, TRUE_VALUE, MEAN, ERROR, FORECAST_PERIOD) %>% 
  bind_rows(forecasting_models %>% 
              select(MODEL_ID, TRUE_VALUE, MEAN, ERROR, FORECAST_PERIOD)) %>% 
  mutate(REL_ERROR = MEAN/as_vector(rw_error)) 

forecasting_models %>% 
  group_by(MODEL_ID) %>%             
  summarise(MSE = mean(ERROR^2),                                           # calculate mean squared error (MSE)
            RMSE = sqrt(mean(ERROR^2)),                                    # calculate root mean squared error (RMSE)
            MRAE = mean(abs(REL_ERROR)),                                   # calculate Mean Relative Absolute Error (MRAE)
            MdRAE = median(abs(REL_ERROR))) %>%                            # calculate Median Relative Absolute Error (MdRAE)
  ungroup() %>%  
  mutate(RelRMSE = RMSE/(.[.$MODEL_ID=="RW", "RMSE"] %>% pull()),
         AccIMP = (1 - RelRMSE)*100) 

# Diebold-Mariano test for predictive accuracy
dm.test(e1 = as_vector(forecasting_models[forecasting_models$MODEL_ID == "RW", "ERROR"]),
        e2 = as_vector(forecasting_models[forecasting_models$MODEL_ID == "ARIMA", "ERROR"]),
        alternative = "greater",                                         # if p < 0.01 one can reject the H_0 in favor of the the alternative hypothesis which states that method 2 is more accurate than method 1
        h = 1,                                                             # forecasting horizon to produce these forecasts is 1
        power = 2)                                                         # power used in the loss function is 2 (MSE)

dm.test(e1 = as_vector(forecasting_models[forecasting_models$MODEL_ID == "RW", "ERROR"]),
        e2 = as_vector(forecasting_models[forecasting_models$MODEL_ID == "RF", "ERROR"]),
        alternative = "greater",
        h = 1,
        power = 2)

dm.test(e1 = as_vector(forecasting_models[forecasting_models$MODEL_ID == "RW", "ERROR"]),
        e2 = as_vector(forecasting_models[forecasting_models$MODEL_ID == "GB", "ERROR"]),
        alternative = "greater",
        h = 1,
        power = 2)

dm.test(e1 = as_vector(forecasting_models[forecasting_models$MODEL_ID == "RW", "ERROR"]),
        e2 = as_vector(forecasting_models[forecasting_models$MODEL_ID == "SVR", "ERROR"]),
        alternative = "greater",
        h = 1,
        power = 2)

# Visualization
tidy_yx_yq_stat %>% 
  as_tsibble(index = DATE_QUARTER) %>% 
  filter_index(yearquarter("2007 Q1") ~ .) %>% 
  select(DATE_QUARTER, REAL_GDP_GROWTH) %>% 
  mutate(
    #RW = NA,
    #ARIMA = NA,
    #VAR = NA,
    #RF = NA, 
    GB = NA,
    #SVR = NA
  ) %>% 
  melt(id = c("DATE_QUARTER", "REAL_GDP_GROWTH"), variable.name = "MODEL_ID") %>% 
  as_tibble() %>% 
  mutate(MODEL_ID = as.character(MODEL_ID)) %>% 
  select(-value) %>% 
  left_join(forecasting_models, by = c("DATE_QUARTER" = "FORECAST_PERIOD", "MODEL_ID")) %>% 
  #select(MODEL_ID, DATE_QUARTER, REAL_GDP_GROWTH, MEAN, LOWER, UPPER) %>%
  select(MODEL_ID, DATE_QUARTER, REAL_GDP_GROWTH, MEAN) %>%
  mutate(DATE_QUARTER = yearquarter(DATE_QUARTER)) %>% 
  ggplot() +
  geom_line(aes(x = DATE_QUARTER, y = REAL_GDP_GROWTH), linetype = 5) +
  geom_line(aes(x = DATE_QUARTER, y = MEAN), color = "red") +
  #geom_ribbon(aes(x = DATE_QUARTER, ymin = LOWER, ymax = UPPER), fill = "blue", alpha = .25) +
  facet_wrap(~ MODEL_ID, dir = "v") +
  theme_thesis



# 1Q_LI -------------------------------------------------------------------
# Read data
load(file = file.path(getwd(), "Results", "Backup", "ZEW", "1Q_LI", "forecasting_models.RData"))
load(file = file.path(getwd(), "Results", "Backup", "ZEW", "1Q_LI", "performance_benchmark_rf.RData"))
load(file = file.path(getwd(), "Results", "Backup", "ZEW", "1Q_LI", "performance_benchmark_gb.RData"))
load(file = file.path(getwd(), "Results", "Backup", "ZEW", "1Q_LI", "performance_benchmark_sv.RData"))


# Define RW errors
rel_error <- forecasting_models %>% 
  filter(MODEL_ID == "VAR") %>% 
  select(MEAN)

# Calculate error measures
forecasting_models <- performance_benchmark_rf$results$gdp_forecast$ranger$pred$data %>% 
  as_tibble() %>% 
  filter(set == "test") %>% 
  mutate(MODEL_ID = "RF") %>% 
  bind_rows(performance_benchmark_gb$results$gdp_forecast$xgboost$pred$data %>% 
              as_tibble() %>% 
              filter(set == "test") %>% 
              mutate(MODEL_ID = "GB")) %>% 
  bind_rows(performance_benchmark_sv$results$gdp_forecast$svm$pred$data %>% 
              as_tibble() %>% 
              filter(set == "test") %>% 
              mutate(MODEL_ID = "SVR")) %>% 
  mutate(ERROR = truth - response) %>%
  rename(TRUE_VALUE=truth, MEAN = response) %>%
  mutate(FORECAST_PERIOD = rep(unique(forecasting_models$FORECAST_PERIOD), 3)) %>% 
  select(MODEL_ID, TRUE_VALUE, MEAN, ERROR, FORECAST_PERIOD) %>% 
  bind_rows(forecasting_models %>% 
              select(MODEL_ID, TRUE_VALUE, MEAN, ERROR, FORECAST_PERIOD)) %>% 
  mutate(REL_ERROR = MEAN/as_vector(rel_error)) 

forecasting_models %>% 
  group_by(MODEL_ID) %>%             
  summarise(MSE = mean(ERROR^2),                                           # calculate mean squared error (MSE)
            RMSE = sqrt(mean(ERROR^2)),                                    # calculate root mean squared error (RMSE)
            MRAE = mean(abs(REL_ERROR)),                                   # calculate Mean Relative Absolute Error (MRAE)
            MdRAE = median(abs(REL_ERROR))) %>%                            # calculate Median Relative Absolute Error (MdRAE)
  ungroup() %>%  
  mutate(RelRMSE = RMSE/(.[.$MODEL_ID=="VAR", "RMSE"] %>% pull()),
         AccIMP = (1 - RelRMSE)*100) 

# Diebold-Mariano test for predictive accuracy
dm.test(e1 = as_vector(forecasting_models[forecasting_models$MODEL_ID == "RW", "ERROR"]),
        e2 = as_vector(forecasting_models[forecasting_models$MODEL_ID == "ARIMA", "ERROR"]),
        alternative = "greater",                                         # if p < 0.01 one can reject the H_0 in favor of the the alternative hypothesis which states that method 2 is more accurate than method 1
        h = 4,                                                             # forecasting horizon to produce these forecasts is 1
        power = 2)                                                         # power used in the loss function is 2 (MSE)

dm.test(e1 = as_vector(forecasting_models[forecasting_models$MODEL_ID == "RW", "ERROR"]),
        e2 = as_vector(forecasting_models[forecasting_models$MODEL_ID == "RF", "ERROR"]),
        alternative = "greater",
        h = 1,
        power = 2)

dm.test(e1 = as_vector(forecasting_models[forecasting_models$MODEL_ID == "RW", "ERROR"]),
        e2 = as_vector(forecasting_models[forecasting_models$MODEL_ID == "GB", "ERROR"]),
        alternative = "greater",
        h = 1,
        power = 2)

dm.test(e1 = as_vector(forecasting_models[forecasting_models$MODEL_ID == "RW", "ERROR"]),
        e2 = as_vector(forecasting_models[forecasting_models$MODEL_ID == "SVR", "ERROR"]),
        alternative = "greater",
        h = 1,
        power = 2)

# Visualization
tidy_yx_yq_stat %>% 
  as_tsibble(index = DATE_QUARTER) %>% 
  filter_index(yearquarter("2007 Q2") ~ .) %>% 
  select(DATE_QUARTER, REAL_GDP_GROWTH) %>% 
  mutate(
    RW = NA,
    #ARIMA = NA,
    #VAR = NA,
    #RF = NA, 
    GB = NA,
    #SVR = NA
  ) %>% 
  melt(id = c("DATE_QUARTER", "REAL_GDP_GROWTH"), variable.name = "MODEL_ID") %>% 
  as_tibble() %>% 
  mutate(MODEL_ID = as.character(MODEL_ID)) %>% 
  select(-value) %>% 
  left_join(forecasting_models, by = c("DATE_QUARTER" = "FORECAST_PERIOD", "MODEL_ID")) %>% 
  #select(MODEL_ID, DATE_QUARTER, REAL_GDP_GROWTH, MEAN, LOWER, UPPER) %>%
  select(MODEL_ID, DATE_QUARTER, REAL_GDP_GROWTH, MEAN) %>%
  mutate(DATE_QUARTER = yearquarter(DATE_QUARTER)) %>% 
  ggplot() +
  geom_line(aes(x = DATE_QUARTER, y = REAL_GDP_GROWTH), linetype = 5) +
  geom_line(aes(x = DATE_QUARTER, y = MEAN), color = "#E7B800") +
  #geom_ribbon(aes(x = DATE_QUARTER, ymin = LOWER, ymax = UPPER), fill = "blue", alpha = .25) +
  facet_wrap(~ MODEL_ID, dir = "v") +
  geom_rect(data = recessions %>% filter(row.names(.)==11),
            aes(xmin = start_recession,
                xmax = end_recession,
                ymin = -Inf,
                ymax = Inf),
            fill = "grey",
            alpha = 0.3) +
  xlab("Date") +
  ylab("Real GDP growth (quarterly) in %") +
  theme_thesis

# 1Y ----------------------------------------------------------------------
# Read data
load(file = file.path(getwd(), "Results", "Backup", "ZEW", "1Y", "forecasting_models.RData"))
load(file = file.path(getwd(), "Results", "Backup", "ZEW", "1Y", "performance_benchmark_rf.RData"))
load(file = file.path(getwd(), "Results", "Backup", "ZEW", "1Y", "performance_benchmark_gb.RData"))
load(file = file.path(getwd(), "Results", "Backup", "ZEW", "1Y", "performance_benchmark_sv.RData"))


# Define RW errors
rw_error <- forecasting_models %>% 
  filter(MODEL_ID == "RW") %>% 
  select(MEAN)

# Calculate error measures
forecasting_models <- performance_benchmark_rf$results$gdp_forecast$randomForest$pred$data %>% 
  as_tibble() %>% 
  filter(set == "test") %>% 
  mutate(MODEL_ID = "RF") %>% 
  bind_rows(performance_benchmark_gb$results$gdp_forecast$gbm$pred$data %>% 
              as_tibble() %>% 
              filter(set == "test") %>% 
              mutate(MODEL_ID = "GB")) %>% 
  bind_rows(performance_benchmark_sv$results$gdp_forecast$svm$pred$data %>% 
              as_tibble() %>% 
              filter(set == "test") %>% 
              mutate(MODEL_ID = "SVR")) %>% 
  mutate(ERROR = truth - response) %>%
  rename(TRUE_VALUE=truth, MEAN = response) %>%
  mutate(FORECAST_PERIOD = rep(unique(forecasting_models$FORECAST_PERIOD), 3)) %>% 
  select(MODEL_ID, TRUE_VALUE, MEAN, ERROR, FORECAST_PERIOD) %>% 
  bind_rows(forecasting_models %>% 
              select(MODEL_ID, TRUE_VALUE, MEAN, ERROR, FORECAST_PERIOD)) %>% 
  mutate(REL_ERROR = MEAN/as_vector(rw_error)) 

forecasting_models %>% 
  group_by(MODEL_ID) %>%             
  summarise(MSE = mean(ERROR^2),                                           # calculate mean squared error (MSE)
            RMSE = sqrt(mean(ERROR^2)),                                    # calculate root mean squared error (RMSE)
            MRAE = mean(abs(REL_ERROR)),                                   # calculate Mean Relative Absolute Error (MRAE)
            MdRAE = median(abs(REL_ERROR))) %>%                            # calculate Median Relative Absolute Error (MdRAE)
  ungroup() %>%  
  mutate(RelRMSE = RMSE/(.[.$MODEL_ID=="RW", "RMSE"] %>% pull()),
         AccIMP = (1 - RelRMSE)*100) 

# Diebold-Mariano test for predictive accuracy
dm.test(e1 = as_vector(forecasting_models[forecasting_models$MODEL_ID == "RW", "ERROR"]),
        e2 = as_vector(forecasting_models[forecasting_models$MODEL_ID == "ARIMA", "ERROR"]),
        alternative = "greater",                                         # if p < 0.01 one can reject the H_0 in favor of the the alternative hypothesis which states that method 2 is more accurate than method 1
        h = 4,                                                             # forecasting horizon to produce these forecasts is 1
        power = 2)                                                         # power used in the loss function is 2 (MSE)

dm.test(e1 = as_vector(forecasting_models[forecasting_models$MODEL_ID == "RW", "ERROR"]),
        e2 = as_vector(forecasting_models[forecasting_models$MODEL_ID == "RF", "ERROR"]),
        alternative = "greater",
        h = 1,
        power = 2)

dm.test(e1 = as_vector(forecasting_models[forecasting_models$MODEL_ID == "RW", "ERROR"]),
        e2 = as_vector(forecasting_models[forecasting_models$MODEL_ID == "GB", "ERROR"]),
        alternative = "greater",
        h = 1,
        power = 2)

dm.test(e1 = as_vector(forecasting_models[forecasting_models$MODEL_ID == "RW", "ERROR"]),
        e2 = as_vector(forecasting_models[forecasting_models$MODEL_ID == "SVR", "ERROR"]),
        alternative = "greater",
        h = 1,
        power = 2)

# Visualization
tidy_yx_yq_stat %>% 
  as_tsibble(index = DATE_QUARTER) %>% 
  filter_index(yearquarter("2007 Q2") ~ .) %>% 
  select(DATE_QUARTER, REAL_GDP_GROWTH) %>% 
  mutate(
    RW = NA,
    ARIMA = NA,
    #VAR = NA,
    RF = NA, 
    GB = NA,
    SVR = NA
  ) %>% 
  melt(id = c("DATE_QUARTER", "REAL_GDP_GROWTH"), variable.name = "MODEL_ID") %>% 
  as_tibble() %>% 
  mutate(MODEL_ID = as.character(MODEL_ID)) %>% 
  select(-value) %>% 
  left_join(forecasting_models, by = c("DATE_QUARTER" = "FORECAST_PERIOD", "MODEL_ID")) %>% 
  #select(MODEL_ID, DATE_QUARTER, REAL_GDP_GROWTH, MEAN, LOWER, UPPER) %>%
  select(MODEL_ID, DATE_QUARTER, REAL_GDP_GROWTH, MEAN) %>%
  mutate(DATE_QUARTER = yearquarter(DATE_QUARTER)) %>% 
  ggplot() +
  geom_line(aes(x = DATE_QUARTER, y = REAL_GDP_GROWTH), linetype = 5) +
  geom_line(aes(x = DATE_QUARTER, y = MEAN), color = "#E7B800") +
  #geom_ribbon(aes(x = DATE_QUARTER, ymin = LOWER, ymax = UPPER), fill = "blue", alpha = .25) +
  facet_wrap(~ MODEL_ID, dir = "v") +
  geom_rect(data = recessions %>% filter(row.names(.)==11),
            aes(xmin = start_recession,
                xmax = end_recession,
                ymin = -Inf,
                ymax = Inf),
            fill = "grey",
            alpha = 0.3) +
  xlab("Date") +
  ylab("Real GDP growth (quarterly) in %") +
  theme_thesis


# 1Y_LI -------------------------------------------------------------------
# Read data
load(file = file.path(getwd(), "Results", "Backup", "ZEW", "1Y_LI", "forecasting_models.RData"))
load(file = file.path(getwd(), "Results", "Backup", "ZEW", "1Y_LI", "performance_benchmark_rf.RData"))
load(file = file.path(getwd(), "Results", "Backup", "ZEW", "1Y_LI", "performance_benchmark_gb.RData"))
load(file = file.path(getwd(), "Results", "Backup", "ZEW", "1Y_LI", "performance_benchmark_sv.RData"))


# Define RW errors
rel_error <- forecasting_models %>% 
  filter(MODEL_ID == "VAR") %>% 
  select(MEAN)

# Calculate error measures
forecasting_models <- performance_benchmark_rf$results$gdp_forecast$ranger$pred$data %>% 
  as_tibble() %>% 
  filter(set == "test") %>% 
  mutate(MODEL_ID = "RF") %>% 
  bind_rows(performance_benchmark_gb$results$gdp_forecast$xgboost$pred$data %>% 
              as_tibble() %>% 
              filter(set == "test") %>% 
              mutate(MODEL_ID = "GB")) %>% 
  bind_rows(performance_benchmark_sv$results$gdp_forecast$svm$pred$data %>% 
              as_tibble() %>% 
              filter(set == "test") %>% 
              mutate(MODEL_ID = "SVR")) %>% 
  mutate(ERROR = truth - response) %>%
  rename(TRUE_VALUE=truth, MEAN = response) %>%
  mutate(FORECAST_PERIOD = rep(unique(forecasting_models$FORECAST_PERIOD), 3)) %>% 
  select(MODEL_ID, TRUE_VALUE, MEAN, ERROR, FORECAST_PERIOD) %>% 
  bind_rows(forecasting_models %>% 
              select(MODEL_ID, TRUE_VALUE, MEAN, ERROR, FORECAST_PERIOD)) %>% 
  mutate(REL_ERROR = MEAN/as_vector(rel_error)) 

forecasting_models %>% 
  group_by(MODEL_ID) %>%             
  summarise(MSE = mean(ERROR^2),                                           # calculate mean squared error (MSE)
            RMSE = sqrt(mean(ERROR^2)),                                    # calculate root mean squared error (RMSE)
            MRAE = mean(abs(REL_ERROR)),                                   # calculate Mean Relative Absolute Error (MRAE)
            MdRAE = median(abs(REL_ERROR))) %>%                            # calculate Median Relative Absolute Error (MdRAE)
  ungroup() %>%  
  mutate(RelRMSE = RMSE/(.[.$MODEL_ID=="VAR", "RMSE"] %>% pull()),
         AccIMP = (1 - RelRMSE)*100) 

# Diebold-Mariano test for predictive accuracy
dm.test(e1 = as_vector(forecasting_models[forecasting_models$MODEL_ID == "VAR", "ERROR"]),
        e2 = as_vector(forecasting_models[forecasting_models$MODEL_ID == "ARIMA", "ERROR"]),
        alternative = "greater",                                         # if p < 0.01 one can reject the H_0 in favor of the the alternative hypothesis which states that method 2 is more accurate than method 1
        h = 4,                                                             # forecasting horizon to produce these forecasts is 1
        power = 2)                                                         # power used in the loss function is 2 (MSE)

dm.test(e1 = as_vector(forecasting_models[forecasting_models$MODEL_ID == "VAR", "ERROR"]),
        e2 = as_vector(forecasting_models[forecasting_models$MODEL_ID == "RF", "ERROR"]),
        alternative = "greater",
        h = 1,
        power = 2)

dm.test(e1 = as_vector(forecasting_models[forecasting_models$MODEL_ID == "VAR", "ERROR"]),
        e2 = as_vector(forecasting_models[forecasting_models$MODEL_ID == "GB", "ERROR"]),
        alternative = "greater",
        h = 1,
        power = 2)

dm.test(e1 = as_vector(forecasting_models[forecasting_models$MODEL_ID == "VAR", "ERROR"]),
        e2 = as_vector(forecasting_models[forecasting_models$MODEL_ID == "SVR", "ERROR"]),
        alternative = "greater",
        h = 1,
        power = 2)

# Visualization
tidy_yx_yq_stat %>% 
  as_tsibble(index = DATE_QUARTER) %>% 
  filter_index(yearquarter("2007 Q2") ~ .) %>% 
  select(DATE_QUARTER, REAL_GDP_GROWTH) %>% 
  mutate(
    RW = NA,
    ARIMA = NA,
    VAR = NA,
    RF = NA, 
    GB = NA,
    SVR = NA
  ) %>% 
  melt(id = c("DATE_QUARTER", "REAL_GDP_GROWTH"), variable.name = "MODEL_ID") %>% 
  as_tibble() %>% 
  mutate(MODEL_ID = as.character(MODEL_ID)) %>% 
  select(-value) %>% 
  left_join(forecasting_models, by = c("DATE_QUARTER" = "FORECAST_PERIOD", "MODEL_ID")) %>% 
  #select(MODEL_ID, DATE_QUARTER, REAL_GDP_GROWTH, MEAN, LOWER, UPPER) %>%
  select(MODEL_ID, DATE_QUARTER, REAL_GDP_GROWTH, MEAN) %>%
  mutate(DATE_QUARTER = yearquarter(DATE_QUARTER)) %>% 
  ggplot() +
  geom_line(aes(x = DATE_QUARTER, y = REAL_GDP_GROWTH), linetype = 5) +
  geom_line(aes(x = DATE_QUARTER, y = MEAN), color = "#E7B800") +
  #geom_ribbon(aes(x = DATE_QUARTER, ymin = LOWER, ymax = UPPER), fill = "blue", alpha = .25) +
  facet_wrap(~ MODEL_ID, dir = "v") +
  geom_rect(data = recessions %>% filter(row.names(.)==11),
            aes(xmin = start_recession,
                xmax = end_recession,
                ymin = -Inf,
                ymax = Inf),
            fill = "grey",
            alpha = 0.3) +
  xlab("Date") +
  ylab("Real GDP growth (quarterly) in %") +
  theme_thesis
