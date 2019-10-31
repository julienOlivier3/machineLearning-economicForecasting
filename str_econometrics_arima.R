# ARMA
# Steering ----------------------------------------------------------------
setwd("J:\\Studium\\Master\\Masterthesis")

# Training test split
#training_start <- "1987 Q2"       # Set the first quarter of training data
training_end <- "2007 Q1"         # Set the last quarter of training data, the subsequent quarter is the first quarter to be forecasted (Note: Go to "2007 Q1" once the latest observation is "2019 Q2")


# ARIMA
ic <- "bic"                      # select information criteria used for order selection ("aic", "bic" or "aicc")
max_p <- 10                       # select maximum number of AR terms 
max_q <- 10                       # select maximum number of MA terms
lag_max <- 10                     # maximum number of lags considered in ACF plots
 
final_p <- c(0.2023,  0.1898)     # define AR parameters of final model
#final_p <- c(0.0076,  -0.6564,  0.2239,  0.2098,  -0.0645,  0.1304)
final_q <- 0              # define MA parameters of final model
#final_q <- c(0.2548, 0.9742)                   


# Sourcing ----------------------------------------------------------------
source(file = file.path(getwd(), "Code", "src_setup.R"))

# Read data ---------------------------------------------------------------
load(file = file.path(getwd(), "Results", "tidy_data", "tidy_yx_yq_stat.RData"))



# Filter data -------------------------------------------------------------

# Select target variable gdp growth
tidy_y_yq_stat <- tidy_yx_yq_stat %>% 
  select(DATE_QUARTER, REAL_GDP_GROWTH)


# Change data type of last observation of training data
training_end <- training_end %>% 
  yearquarter()

# Training data
data_training <- tidy_y_yq_stat %>% 
  as_tsibble(index = DATE_QUARTER) %>% 
  filter_index(~training_end) %>% 
  as_tibble()


# Test data
data_test <- tidy_yx_yq_stat %>% 
  as_tsibble(index = DATE_QUARTER) %>% 
  filter_index((training_end+1)~.) %>% 
  as_tibble()








# Examination autocorrelation ---------------------------------------------
# ACF and PACF diagnostic plots
plot_acf <- data_training %>% 
  select(REAL_GDP_GROWTH) %>%                                              # select target variable
  ggAcf(main = "", lag.max = lag_max) +                                    # plot empirical ACF
  geom_segment(aes(x = seq(1,lag_max),                                     # add theoretical ACF based on the parameter estimates from the final model
                   y = ARMAacf(ar = final_p, ma = final_q, lag.max = lag_max, pacf = FALSE)[-1],
                   xend = seq(1,lag_max),
                   yend = 0), 
               color = "red",
               alpha = 0.5) +
  theme_thesis


plot_pcf <- data_training %>%  
  select(REAL_GDP_GROWTH) %>%                                              # select target variable
  ggPacf(main = "", lag.max = lag_max) +                                   # plot empirical ACF
  geom_segment(aes(x = seq(1,lag_max),                                     # add theoretical PACF based on the parameter estimates from the final model
                   y = ARMAacf(ar = final_p, ma = final_q, lag.max = lag_max, pacf = TRUE),
                   xend = seq(1,lag_max),
                   yend = 0), 
               color = "red",
               alpha = 0.5) +
  theme_thesis

grid.arrange(plot_acf, plot_pcf)


# Model selection ---------------------------------------------------------

# Information criteria
aicc_arima <- data_training %>% 
  select(REAL_GDP_GROWTH) %>%
  auto.arima(stepwise = FALSE,                                             # serch over all possible models
             approximation = FALSE,                                        # no approximation, consider full data 
             trace = TRUE,                                                 # print results
             ic = ic,                                                      # information criterion for selecting model order
             max.p = max_p,                                                # maximum number of AR terms 
             max.q = max_q,                                                # maximum number of MA terms
             max.order = max_p + max_q,                                    # maximim order of lags in model (p + q)
             nmodels = 2*max_p*max_q,                                      # maximum number of different models to consider
             stationary = TRUE,                                            # data is stationary (done in data preparation step), so no differencing
             allowmean = TRUE,                                             # models with a non-zero mean are considered
             lambda = NULL                                                 # no box-cox transformation of data (data is transformed in the data preparation step prior to modelling)
             )

summary(aicc_arima)



# Model estimation --------------------------------------------------------
model_AR <- data_training %>% 
  select(REAL_GDP_GROWTH) %>%                                              # select target variable
  Arima(order = c(ifelse(final_p[1]==0, 0,length(final_p)),                # define order of the arima model according to steering input
                  0,                                      
                  ifelse(final_q[1]==0, 0,length(final_q))),  
        include.mean = TRUE,                                               # estimate the mean of the series (note that it is the mean and not the intercept)
        include.drift = FALSE,                                             # no drift: stationary series!
        include.constant = TRUE,                                           # corresponds to include mean
        method = "CSS") 


# # Compare: check
# tidy_y_yq_stat %>% 
#   select(REAL_GDP_GROWTH) %>%
#   arima(order = c(2, 0, 0), include.mean = TRUE, method = "CSS") 
# 
# # Compare:
# tidy_y_yq_stat %>% 
#   select(REAL_GDP_GROWTH) %>%
#   ar.ols(order.max = 2, demean = FALSE, intercept = TRUE)
# 
# # Compare:
# AR_test <- lm(formula = REAL_GDP_GROWTH ~ lag(REAL_GDP_GROWTH, n = 1) + lag(REAL_GDP_GROWTH, n = 2)
#               ,
#               data = tidy_y_yq_stat)
# # Note:
# # See here for a good explanation between mean and intercept in ARIMA modelling
# # http://ftp.uni-bayreuth.de/math/statlib/general/tsa2/Rissues.htm
# AR_test$coefficient[1]
# model_AR$coef[3]*(1-model_AR$coef[1]-model_AR$coef[2])




# Analysis residuals ------------------------------------------------------
# Define residuals tibbel
residual <- model_AR %>% 
  residuals() %>%                                                          # extract residuals from model
  as_tibble() %>%
  rename(RESIDUALS = x) %>% 
  bind_cols(data_training %>% select(DATE_QUARTER)) %>%                    # bind the respective date to the residuals (required for time series plot of residuals)
  filter(!(row.names(.) %in% 1:length(final_p)))                           # drop the residuals which are meaningless (=0) according to the number of AR components in the model
  select(DATE_QUARTER, RESIDUALS)
  

# Residual ACF
residual %>% 
  select(RESIDUALS) %>%                                                     # select residuals
  ggAcf(main = "", lag.max = 30) +                                          # plot empirical ACF
  theme_thesis

# Residual time series
residual %>% 
  ggplot() +
  geom_line(aes(x = DATE_QUARTER, y = RESIDUALS)) +
  theme_thesis +
  xlab("Date") +
  ylab("Residuals")

# Residual distribution
residual %>% 
  ggplot(aes(x = RESIDUALS)) +
  geom_histogram(aes(y = ..density..), fill = bb_blue_medium, color = "white") +
  geom_density() +
  theme_thesis +
  xlab("Residuals") +
  ylab("Density")



# Ljung-Box Test
residual %>% 
  select(RESIDUALS) %>% 
  Box.test(lag = 10,
           type = "Ljung-Box",
           fitdf = 0)  
# cannot reject the H_0 that residuals are independent (White noise) or alternatively as p > 0.1
# cannot reject the H_0 that autocorrelation in residual series is not statistically different from a zero set

residual %>% 
  select(RESIDUALS) %>% 
  Box.test(lag = 10,
           type = "Box-Pierce",
           fitdf = 0)  
# cannot reject the H_0 that residuals are independent (White noise) or alternatively as p > 0.1
# cannot reject the H_0 that autocorrelation in residual series is not statistically different from a zero set


# Forecasting -------------------------------------------------------------


#--------------------------------------------------------------------------
# Miscellaneous -----------------------------------------------------------

## Time series plots ======================================================


tidy_gdp %>% 
  melt(id.vars = "DATE_QUARTER", variable.name = "ESTIMATES") %>% 
  as_tibble() %>% 
  ggplot() +
  geom_line(aes(x = DATE_QUARTER, y = value, color = ESTIMATES, linetype = ESTIMATES)) +
  scale_color_manual(values = function_gradient_blue(4)) +
  theme_thesis 

# Seasonal plot
ggseasonplot(ts(tidy_yx_yq$REAL_GDP_GROWTH, 
                start = as.yearqtr("1965 Q3"), 
                end = as.yearqtr("2019 Q1"), 
                frequency = 4), 
             year.labels = FALSE) +
  labs(title = "GDP growth", subtitle = "Seasonal plot") +
  scale_color_viridis(discrete = TRUE) +
  theme_thesis




## Stationarity testing ===================================================

# Dickey Fuller 1
tidy_y_yq_stat %>% 
  select(REAL_GDP_GROWTH) %>%
  as_vector() %>% 
  adf.test(k = 0)  # can reject the H_0 that process is non-stationary
# Note that for the Dickey Fuller Test the model is reformulated as follows:
# Y_t = b_0 + b_1*Y_t-1 + u_t =>
# ??Y_t = b_0 + (b_1 - 1)*Y_t-1 + u_t 
# if coefficient of Y_t-1 is significantly different from 0, the series has no unit root. 
# or:
# Y_t = b_0 + b_1*Y_t-1 + b_2*Y_t-2 + u_t =>
# ??Y_t = b_0 + (b_1 + b_2 - 1)*Y_t-1 - b_2*??Y_t-1 + u_t
# if coefficient of Y_t-1 is significantly different from 0, the series has no unit root (see Verbeek p. 272).
# etc. for higher lag orders

# Dickey Fuller 2
# Testing for non-stationarity due to unit root/stochastic trend (random walk with drift)
temp1 <- tidy_y_yq_stat %>% 
  select(REAL_GDP_GROWTH) %>% 
  as_vector() %>% 
  ur.df(type = "drift", 
        lags = 0, 
        selectlags = "Fixed")  # can reject the H_0 that process is non-stationary

summary(temp1)

# Compare:
# REAL_GDP_GROWTH <- ts(tidy_y_yq_stat$REAL_GDP_GROWTH, start = c(1959, 3), end = c(2019, 2), frequency = 4)
# temp <- dynlm(formula = diff(REAL_GDP_GROWTH) ~ L(REAL_GDP_GROWTH, 1) + diff(L(REAL_GDP_GROWTH, 1)))
# temp3 <- coeftest(temp)
# temp$coefficients[2]/sqrt(diag(vcov(temp)))[2]


# Testing for non-stationarity due to deterministic trend (Y_t = b_0 + d*t + b_1*Y_t-1 + u_t)
temp2 <- tidy_y_yq_stat %>% 
  select(REAL_GDP_GROWTH) %>%
  as_vector() %>% 
  ur.df(type = "trend", lags = 0, selectlags = "Fixed")  # can reject the H_0 that process is non-stationary

summary(temp2)

# Compare:
# REAL_GDP_GROWTH <- ts(tidy_y_yq_stat$REAL_GDP_GROWTH, start = c(1959, 3), end = c(2019, 2), frequency = 4)
# temp <- dynlm(formula = trend(REAL_GDP_GROWTH) ~ L(REAL_GDP_GROWTH, 1) + diff(L(REAL_GDP_GROWTH, 1)))
# temp3 <- coeftest(temp)
# temp$coefficients[3]/sqrt(diag(vcov(temp)))[3]


# KPSS Test
tidy_y_yq_stat %>% 
  select(REAL_GDP_GROWTH) %>%
  as_vector() %>% 
  kpss.test(null = "Level")  # cannot reject the H_0 that process is stationary

# # Compare:
# tidy_gdp %>% 
#   features(THIRD, unitroot_kpss)
