# ARMA
# Steering ----------------------------------------------------------------
setwd("J:\\Studium\\Master\\Masterthesis")

ic <- "aicc"   # select information criteria used for order selection ("aic", "bic" or "aicc")

# Sourcing ----------------------------------------------------------------
source(file = file.path(getwd(), "Code", "src_setup.R"))

# Read data ---------------------------------------------------------------
load(file = file.path(getwd(), "Results", "tidy_data", "tidy_yx_yq_stat.RData"))

# Select target variable gdp growth
tidy_y_yq_stat <- tidy_yx_yq_stat %>% 
  select(DATE_QUARTER, REAL_GDP_GROWTH)










# Examination autocorrelation ---------------------------------------------
# ACF and PACF diagnostic plots
plot_acf <- tidy_y_yq_stat %>% 
  select(REAL_GDP_GROWTH) %>% 
  ggAcf(main = "", lag.max = 40) +
  theme_thesis


plot_pcf <- tidy_y_yq_stat %>% 
  select(REAL_GDP_GROWTH) %>%
  ggPacf(main = "", lag.max = 40) +
  theme_thesis

grid.arrange(plot_acf, plot_pcf)


# Model selection ---------------------------------------------------------

# Information criteria
aicc_arima <- tidy_y_yq_stat %>% 
  select(REAL_GDP_GROWTH) %>%
  auto.arima(stepwise = FALSE,                                             # serch over all possible models
             approximation = FALSE,                                        # no approximation, consider full data 
             trace = TRUE,                                                 # print results
             ic = ic,                                                      # information criterion for selecting model order
             max.p = 5,                                                    # maximum number of AR terms 
             max.q = 5,                                                    # maximum number of MA terms
             max.order = 10,                                               # maximim order of lags in model (p + q)
             nmodels = 50,                                                 # maximum number of different models to consider
             stationary = TRUE,                                            # data is stationary (done in data preparation step), so no differencing
             allowmean = TRUE,                                             # models with a non-zero mean are considered
             lambda = NULL                                                 # no box-cox transformation of data (data is transformed in the data preparation step prior to modelling)
             )

summary(aicc_arima)



# Model estimation --------------------------------------------------------
model_AR <- tidy_y_yq_stat %>% 
  select(REAL_GDP_GROWTH) %>% 
  Arima(order = c(2, 0, 0), include.mean = TRUE, method = "CSS") 


# Compare:
tidy_y_yq_stat %>% 
  select(REAL_GDP_GROWTH) %>%
  arima(order = c(2, 0, 0), include.mean = TRUE, method = "CSS") 

# Compare:
tidy_y_yq_stat %>% 
  select(REAL_GDP_GROWTH) %>%
  ar.ols(order.max = 2, demean = TRUE, intercept = TRUE)

# Compare:
AR_test <- lm(formula = REAL_GDP_GROWTH ~ lag(REAL_GDP_GROWTH, n = 1) + lag(REAL_GDP_GROWTH, n = 2), 
              data = tidy_y_yq_stat)
# Note:
AR_test$coefficients[1]/(1-model_AR$coef[1])

# Compare:
AR2 <- dynlm(formula = REAL_GDP_GROWTH ~ L(REAL_GDP_GROWTH, 1) + L(REAL_GDP_GROWTH, 2),
      start = c(1959, 3),
      end = c(2019, 2))

coeftest(AR2)


# Analysis residuals ------------------------------------------------------
# Residual plots
model_AR %>% 
  residuals() %>% 
  as_tibble() %>% 
  rename(RESIDUALS = x) %>% 
  ggplot(aes(x = RESIDUALS)) +
  geom_histogram(aes(y = ..density..), fill = bb_blue_medium, color = "white") +
  geom_density() +
  theme_thesis

# Ljung-Box Test
model_AR %>% 
  residuals() %>% 
  as_tibble() %>% 
  rename(RESIDUALS = x) %>% 
  Box.test(lag = 2, type = "Ljung-Box")  # cannot reject the H_0 that residuals from AR(1) process are White Noise


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
