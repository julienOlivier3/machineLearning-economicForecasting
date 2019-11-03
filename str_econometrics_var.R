# VAR
# Steering ----------------------------------------------------------------
setwd("J:\\Studium\\Master\\Masterthesis")

# Training test split
#training_start <- "1987 Q2"       # Set the first quarter of training data
training_end <- "2007 Q1"         # Set the last quarter of training data, the subsequent quarter is the first quarter to be forecasted (Note: Go to "2007 Q1" once the latest observation is "2019 Q2")


# VAR
VAR_variables <- c("REAL_GDP_GROWTH",
                   "HOUST",
                   "AMDMN_OX",
                   "S_P_500",
                   "UMCSEN_TX",
                   "AWHMAN",
                   "CPF3MTB3MX")
ic <- "bic"                       # select information criteria used for order selection ("aic", "bic" or "aicc")
max_p <- 10                       # select maximum number of AR terms 
lag_max <- 10                     # maximum number of lags considered in ACF plots

final_p <- c(0)                   # define AR parameters of final model (integer is here possibly enough, the one needs to redefine code below: no more length(final_p))

                


# Sourcing ----------------------------------------------------------------
source(file = file.path(getwd(), "Code", "src_setup.R"))

# Read data ---------------------------------------------------------------
load(file = file.path(getwd(), "Results", "tidy_data", "tidy_yx_yq_stat.RData"))



# Filter data -------------------------------------------------------------

# Select target variable gdp growth
tidy_y_yq_stat <- tidy_yx_yq_stat %>% 
  select(DATE_QUARTER, VAR_variables)


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




# Model selection ---------------------------------------------------------
# Plot non-stationary time series
tidy_yx_yq %>% 
  as_tibble() %>% 
  select(DATE_QUARTER, VAR_variables) %>% 
  melt(id.vars = "DATE_QUARTER", variable.name = "SERIES") %>% 
  as_tibble() %>% 
  ggplot() +
  geom_line(aes(x = DATE_QUARTER, y = value, color = SERIES, linetype = SERIES)) +
  scale_color_manual(values = function_gradient_blue(7)) +
  scale_y_log10()+
  theme_thesis 

# Plot non-stationary time series
tidy_yx_yq_stat %>% 
  as_tibble() %>% 
  select(DATE_QUARTER, VAR_variables) %>% 
  melt(id.vars = "DATE_QUARTER", variable.name = "SERIES") %>% 
  as_tibble() %>% 
  ggplot() +
  geom_line(aes(x = DATE_QUARTER, y = value, color = SERIES, linetype = SERIES)) +
  scale_color_manual(values = function_gradient_blue(7)) +
  theme_thesis 



data_training %>%   
  select(VAR_variables) %>% 
  VARselect(type = "const", lag.max = 10)
# According to Schwarz-Criterion (BIC) one lags are preferred. But compare with other information criteria!



# Model estimation --------------------------------------------------------

model_VAR <- data_training %>% 
  select(VAR_variables) %>% 
  VAR(p = length(final_p), type = "const")

summary(model_VAR$varresult$REAL_GDP_GROWTH)

# # Compare:
# model_ADL <- data_training %>% 
#   #select(DATE_QUARTER, REAL_GDP_GROWTH, FEDFUNDS_STAT, UNRATE_STAT, CPIAUCSL_STAT) %>% 
#   ts(start = c(lubridate::year(.$DATE_QUARTER[1]), lubridate::quarter(.$DATE_QUARTER[1])),
#      end = c(lubridate::year(.$DATE_QUARTER[nrow(.)]), lubridate::quarter(.$DATE_QUARTER[nrow(.)])),
#      frequency = 4) %>% 
#   dynlm(formula = REAL_GDP_GROWTH ~ L(REAL_GDP_GROWTH, 1:length(final_p)) + L(HOUST, 1:length(final_p)) +
#           L(AMDMN_OX, 1:length(final_p)) + L(S_P_500, 1:length(final_p)) + 
#           L(UMCSEN_TX, 1:length(final_p)) + L(AWHMAN, 1:length(final_p)) +
#           L(CPF3MTB3MX, 1:length(final_p))) 
# summary(model_ADL)



# Continue here:
# Granger causality -------------------------------------------------------
# Tests whether independent variables granger cause dependent variable (REAL_GDP_GROWTH).
# Basically this is a F-Test comparing explanatory power of an unrestricted model with all variables and the smaller model
# without the independent variable whose "causality" should be tested.
# Multivariate granger causality test
granger_VAR <- data_training %>% 
  select(VAR_variables) %>%
  colnames() %>% 
  enframe(name = NULL, value = "SERIES") %>% 
  mutate(GRANGER = map(SERIES, function(x) causality(model_VAR, cause = x, vcov. = vcovHC(model_VAR))$Granger),
         P_VALUE = map(GRANGER, ~ .$p.value[1,1])) %>% 
  unnest(P_VALUE)

# dropping insignificant variables?
granger_VAR$GRANGER

model_VAR %>% 
  causality(cause = VAR_variables[2:length(VAR_variables)], vcov. = vcovHC(.)) %>% 
  .$Granger


# # Check (compare both results): 
# temp <- data_VAR %>% 
#   select(DATE_QUARTER, REAL_GDP_GROWTH, FEDFUNDS_STAT, UNRATE_STAT, CPIAUCSL_STAT) %>% 
#   ts(start = c(year(.$DATE_QUARTER[1]), quarter(.$DATE_QUARTER[1])),
#      end = c(year(.$DATE_QUARTER[nrow(.)]), quarter(.$DATE_QUARTER[nrow(.)])),
#      frequency = 4) %>% 
#   dynlm(formula = REAL_GDP_GROWTH ~ L(REAL_GDP_GROWTH, 1:2) + L(FEDFUNDS_STAT, 1:2))
# 
# 
# temp %>% 
#   linearHypothesis(c("L(FEDFUNDS_STAT, 1:2)1 = 0", "L(FEDFUNDS_STAT, 1:2)2 = 0")) 
# 
# grangertest(data_VAR$FEDFUNDS_STAT, y = data_VAR$REAL_GDP_GROWTH, order = 2)
#
# # Granger causality compares both models and tests whether increase 
# # in explanatory power of extended model is statistically significant
# # based of F-statistic

# "Univariate" granger causality test
test_granger <- vector(mode = "list", length = length(VAR_variables))
for (i in 1:length(test_granger)){
  test_granger[[i]]$EXCLUDED_VARIABLES <- names(model_ADL$coefficients)[(2*i):(2*i+1)] 
  
  test_granger[[i]]$P_VALUE <- linearHypothesis(model_ADL, 
                                                hypothesis.matrix = paste(names(model_ADL$coefficients)[(2*i):(2*i+1)], "= 0"),
                                                vcov. = vcovHC(model_ADL))$`Pr(>F)`[2]
  
  
}
test_granger


# Cointegration test ------------------------------------------------------
# Johansen test
data_training %>% 
  select(VAR_variables) %>%
  ca.jo(type = "trace", ecdet = "none", K = 2, spec = "longrun") %>% 
  summary()
# Test suggest that there are 3 cointegarted vectors (test statistic of r<=3 is greater than critical value at 1pct (36.08 > 11.65))
# At this stage H_0 is that there are 2 or less cointegrating vectors which by the above result can be rejected.
# However, since we are interested in modelling percentage change time series (which are all stationary) and not level time series
# which are I(1), it is sufficient to choose a VAR model instead a VECM model (further reading possibly required, e.g. Hamilton).


# Forecasting -------------------------------------------------------------


#--------------------------------------------------------------------------
# Miscellaneous -----------------------------------------------------------
## Stationarity testing ===================================================
# Plot time series of VAR variables
data_VAR %>% 
  select(DATE_QUARTER, REAL_GDP_GROWTH, FEDFUNDS_STAT, UNRATE_STAT, CPIAUCSL_STAT) %>% 
  melt(id.vars = "DATE_QUARTER", variable.name = "SERIES") %>% 
  as_tibble() %>% 
  ggplot() +
  geom_line(aes(x = DATE_QUARTER, y = value, color = SERIES), size = 1) +
  scale_color_viridis(discrete = TRUE) +
  theme_thesis
# Time series seem to move around a constant mean value (first hint for stationarity)

# Dickey Fuller stochastic trend
data_VAR %>% 
  select(-DATE_QUARTER) %>% 
  melt(variable.name = "SERIES") %>% 
  as_tibble() %>% 
  group_by(SERIES) %>% 
  nest(.key = DATA) %>% 
  mutate(DF = map(DATA, ~ ur.df(as_vector(.), lags = 1, type = "drift")),
         T_VALUE = map(DF, ~ .@teststat[1,1]),  
         C_VALUE1 = map(DF, ~ .@cval[1,1]),
         C_VALUE5 = map(DF, ~ .@cval[1,2]),
         C_VALUE10 = map(DF, ~ .@cval[1,3])) %>% 
  select(SERIES, T_VALUE, C_VALUE1, C_VALUE5, C_VALUE10) %>% 
  unnest() %>% 
  rowwise() %>% 
  mutate(P_VALUE = case_when(T_VALUE < C_VALUE1 ~ "<0.01",
                             T_VALUE > C_VALUE1 & T_VALUE < C_VALUE5 ~ "<0.05",
                             T_VALUE > C_VALUE5 & T_VALUE < C_VALUE10 ~ "<0.1",
                             T_VALUE > C_VALUE10 ~ "non-stationary"))
# can reject the H_0 for all variables which enter into VAR model that process is non-stationary due to unit root


# Dickey Fuller deterministic trend
data_VAR %>% 
  select(-DATE_QUARTER) %>% 
  melt(variable.name = "SERIES") %>% 
  as_tibble() %>% 
  group_by(SERIES) %>% 
  nest(.key = DATA) %>% 
  mutate(DF = map(DATA, ~ ur.df(as_vector(.), lags = 1, type = "trend")),
         T_VALUE = map(DF, ~ .@teststat[1,1]),  
         C_VALUE1 = map(DF, ~ .@cval[1,1]),
         C_VALUE5 = map(DF, ~ .@cval[1,2]),
         C_VALUE10 = map(DF, ~ .@cval[1,3])) %>% 
  select(SERIES, T_VALUE, C_VALUE1, C_VALUE5, C_VALUE10) %>% 
  unnest() %>% 
  rowwise() %>% 
  mutate(P_VALUE = case_when(T_VALUE < C_VALUE1 ~ "<0.01",
                             T_VALUE > C_VALUE1 & T_VALUE < C_VALUE5 ~ "<0.05",
                             T_VALUE > C_VALUE5 & T_VALUE < C_VALUE10 ~ "<0.1",
                             T_VALUE > C_VALUE10 ~ "non-stationary"))
# can reject the H_0 for all variables which enter into VAR model that process is non-stationary due to time trend


# KPSS Test stochastic trend
data_VAR %>% 
  select(-DATE_QUARTER) %>% 
  melt(variable.name = "SERIES") %>% 
  as_tibble() %>% 
  group_by(SERIES) %>% 
  nest(.key = DATA) %>% 
  mutate(KPSS = map(DATA, ~ kpss.test(as_vector(.), null = "Level")),
         P_VALUE = map(KPSS, ~ .$p.value)) %>% 
  select(SERIES, P_VALUE) %>% 
  unnest()
# cannot reject the H_0 for all variables which enter into VAR model that process is stationary due to unit root

# KPSS Test deterministic trend
data_VAR %>% 
  select(-DATE_QUARTER) %>% 
  melt(variable.name = "SERIES") %>% 
  as_tibble() %>% 
  group_by(SERIES) %>% 
  nest(.key = DATA) %>% 
  mutate(KPSS = map(DATA, ~ kpss.test(as_vector(.), null = "Trend")),
         P_VALUE = map(KPSS, ~ .$p.value)) %>% 
  select(SERIES, P_VALUE) %>% 
  unnest()
# cannot reject the H_0 for all variables which enter into VAR model that process is stationary due to time trend

