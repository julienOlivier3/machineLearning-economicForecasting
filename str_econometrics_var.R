# VAR
# Steering ----------------------------------------------------------------
setwd("J:\\Studium\\Master\\Masterthesis")

# Training test split
#training_start <- "1987 Q2"       # Set the first quarter of training data
training_end <- "2007 Q1"         # Set the last quarter of training data, the subsequent quarter is the first quarter to be forecasted (Note: Go to "2007 Q1" once the latest observation is "2019 Q2")


# VAR
VAR_variables <- c("REAL_GDP_GROWTH",
                   "HOUST",           # Housing Starts: Total: New Privately Owned Housing Units Started (Thousands of Units)
                   "AMDMN_OX",        # Real Manufacturers' New Orders: Durable Goods (Millions of 2012 Dollars), deflated by Core PCE
                   #"S_P_500",         # S&P's Common Stock Price Index: Composite
                   "UMCSEN_TX",       # University of Michigan: Consumer Sentiment (Index 1st Quarter 1966=100)
                   "AWHMAN",          # Average Weekly Hours of Production and Nonsupervisory Employees: Manufacturing (Hours)
                   "T5YFFM")        # 3-month treasury constant maturity rate and federal funds rate

# VAR_variables <- c("CPIAUCSL",
#                    "FEDFUNDS",
#                    "REAL_GDP_GROWTH")

ic <- "bic"                       # select information criteria used for order selection ("aic", "bic" or "aicc")
max_p <- 10                       # select maximum number of AR terms 
lag_max <- 10                     # maximum number of lags considered in ACF plots

final_p <- 1                      # define AR parameters of final model

                


# Sourcing ----------------------------------------------------------------
source(file = file.path(getwd(), "Code", "src_setup.R"))

# Read data ---------------------------------------------------------------
load(file = file.path(getwd(), "Results", "tidy_data", "tidy_yx_yq_stat.RData"))
load(file = file.path(getwd(), "Results", "tidy_data", "tidy_yx_yq.RData"))


# Filter data -------------------------------------------------------------

# Select target variable gdp growth and further variables considered in the VAR model
tidy_yx_yq_stat <- tidy_yx_yq_stat %>% 
  select(DATE_QUARTER, VAR_variables)


# Change data type of last observation of training data
training_end <- training_end %>% 
  yearquarter()

# Training data
data_training <- tidy_yx_yq_stat %>% 
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
  scale_color_viridis(discrete = TRUE) +
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
  scale_color_viridis(discrete = TRUE) +
  theme_thesis 



data_training %>%   
  select(VAR_variables) %>% 
  VARselect(type = "const", lag.max = 10)
# According to Schwarz-Criterion (BIC) one lags are preferred. But compare with other information criteria!



# Model estimation --------------------------------------------------------

model_VAR <- data_training %>% 
  select(VAR_variables) %>% 
  VAR(p = final_p, type = "const")

summary(model_VAR$varresult$REAL_GDP_GROWTH)

# # Compare:
# model_ADL <- data_training %>% 
#   #select(DATE_QUARTER, REAL_GDP_GROWTH, FEDFUNDS_STAT, UNRATE_STAT, CPIAUCSL_STAT) %>% 
#   ts(start = c(lubridate::year(.$DATE_QUARTER[1]), lubridate::quarter(.$DATE_QUARTER[1])),
#      end = c(lubridate::year(.$DATE_QUARTER[nrow(.)]), lubridate::quarter(.$DATE_QUARTER[nrow(.)])),
#      frequency = 4) %>% 
#   dynlm(formula = REAL_GDP_GROWTH ~ L(REAL_GDP_GROWTH, 1:final_p) + L(HOUST, 1:final_p) +
#           L(AMDMN_OX, 1:final_p) + L(S_P_500, 1:final_p) + 
#           L(UMCSEN_TX, 1:final_p) + L(AWHMAN, 1:final_p) +
#           L(CPF3MTB3MX, 1:final_p)) 
# summary(model_ADL)



# Granger causality -------------------------------------------------------
# Tests whether independent variables granger cause dependent variable (REAL_GDP_GROWTH).
# Basically this is a F-Test comparing explanatory power of an unrestricted model with all variables (Y & X) and the smaller model
# without the variable whose "causality" should be tested.
# In its core the Granger causality test is a F-Test with H_0: R^2 of the unrestricted model including the lagged values 
# of Y and X is equal to the R^2 of the smaller model with only lagged values of Y. The ratio of both R^2 builds the F-statistic.
# For large values of the F-statitistic H_0 (no "granger causality") can be rejected.



## Joint Granger causality ================================================

# Joint granger causality test (causality of one variable on all other variables)
granger_VAR_joint1 <- data_training %>% 
  select(VAR_variables) %>%
  colnames() %>% 
  enframe(name = NULL, value = "SERIES") %>% 
  mutate(GRANGER = map(SERIES, function(x) causality(model_VAR, cause = x, vcov. = vcovHC(model_VAR))$Granger),
         P_VALUE = map(GRANGER, ~ .$p.value[1,1])) %>% 
  unnest(P_VALUE)

#granger_VAR_joint1


# Joint granger causality test (causality of all variables on one variables)
granger_VAR_joint2 <- data_training %>% 
  select(VAR_variables) %>%
  colnames() %>% 
  enframe(name = NULL, value = "SERIES") %>% 
  mutate(GRANGER = map(SERIES, function(x) causality(model_VAR, cause = VAR_variables[!(VAR_variables == x)], vcov. = vcovHC(model_VAR))$Granger),
         P_VALUE = map(GRANGER, ~ .$p.value[1,1])) %>% 
  unnest(P_VALUE)

granger_VAR_joint2 %>% 
  mutate(P_VALUE=format(P_VALUE, scientific=FALSE))






# Granger causality of all features on traget variable
model_VAR %>% 
  causality(cause = VAR_variables[!str_detect(string = VAR_variables, pattern = "REAL_GDP_GROWTH")], vcov. = vcovHC(.)) %>% 
  .$Granger


## Individual Granger causality ===========================================

 
# Test whether the inclusion of the respective variable improves
# the minimal model with only lagged values of the target (AR model)
granger_VAR_ind <- tibble(.rows = ncol(data_training)) %>%                 # create tibble with number of rows being equal to the number of columns in dataframe input
  mutate(VARIABLE = colnames(data_training)) %>%                           # create string column with column names as input
  mutate(SERIES = map(data_training, ~ (c(t(.))))) %>%                     # create column with nested time series of respective variable
  filter(VARIABLE != "DATE_QUARTER") %>% 
  filter(VARIABLE != "REAL_GDP_GROWTH") %>% 
  mutate(GRANGER = map(SERIES, function(v) grangertest(x = data_training$REAL_GDP_GROWTH, y = v, order = final_p)),
         P_VALUE = map(GRANGER, ~ .$`Pr(>F)`[2])
         ) %>% 
  unnest(P_VALUE)

granger_VAR_ind


# Test whether the inclusion of the respective variable improves
# the full model including all variables
#function_granger_stepwise(df = data_training, lags = final_p)




# Analysis residuals ------------------------------------------------------
## Complete VAR model =====================================================
# Define residuals tibbel
residual <- model_VAR %>% 
  residuals() %>%                                                          # extract residuals from model
  as_tibble() %>% 
  bind_cols(data_training %>%                                              # bind the respective date to the residuals (required for time series plot of residuals)
              select(DATE_QUARTER) %>% 
              filter(!(row.names(.) %in% 1:final_p))) %>%                  # drop the residuals which are meaningless (=0) according to the number of AR components in the model   
  select(DATE_QUARTER, everything())

# Residual ACF (compress plots into one figure here)
for (v in VAR_variables){
  print(residual %>% 
          select(v) %>% 
          ggAcf(main = "", lag.max = lag_max) +                                     # plot empirical ACF
          theme_thesis)
  
}



# Multivate Portmanteau tests to test for serial correlation 
serial.test(model_VAR, 
            lags.pt = 40,
            lags.bg = 50,
            type = "PT.asymptotic")


BoxPierce(model_VAR, lags = seq(10,50,5)) %>%                              # identical to serial.test above
  as_tibble()


# Additional Tests
LjungBox(model_VAR, lags = seq(10,40,5))
Hosking(model_VAR, lags = seq(10,40,5))


## GDP from VAR model =====================================================
# Define GDP residuals tibbel
residual <- residual %>% 
  select(DATE_QUARTER, REAL_GDP_GROWTH) %>% 
  rename(RESIDUALS = REAL_GDP_GROWTH) 


# Residual ACF
residual %>% 
  select(RESIDUALS) %>%                                                     # select residuals
  ggAcf(main = "", lag.max = lag_max) +                                     # plot empirical ACF
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
# cannot reject the H_0 that residuals are independent (White noise) as p > 0.01 or alternatively
# cannot reject the H_0 that autocorrelation in residual series is not statistically different from a zero set

residual %>% 
  select(RESIDUALS) %>% 
  Box.test(lag = 10,
           type = "Box-Pierce",
           fitdf = 0)  
# cannot reject the H_0 that residuals are independent (White noise) as p > 0.01 or alternatively 
# cannot reject the H_0 that autocorrelation in residual series is not statistically different from a zero set




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


## Cointegration test =====================================================
# Johansen test
data_training %>% 
  select(VAR_variables) %>%
  ca.jo(type = "trace", ecdet = "none", K = 2, spec = "longrun") %>% 
  summary()
# Test suggest that there are 3 cointegarted vectors (test statistic of r<=3 is greater than critical value at 1pct (36.08 > 11.65))
# At this stage H_0 is that there are 2 or less cointegrating vectors which by the above result can be rejected.
# However, since we are interested in modelling percentage change time series (which are all stationary) and not level time series
# which are I(1), it is sufficient to choose a VAR model instead a VECM model (further reading possibly required, e.g. Hamilton).

