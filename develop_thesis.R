setwd("J:\\Studium\\Master\\Masterthesis")
#--------------------------------------------------------------------------

# Sourcing ----------------------------------------------------------------
source(file = file.path(getwd(), "Code", "src_setup.R"))

# Read data ---------------------------------------------------------------


raw_gdp <- as_tibble(read.xlsx(file = file.path(getwd(), "Data", "routput_first_second_third.xlsx"), 
                               sheetIndex = 2, 
                               startRow = 4, 
                               header = TRUE)) 

raw_x <- as_tibble(read.delim(file = file.path(getwd(), "Data", "2019-07.txt"),
                              header = TRUE, 
                              sep = ",", 
                              dec = "."))

raw_x_yq <- read_csv(file = file.path(getwd(), "Data", "2019-07_yq.csv"))

trans_info <- raw_x %>% filter(rownames(.) == 1)
trans_info_yq <- raw_x_yq %>% filter(rownames(.) == 2)

# raw_gdp <- read_csv(file = file.path(getwd(), "Data", "gdp_growth_quarterly.csv"),
#                     col_types = list(col_date(), col_double())) %>% 
#   rename(date = DATE, GDP_GROWTH = A191RP1Q027SBEA) %>% 
#   as_tsibble(index = "date")




# Tidy data ---------------------------------------------------------------


# GDP
tidy_gdp <- raw_gdp %>% 
  clean_names(case = "all_caps") %>%                                       # capitalize all variable names
  mutate(DATE_QUARTER = yearquarter(as.yearqtr(format(DATE), "%Y:Q%q")),   # change data types: convert first to lubridate::yearqtr and the to tsibble::yearquarter as the tsibble format is required for later operations
         #DATE = as.Date(DATE_QUARTER, format = "%Y Q%q"),
         FIRST = as.numeric(as.character(FIRST)),
         SECOND = as.numeric(as.character(SECOND))) %>% 
  select(-c(DATE)) %>% 
  select(DATE_QUARTER, everything())

# FRED-MD    
tidy_yx <- raw_x %>%   
  clean_names(case = "all_caps") %>% 
  filter(!(rownames(.) == 1)) %>% 
  mutate(DATE = ceiling_date(as.Date(SASDATE, format = "%m/%d/%Y"),        # change date to date at end of month
                             "month") - days(1),
         DATE_QUARTER = yearquarter(as.yearqtr(DATE))) %>% 
  select(-SASDATE) %>% 
  left_join(tidy_gdp, by = c("DATE_QUARTER")) %>%                          # join gdp data by DATE_QUARTER
  select(DATE, DATE_QUARTER, FIRST, 
         SECOND, THIRD, MOST_RECENT, everything()) #%>% 
  #as_tsibble(index = DATE) 

# Use FRED-MD data for year-quarter dataset
# tidy_yx_yq <- tidy_yx %>% 
#   filter(month(DATE) %in% c(3, 6, 9, 12)) %>%                              # only keep end of quarter dates
#   filter(!(is.na(MOST_RECENT)))                                            # only keep rows with observations for GDP growth

# FRED-QD
tidy_yx_yq <- raw_x_yq %>%   
  clean_names(case = "all_caps") %>% 
  filter(!(rownames(.) %in% c(1, 2))) %>%                                  # drop first two rows containing variable information
  mutate(DATE_QUARTER = yearquarter(as.yearqtr(SASDATE, "%m/%d/%Y"))) %>% 
  select(-SASDATE) %>% 
  left_join(tidy_gdp, by = c("DATE_QUARTER")) %>%                          # join gdp data by DATE_QUARTER
  mutate(DATE_QUARTER = yearquarter(as.yearqtr(DATE_QUARTER, 
                                               "%m/%d/%Y"))) %>%           # define DATE_QUARTER as qtr type again since the join command does not preserve the data type
  mutate(MOST_RECENT_ANNUALIZED = MOST_RECENT,                             # assign annualized quarterly GDP growth 
         MOST_RECENT = MOST_RECENT/4) %>%                                  # calculate quarterly GDP growth
  mutate(GDPC1_GROWTH_ANNUALIZED = c(NA, 4*(diff(log(GDPC1))*100)),        # calculate quarterly GDP growth from GDPC1 manually and annualize it for comparison reasons
         GDPC1_GROWTH = c(NA, (diff(log(GDPC1))*100))) %>%                 # calculate quarterly GDP growth from GDPC1 manually
  select(DATE_QUARTER, FIRST, 
         SECOND, THIRD, MOST_RECENT, everything())

# Save dataset
# save(tidy_yx_yq, file = file.path(getwd(), "Data", "tidy_data", "tidy_yx_yq.RData"))

# Check variable GDPC1
tidy_yx_yq %>% 
  select(DATE_QUARTER, GDPC1, GDPC1_GROWTH, GDPC1_GROWTH_ANNUALIZED,       # select GDP variables only
         FIRST, SECOND, THIRD, MOST_RECENT, MOST_RECENT_ANNUALIZED) %>%    # select actual GDP growth variables
  select(DATE_QUARTER, GDPC1_GROWTH, GDPC1_GROWTH_ANNUALIZED, 
         MOST_RECENT, MOST_RECENT_ANNUALIZED) %>% 
  melt(id.vars = "DATE_QUARTER", variable.name = "GDP_GROWTH") %>% 
  as_tibble() %>% 
  ggplot() +
  geom_line(aes(x = DATE_QUARTER, y = value, 
                color = GDP_GROWTH, linetype = GDP_GROWTH)) +
  scale_color_viridis(discrete = TRUE) +
  theme_thesis
  

# Stationary data ---------------------------------------------------------
tidy_yx_yq_stat <- raw_x_yq %>%   
  clean_names(case = "all_caps") %>% 
  filter(!(rownames(.) %in% c(1, 2))) %>%                                  # drop first two rows containing variable information
  mutate(DATE_QUARTER = yearquarter(as.yearqtr(SASDATE, "%m/%d/%Y"))) %>% 
  select(-SASDATE) %>% 
  function_stationary(data = ., tcode = trans_info_yq,                     # make variables stationary
                      date_index = "DATE_QUARTER", ppt = TRUE) %>% 
  left_join(tidy_gdp, by = c("DATE_QUARTER")) %>%                          # join gdp data by DATE_QUARTER
  mutate(DATE_QUARTER = yearquarter(as.yearqtr(DATE_QUARTER, 
                                               "%m/%d/%Y"))) %>%           # define DATE_QUARTER as qtr type again since the join command does not preserve the data type
  mutate(MOST_RECENT_ANNUALIZED = MOST_RECENT,                             # assign annualized quarterly GDP growth 
         MOST_RECENT = MOST_RECENT/4) %>%                                  # calculate quarterly GDP growth
  # mutate(GDPC1_GROWTH_ANNUALIZED = c(NA, 4*(diff(log(GDPC1))*100)),        # calculate quarterly GDP growth from GDPC1 manually and annualize it for comparison reasons
  #        GDPC1_GROWTH = c(NA, (diff(log(GDPC1))*100))) %>%                 # calculate quarterly GDP growth from GDPC1 manually
  mutate(GDPC1_ANNUALIZED = 4*GDPC1) %>% 
  select(DATE_QUARTER, FIRST, 
         SECOND, THIRD, MOST_RECENT, everything())

### Stationarity preparation ##############################################


# Variables detected as non-stationary by at least one of the tests
variabels_nonstat <- function_stationary_tests(tidy_yx_yq_stat[,-1]) %>% 
  as_tibble() %>% 
  filter(KPSS == FALSE | ADF == FALSE) %>% 
  select(VAR) %>% 
  mutate(VAR = as.character(VAR)) %>% 
  as_vector() %>% 
  str_c()

# Variables detected as non-stationary by both tests
variabels_nonstat2 <- function_stationary_tests(tidy_yx_yq_stat[,-1]) %>% 
  as_tibble() %>% 
  filter(KPSS == FALSE & ADF == FALSE) %>% 
  select(VAR) %>% 
  mutate(VAR = as.character(VAR)) %>% 
  as_vector() %>% 
  str_c()

# Return maximum value of the respective time series
tidy_yx_yq_stat %>% 
  select(variabels_nonstat) %>% 
  summarise_all(max, na.rm = TRUE) %>% 
  select(-c("TLBSNNBBD_IX", "GFDEBT_NX", "HW_IX", "NWP_IX")) %>% 
  select(-c("TCU", "CUMFNS", "REVOLS_LX", "UMCSEN_TX", "DRIWCIL", "IMFS_LX", "TLBSNNC_BX", "AWHMAN")) %>% 
  t


### Check non-stationary variables ########################################
# Visual inspection of variables which have been detected as non-stationary by one of the tests
tidy_yx_yq_stat %>% 
  select(DATE_QUARTER, variabels_nonstat) %>% 
  select(-variabels_nonstat2) %>% 
  #select(-c("TLBSNNBBD_IX")) %>%
  select(-c("GFDEBT_NX")) %>%
  melt(id.vars = "DATE_QUARTER", variable.name = "ESTIMATES") %>% 
  as_tibble() %>% 
  ggplot() +
  geom_line(aes(x = DATE_QUARTER, y = value, color = ESTIMATES), alpha = 0.5) +
  scale_color_viridis(discrete = TRUE) +
  theme_thesis +
  theme(legend.position = "none")

# Define dropping variables
variables_drop1 <- c("TLBSNNBBD_IX",                                       # trending and variance non-stationary
                       "GFDEBT_NX")                                        # trending

# Visual inspection of variables which have been detected as non-stationary by both tests
tidy_yx_yq_stat %>% 
  select(DATE_QUARTER, variabels_nonstat2) %>% 
  #select(DATE_QUARTER, AWHMAN) %>%                                         # trend (tcode: 1)
  #select(DATE_QUARTER, HW_IX) %>%                                          # trend (tcode: 1)
  #select(DATE_QUARTER, CES2000000008X) %>%                                 # not mean-stationary (tcode: 5)
  #select(DATE_QUARTER, CES3000000008X) %>%                                 # not variance-stationary (tcode: 5)
  #select(DATE_QUARTER, GS1TB3MX) %>%                                       # not variance-stationary (tcode: 1)
  select(DATE_QUARTER, NWP_IX) %>%                                          # trend (tcode: 1)
  melt(id.vars = "DATE_QUARTER", variable.name = "ESTIMATES") %>% 
  as_tibble() %>% 
  ggplot() +
  geom_line(aes(x = DATE_QUARTER, y = value, color = ESTIMATES), alpha = 0.5) +
  scale_color_viridis(discrete = TRUE) +
  theme_thesis

# Define dropping variables
variables_drop2 <- variabels_nonstat2


### Check stationary variables ############################################



tidy_yx_yq_stat %>% 
  #select(-variabels_nonstat) %>% 
  #select(-c("NONBORRES","TNWMVBSNNCBBD_IX", 
  #"TNWBSNNBBD_IX", "TOTRESNS")) %>% 
  #select("DATE_QUARTER", "NONBORRES") %>%                                 # i.O.     
  select("DATE_QUARTER", "TNWMVBSNNCBBD_IX") %>%                          # drop
  #select("DATE_QUARTER", "TNWBSNNBBD_IX") %>%                             # drop
  #select("DATE_QUARTER", "TOTRESNS") %>%                                  # i.O.
  melt(id.vars = "DATE_QUARTER", variable.name = "ESTIMATES") %>% 
  as_tibble() %>% 
  ggplot() +
  geom_line(aes(x = DATE_QUARTER, y = value, color = ESTIMATES)) +
  scale_color_viridis(discrete = TRUE) +
  theme_thesis +
  theme(legend.position = "none")


# NONBORRES
trans_info_yq %>% 
  select("NONBORRES")

tidy_yx_yq %>% 
  #select(-variabels_nonstat) %>% 
  #select(-c("NONBORRES","TNWMVBSNNCBBD_IX", "TNWBSNNBBD_IX", "TOTRESNS")) %>% 
  select("DATE_QUARTER", "NONBORRES") %>% 
  #select("DATE_QUARTER", "TNWMVBSNNCBBD_IX") %>% 
  #select("DATE_QUARTER", "TNWBSNNBBD_IX") %>% 
  #select("DATE_QUARTER", "TOTRESNS") %>% 
  melt(id.vars = "DATE_QUARTER", variable.name = "ESTIMATES") %>% 
  as_tibble() %>% 
  ggplot() +
  geom_line(aes(x = DATE_QUARTER, y = value, color = ESTIMATES)) +
  scale_color_viridis(discrete = TRUE) +
  theme_thesis +
  theme(legend.position = "none")


# TNWMVBSNNCBBD_IX
trans_info_yq %>% 
  select("TNWMVBSNNCBBDIx")

tidy_yx_yq %>% 
  #select(-variabels_nonstat) %>% 
  #select(-c("NONBORRES","TNWMVBSNNCBBD_IX", "TNWBSNNBBD_IX", "TOTRESNS")) %>% 
  #select("DATE_QUARTER", "NONBORRES") %>% 
  select("DATE_QUARTER", "TNWMVBSNNCBBD_IX") %>% 
  #select("DATE_QUARTER", "TNWBSNNBBD_IX") %>% 
  #select("DATE_QUARTER", "TOTRESNS") %>% 
  melt(id.vars = "DATE_QUARTER", variable.name = "ESTIMATES") %>% 
  as_tibble() %>% 
  ggplot() +
  geom_line(aes(x = DATE_QUARTER, y = value, color = ESTIMATES)) +
  scale_color_viridis(discrete = TRUE) +
  theme_thesis +
  theme(legend.position = "none")


# TNWBSNNBBD_IX
trans_info_yq %>% 
  select("TNWBSNNBBDIx")

tidy_yx_yq %>% 
  #select(-variabels_nonstat) %>% 
  #select(-c("NONBORRES","TNWMVBSNNCBBD_IX", "TNWBSNNBBD_IX", "TOTRESNS")) %>% 
  #select("DATE_QUARTER", "NONBORRES") %>% 
  #select("DATE_QUARTER", "TNWMVBSNNCBBD_IX") %>% 
  select("DATE_QUARTER", "TNWBSNNBBD_IX") %>% 
  #select("DATE_QUARTER", "TOTRESNS") %>% 
  melt(id.vars = "DATE_QUARTER", variable.name = "ESTIMATES") %>% 
  as_tibble() %>% 
  ggplot() +
  geom_line(aes(x = DATE_QUARTER, y = value, color = ESTIMATES)) +
  scale_color_viridis(discrete = TRUE) +
  theme_thesis +
  theme(legend.position = "none")


# TOTRESNS
trans_info_yq %>% 
  select("TOTRESNS")

tidy_yx_yq %>% 
  #select(-variabels_nonstat) %>% 
  #select(-c("NONBORRES","TNWMVBSNNCBBD_IX", "TNWBSNNBBD_IX", "TOTRESNS")) %>% 
  #select("DATE_QUARTER", "NONBORRES") %>% 
  #select("DATE_QUARTER", "TNWMVBSNNCBBD_IX") %>% 
  #select("DATE_QUARTER", "TNWBSNNBBD_IX") %>% 
  select("DATE_QUARTER", "TOTRESNS") %>% 
  melt(id.vars = "DATE_QUARTER", variable.name = "ESTIMATES") %>% 
  as_tibble() %>% 
  ggplot() +
  geom_line(aes(x = DATE_QUARTER, y = value, color = ESTIMATES)) +
  scale_color_viridis(discrete = TRUE) +
  theme_thesis +
  theme(legend.position = "none")


# Define dropping variables
variables_drop3 <- c("TNWMVBSNNCBBD_IX",                                   # outlier
                     "TNWBSNNBBD_IX")                                      # outlier

# Bind dropping variables
variables_drop <- c(variables_drop1,
                    variables_drop2,
                    variables_drop3)

# Final stationary dataset without dropping variables
tidy_yx_yq_stat <- tidy_yx_yq_stat %>% 
  select(-variables_drop)

# Save dataset
# save(tidy_yx_yq_stat, file = file.path(getwd(), "Data", "tidy_data", "tidy_yx_yq_stat.RData"))

# Econometric Models ------------------------------------------------------

# Time series plot
tidy_gdp %>% 
  melt(id.vars = "DATE_QUARTER", variable.name = "ESTIMATES") %>% 
  as_tibble() %>% 
  ggplot() +
  geom_line(aes(x = DATE_QUARTER, y = value, color = ESTIMATES, linetype = ESTIMATES)) +
  scale_color_manual(values = function_gradient_blue(4)) +
  theme_thesis 

# Seasonal plot
ggseasonplot(ts(tidy_yx_yq$MOST_RECENT, 
                start = as.yearqtr("1965 Q3"), 
                end = as.yearqtr("2019 Q1"), 
                frequency = 4), 
             year.labels = FALSE) +
  labs(title = "GDP growth", subtitle = "Seasonal plot") +
  scale_color_viridis(discrete = TRUE) +
  theme_thesis
  
  

## AR =====================================================================
### Model selection #######################################################

# ACF and PACF diagnostic plots
plot_acf <- tidy_gdp %>% 
  select(THIRD) %>% 
  ggAcf(main = "", lag.max = 40) +
  theme_thesis

tidy_yx_yq %>% 
  select(MOST_RECENT) %>%
  na.omit() %>% 
  ggAcf(main = "", lag.max = 40) +
  theme_thesis



plot_pcf <- tidy_gdp %>% 
  select(THIRD) %>% 
  ggPacf(main = "", lag.max = 40) +
  theme_thesis

tidy_yx_yq %>% 
  select(MOST_RECENT) %>%
  na.omit() %>% 
  ggPacf(main = "", lag.max = 40) +
  theme_thesis

grid.arrange(plot_acf, plot_pcf)

# Information criteria
tidy_gdp %>% 
  select(THIRD) %>% 
  auto.arima(stepwise = FALSE, approximation = FALSE, trace = TRUE, ic = "bic", max.p = 10, max.order = 10, stationary = TRUE)

### Stationarity testing ##################################################

# Dickey Fuller 1
tidy_gdp %>% 
  select(THIRD) %>%
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
temp1 <- tidy_gdp %>% 
  as_tibble() %>% 
  select(THIRD) %>%
  as_vector() %>% 
  ur.df(type = "drift", lags = 0, selectlags = "Fixed")  # can reject the H_0 that process is non-stationary

summary(temp1)

# Compare:
# THIRD <- ts(tidy_gdp$THIRD, start = c(1965, 3), end = c(2019, 1), frequency = 4)
# temp <- dynlm(formula = diff(THIRD) ~ L(THIRD, 1) + diff(L(THIRD, 1)))
# temp3 <- coeftest(temp)
# temp$coefficients[2]/sqrt(diag(vcov(temp)))[2]


# Testing for non-stationarity due to deterministic trend (Y_t = b_0 + d*t + b_1*Y_t-1 + u_t)
temp2 <- tidy_gdp %>% 
  as_tibble() %>% 
  select(THIRD) %>%
  as_vector() %>% 
  ur.df(type = "trend", lags = 0, selectlags = "Fixed")  # can reject the H_0 that process is non-stationary

summary(temp2)

# Compare:
# THIRD <- ts(tidy_gdp$THIRD, start = c(1965, 3), end = c(2019, 1), frequency = 4)
# temp <- dynlm(formula = diff(THIRD) ~ trend(THIRD, scale = FALSE) + L(THIRD, 1) + diff(L(THIRD, 1)))
# temp3 <- coeftest(temp)
# temp$coefficients[3]/sqrt(diag(vcov(temp)))[3]


# KPSS Test
tidy_gdp %>% 
  select(THIRD) %>%
  as_vector() %>% 
  kpss.test(null = "Level")  # cannot reject the H_0 that process is stationary

# # Compare:
# tidy_gdp %>% 
#   features(THIRD, unitroot_kpss)

### Model estimation ######################################################
model_AR <- tidy_gdp %>%
  as_tibble() %>% 
  select(THIRD) %>% 
  Arima(order = c(1, 0, 0), include.mean = TRUE, method = "CSS") 


# Compare:
# tidy_gdp %>%
#   as_tibble() %>% 
#   select(THIRD) %>% 
#   arima(order = c(1, 0, 0), include.mean = TRUE, method = "CSS") 

# Compare:
# tidy_gdp %>% 
#   select(THIRD) %>% 
#   ar.ols(order.max = 1, demean = TRUE, intercept = TRUE)

# Compare:
# AR_test <- lm(formula = THIRD ~ lag(THIRD, n = 1), data = tidy_gdp)
# Note:
# AR_test$coefficients[1]/(1-model_AR$coef[1])

# Compare:
# THIRD <- ts(tidy_gdp$THIRD, start = c(1965, 3), end = c(2019, 1), frequency = 4)
# AR2 <- dynlm(formula = THIRD ~ L(THIRD, 1),
#       start = c(1965, 3),
#       end = c(2019, 1))
# 
# coeftest(AR2)


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
  Box.test(lag = 1, type = "Ljung-Box")  # cannot reject the H_0 that residuals from AR(1) process are White Noise



## VAR ====================================================================
### Model selection #######################################################
tidy_yx_yq %>% 
  as_tibble() %>% 
  select(DATE_QUARTER, MOST_RECENT, FEDFUNDS, UNRATE, CPIAUCSL) %>% 
  melt(id.vars = "DATE_QUARTER", variable.name = "SERIES") %>% 
  as_tibble() %>% 
  ggplot() +
  geom_line(aes(x = DATE_QUARTER, y = value, color = SERIES, linetype = SERIES)) +
  scale_color_manual(values = function_gradient_blue(4)) +
  theme_thesis 

# Check variable transformation
trans_info %>% 
  select(FEDFUNDS, UNRATE, CPIAUCSL)
# FEDFUNDS (Effective Federal Funds Rate): first difference (i.e. change, note it is already a rate)
# UNRATE (Civilian Unemployment Rate):     first difference (i.e. change, note it is already a rate)
# CPIAUCSL (Consumer Price Index for All 
# Urban Consumers: All Items 
# (Index 1982-84 = 100)):                  second difference of logarithm
# MOST_RECENT (Annualized GDP Growth):     first difference of logarithm * 4 (this has already been done)

# Define dataset for VAR estimation according to McCracken 
data_VAR <- tidy_yx_yq %>% 
  select(DATE_QUARTER, MOST_RECENT, FEDFUNDS, UNRATE, CPIAUCSL) %>%          
  mutate(FEDFUNDS_STAT = c(NA, diff(FEDFUNDS)),                         # first difference of interest rate
         UNRATE_STAT = c(NA, diff(UNRATE)),                             # first difference of unemployment rate
         CPIAUCSL_STAT = c(NA, NA, diff(log(CPIAUCSL), 
                                        differences = 2)*100)) %>%      # second difference of log of inflation rate
  na.omit()                                                             # drop years for wich no GDP growth rate exists (balanced panel)


data_VAR %>%   
  select(MOST_RECENT, FEDFUNDS_STAT, UNRATE_STAT, CPIAUCSL_STAT) %>% 
  VARselect(type = "const", lag.max = 10)
# According to Schwarz-Criterion (BIC) two lags are preferred. But compare with other information criteria!

### Stationarity testing ##################################################
# Plot time series of VAR variables
data_VAR %>% 
  select(DATE_QUARTER, MOST_RECENT, FEDFUNDS_STAT, UNRATE_STAT, CPIAUCSL_STAT) %>% 
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


### Model estimation ######################################################




model_VAR <- data_VAR %>% 
  select(MOST_RECENT, FEDFUNDS_STAT, UNRATE_STAT, CPIAUCSL_STAT) %>% 
  VAR(p = 2, type = "const")

summary(model_VAR$varresult$MOST_RECENT)

# Compare:
model_ADL <- data_VAR %>% 
  select(DATE_QUARTER, MOST_RECENT, FEDFUNDS_STAT, UNRATE_STAT, CPIAUCSL_STAT) %>% 
  ts(start = c(year(.$DATE_QUARTER[1]), quarter(.$DATE_QUARTER[1])),
     end = c(year(.$DATE_QUARTER[nrow(.)]), quarter(.$DATE_QUARTER[nrow(.)])),
     frequency = 4) %>% 
  dynlm(formula = MOST_RECENT ~ L(MOST_RECENT, 1:2) + L(FEDFUNDS_STAT, 1:2) + L(UNRATE_STAT, 1:2) + L(CPIAUCSL_STAT, 1:2))
summary(model_ADL)



### Granger causality #####################################################
# Tests whether independent variables (FEDFUNDS_STAT, UNRATE_STAT, CPIAUCSL_STAT) granger cause dependent variable (MOST_RECENT).
# Basically this is a F-Test comparing explanatory power of an unrestricted model with all variables and the smaller model
# without the independent variable whose "causality" should be tested.
# Multivariate granger causality test
data_VAR %>% 
  select(MOST_RECENT, FEDFUNDS_STAT, UNRATE_STAT, CPIAUCSL_STAT) %>%
  colnames() %>% 
  enframe(name = NULL, value = "SERIES") %>% 
  mutate(GRANGER = map(SERIES, function(x) causality(model_VAR, cause = x, vcov. = vcovHC(model_VAR))$Granger),
         P_VALUE = map(GRANGER, ~ .$p.value[1,1])) %>% 
  unnest(P_VALUE)
# drop CPI?

model_VAR %>% 
  causality(cause = c("FEDFUNDS_STAT", "UNRATE_STAT", "CPIAUCSL_STAT"), vcov. = vcovHC(.)) %>% 
  .$Granger


# # Check (compare both results): 
# temp <- data_VAR %>% 
#   select(DATE_QUARTER, MOST_RECENT, FEDFUNDS_STAT, UNRATE_STAT, CPIAUCSL_STAT) %>% 
#   ts(start = c(year(.$DATE_QUARTER[1]), quarter(.$DATE_QUARTER[1])),
#      end = c(year(.$DATE_QUARTER[nrow(.)]), quarter(.$DATE_QUARTER[nrow(.)])),
#      frequency = 4) %>% 
#   dynlm(formula = MOST_RECENT ~ L(MOST_RECENT, 1:2) + L(FEDFUNDS_STAT, 1:2))
# 
# 
# temp %>% 
#   linearHypothesis(c("L(FEDFUNDS_STAT, 1:2)1 = 0", "L(FEDFUNDS_STAT, 1:2)2 = 0")) 
# 
# grangertest(data_VAR$FEDFUNDS_STAT, y = data_VAR$MOST_RECENT, order = 2)
#
# # Granger causality compares both models and tests whether increase 
# # in explanatory power of extended model is statistically significant
# # based of F-statistic

# "Univariate" granger causality test
test_granger <- vector(mode = "list", length = 4)
for (i in 1:length(test_granger)){
  test_granger[[i]]$EXCLUDED_VARIABLES <- names(model_ADL$coefficients)[(2*i):(2*i+1)] 
  
  test_granger[[i]]$P_VALUE <- linearHypothesis(model_ADL, 
                                              hypothesis.matrix = paste(names(model_ADL$coefficients)[(2*i):(2*i+1)], "= 0"),
                                              vcov. = vcovHC(model_ADL))$`Pr(>F)`[2]
  

}
test_granger

### Cointegration test ####################################################
# Johansen test
data_VAR %>% 
  select(MOST_RECENT, FEDFUNDS_STAT, UNRATE_STAT, CPIAUCSL_STAT) %>%
  ca.jo(type = "trace", ecdet = "none", K = 2, spec = "longrun") %>% 
  summary()
# Test suggest that there are 3 cointegarted vectors (test statistic of r<=3 is greater than critical value at 1pct (36.08 > 11.65))
# At this stage H_0 is that there are 2 or less cointegrating vectors which by the above result can be rejected.
# However, since we are interested in modelling percentage change time series (which are all stationary) and not level time series
# which are I(1), it is sufficient to choose a VAR model instead a VECM model (further reading possibly required, e.g. Hamilton).


# Visualizations ----------------------------------------------------------
# Plot all stationary variables as time series
tidy_yx_yq_stat %>% 
  melt(id.vars = "DATE_QUARTER", variable.name = "ESTIMATES") %>% 
  as_tibble() %>% 
  ggplot() +
  geom_line(aes(x = DATE_QUARTER, y = value, color = ESTIMATES, linetype = ESTIMATES)) +
  scale_color_viridis(discrete = TRUE) +
  theme_thesis +
  theme(legend.position = "none")

# Random Forest -----------------------------------------------------------

training_data <- ts_data %>% 
  filter(!is.na(GDP_GROWTH)) %>% 
  filter(date <  yearmonth("201001"))

test_data <- ts_data %>% 
  filter(!is.na(GDP_GROWTH)) %>% 
  filter(date >=  yearmonth("201001"))

# Regression tree
reg_tree <- rpart(GDP_GROWTH ~ ., data = training_data, method = "anova")
rpart.plot(reg_tree)

training_pred <- predict(reg_tree, training_data)
plot(x = training_data$date, y = training_data$GDP_GROWTH, type = "line")
lines(x = training_data$date, y = training_pred, col = "blue")

test_pred <- predict(reg_tree, test_data)
plot(x = test_data$date, y = test_data$GDP_GROWTH, type = "line")
lines(x = test_data$date, y = test_pred, col = "blue")


# Playing around with tsibble ---------------------------------------------

ts_data %>% 
  filter(date >= yearmonth("196001")) %>% 
  index_by(Year = lubridate::year(date)) %>% 
  summarise(mean_RPI = mean(RPI))



# Dealing wiht missing values (Imputation) --------------------------------


# Select 5 Variables:
#   1: RPI: I Real Personal Income as change to prior month in log
#   2: UEMPMEAN: Average Duration of Unemployment (Weeks) as change to prior month
#   3: HOUST: Housing Starts: 
#             Number of privately owned new houses (technically housing units) 
#             on which construction has been started in a given period in log

na_names <- ts_data %>% 
  map_dfr(., function(x) sum(is.na(x))) %>% 
  #select(which(!colSums(.) %in% 0))
  select_if(function(.) . != 0) %>% 
  names()

na_data <- ts_data %>% 
  select(date, na_names)

na_which <- na_data %>% 
  map(., function(x) which(is.na(x)))


ts_data %>% 
  map_dfr(., function(x) sum(is.na(x))) %>% 
  #select(which(!colSums(.) %in% 0))
  select_if(function(.) . != 0) %>% 
  melt(value.name = "missings") %>% 
  arrange(missings) %>% 
  ggplot() +
  geom_bar(aes(x = reorder(variable, missings), y = missings), stat = "identity", fill = "blue") +
  coord_flip() +
  xlab("Variable") +
  ylab("Number of missing values") +
  theme_light()

na_data %>% 
  melt(id.vars = "date", na.rm = FALSE) %>%   
  ggplot() +
  geom_line(aes(x = date, y = value, col = variable)) +
  scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
                labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  annotation_logticks() +
  theme_light()

# Missing values are either missing at the very beginning of the time series or 
# they are missing because they have not been updated yet (very last period and period before
# last period). Imputation makes little sense
  

# Take only dates with complete observations leads to immense loss of data
# Time series decreases by 723 - 324 = 399 rows
ts_data %>% 
  na.omit()


# Impute with last observed value
ts_data %>% 
  na.locf(na.rm = FALSE) %>% 
  na.locf(na.rm = FALSE, fromLast = TRUE)








