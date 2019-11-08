# Steering ----------------------------------------------------------------
setwd("J:\\Studium\\Master\\Masterthesis")

# Date of latest GDP data extraction (DDMMYY)
gdp_latest <- "201019"

# Date of latest feature variable data extraction (YYYY-MM)
feature_latest <- "2019-09"


#--------------------------------------------------------------------------

# Sourcing ----------------------------------------------------------------
source(file = file.path(getwd(), "Code", "src_setup.R"))

# Read data ---------------------------------------------------------------


## GDP ====================================================================
# GDP data from Federal Reserve Bank of Phildapelphia
# https://www.bea.gov/national/xls/gdplev.xlsx
raw_gdp <- as_tibble(read.xlsx(file = file.path(getwd(), "Data", "GDP", paste0("gdplev_", gdp_latest, ".xlsx")), 
                               sheetIndex = 1, 
                               startRow = 6, 
                               header = TRUE))




## Features ===============================================================

# FRED-QD
# https://research.stlouisfed.org/econ/mccracken/fred-databases/
raw_x_yq <- read_csv(file = file.path(getwd(), "Data", "Features", paste0(feature_latest, "_yq.csv")))
trans_info_yq <- raw_x_yq %>% 
  filter(rownames(.) == 2) %>% 
  select(-sasdate)
  

# FRED-MD
# https://research.stlouisfed.org/econ/mccracken/fred-databases/
# trans_info <- raw_x %>% filter(rownames(.) == 1)
# raw_x <- as_tibble(read.delim(file = file.path(getwd(), "Data", "2019-07.txt"),
#                               header = TRUE, 
#                               sep = ",", 
#                               dec = "."))







## Misc ===================================================================
# NBER recession dates
# https://www.nber.org/cycles.html
raw_recession <- as_tibble(read.xlsx(file = file.path(getwd(), "Data", "Misc", "NBER_chronology.xlsx"), 
                                     sheetIndex = 1, 
                                     startRow = 1, 
                                     header = TRUE)) %>% 
  mutate(DATE_QUARTER = yearquarter(as.yearqtr(format(DATE_QUARTER),       # change data types: convert first to lubridate::yearqtr and the to tsibble::yearquarter as the tsibble format is required for later operations
                                               "%YQ%q")))

# Preprocess data ---------------------------------------------------------
## Cleaning data ==========================================================
# GDP
tidy_gdp <- raw_gdp %>%
  select(-c(1:3)) %>%                                                      # drop first three rows as they only capture yearly figures
  rename(DATE_QUARTER = colnames(.)[1],                                    # rename columns
         NOMINAL_GDP = colnames(.)[2],
         REAL_GDP = colnames(.)[3]) %>% 
  mutate(DATE_QUARTER = yearquarter(as.yearqtr(format(DATE_QUARTER),       # change data types: convert first to lubridate::yearqtr and the to tsibble::yearquarter as the tsibble format is required for later operations
                                               "%YQ%q"))) %>% 
  mutate(REAL_GDP_GROWTH_A = 100*(((REAL_GDP/lag(REAL_GDP, n = 1))^4)-1),  # calculate annualized quarterly GDP growth 
         REAL_GDP_GROWTH = 100*(REAL_GDP/lag(REAL_GDP, n = 1)-1)) %>%      # calculate quarterly discrete GDP growth 
  select(DATE_QUARTER, everything())                                       # make DATE_QUARTER the first column followed by all others




# # GDP Real-Time Data Research Center of the Federal Reserve Bank of Philadelphia
# tidy_gdp <- raw_gdp %>% 
#   clean_names(case = "all_caps") %>%                                       # capitalize all variable names
#   mutate(DATE_QUARTER = yearquarter(as.yearqtr(format(DATE), "%Y:Q%q")),   # change data types: convert first to lubridate::yearqtr and the to tsibble::yearquarter as the tsibble format is required for later operations
#          #DATE = as.Date(DATE_QUARTER, format = "%Y Q%q"),
#          FIRST = as.numeric(as.character(FIRST)),
#          SECOND = as.numeric(as.character(SECOND))) %>% 
#   select(-c(DATE)) %>%                                                     # drop DATE as DATE_QUARTER is the date variable for all subsequent steps
#   mutate(REAL_GDP_GROWTH_A = REAL_GDP_GROWTH,                             # assign annualized quarterly GDP growth (in the original data gdp data extraction values are discrete annualized growth rates)
#          #REAL_GDP_GROWTH = REAL_GDP_GROWTH/4,                                     # calculate quarterly GDP growth
#          REAL_GDP_GROWTH = 100*((1+REAL_GDP_GROWTH_A/100)^(1/4)-1)) %>%   # calculate quarterly GDP growth (exact solution)
#   select(DATE_QUARTER, everything())                                       # make DATE_QUARTER the first column followed by all others



# FRED-QD
tidy_yx_yq <- raw_x_yq %>%   
  clean_names(case = "all_caps") %>% 
  filter(!(rownames(.) %in% c(1, 2))) %>%                                  # drop first two rows containing variable information
  mutate(DATE_QUARTER = yearquarter(as.yearqtr(SASDATE, "%m/%d/%Y"))) %>%  # create date variable as in tidy_gdp
  select(-SASDATE) %>%                                                     # drop old unformatted date variable
  left_join(tidy_gdp, by = c("DATE_QUARTER")) %>%                          # join gdp data by DATE_QUARTER resulting in a complete data set with target and features
  mutate(DATE_QUARTER = yearquarter(as.yearqtr(DATE_QUARTER, 
                                               "%m/%d/%Y"))) %>%           # define DATE_QUARTER as qtr type again since the join command does not preserve the data type
  # mutate(GDPC1_GROWTH_ANNUALIZED = c(NA, 4*(diff(log(GDPC1))*100)),        # calculate continuous quarterly GDP growth from GDPC1 manually and annualize it for comparison reasons (GDPC1 comes from FRED-QD data)
  #        GDPC1_GROWTH = c(NA, (diff(log(GDPC1))*100))) %>%                 # calculate quarterly GDP growth from GDPC1 manually
  mutate(GDPC1_GROWTH_ANNUALIZED = 100*(((GDPC1/lag(GDPC1, n = 1))^4)-1),  # calculate discrete quarterly GDP growth from GDPC1 manually and annualize it for comparison reasons (GDPC1 comes from FRED-QD data)
         GDPC1_GROWTH = 100*((1+GDPC1_GROWTH_ANNUALIZED/100)^(1/4)-1)) %>% # calculate quarterly GDP growth from GDPC1_GROWTH_ANNUALIZED manually
  select(DATE_QUARTER, NOMINAL_GDP, REAL_GDP, 
         REAL_GDP_GROWTH, REAL_GDP_GROWTH_A,
         GDPC1, GDPC1_GROWTH, GDPC1_GROWTH_ANNUALIZED, everything())

# Save dataset
# save(tidy_yx_yq, file = file.path(getwd(), "Data", "tidy_data", "tidy_yx_yq.RData"))

# FRED-MD    
# tidy_yx <- raw_x %>%   
#   clean_names(case = "all_caps") %>% 
#   filter(!(rownames(.) == 1)) %>% 
#   mutate(DATE = ceiling_date(as.Date(SASDATE, format = "%m/%d/%Y"),        # change date to date at end of month
#                              "month") - days(1),
#          DATE_QUARTER = yearquarter(as.yearqtr(DATE))) %>% 
#   select(-SASDATE) %>% 
#   left_join(tidy_gdp, by = c("DATE_QUARTER")) %>%                          # join gdp data by DATE_QUARTER
#   select(DATE, DATE_QUARTER, FIRST, 
#          SECOND, THIRD, REAL_GDP_GROWTH, everything()) #%>% 
# #as_tsibble(index = DATE) 
# 
# Use FRED-MD data for year-quarter dataset
# tidy_yx_yq <- tidy_yx %>% 
#   filter(month(DATE) %in% c(3, 6, 9, 12)) %>%                              # only keep end of quarter dates
#   filter(!(is.na(REAL_GDP_GROWTH)))                                            # only keep rows with observations for GDP growth


# Check variable GDPC1 vs. REAL_GDP_GROWTH
tidy_yx_yq %>% 
  select(DATE_QUARTER, GDPC1, GDPC1_GROWTH, GDPC1_GROWTH_ANNUALIZED,       # select GDP variables only
         REAL_GDP_GROWTH, REAL_GDP_GROWTH_A) %>%                           # select actual GDP growth variables
  select(DATE_QUARTER, GDPC1_GROWTH, GDPC1_GROWTH_ANNUALIZED, 
       REAL_GDP_GROWTH, REAL_GDP_GROWTH_A) %>% 
  melt(id.vars = "DATE_QUARTER", variable.name = "GDP_GROWTH") %>% 
  as_tibble() %>% 
  ggplot() +
  geom_line(aes(x = DATE_QUARTER, y = value, 
                color = GDP_GROWTH, linetype = GDP_GROWTH)) +
  scale_color_viridis(discrete = TRUE) +
  theme_thesis

## Impute data =============================================================
# Imputation of consumer sentiment UMCSEN_TX
for (i in 1:nrow(tidy_yx_yq)){
  
  if((is.na(tidy_yx_yq$UMCSEN_TX[i]) & i!=1)){                              # imputation by the mean of preceding and succeeding observation
    tidy_yx_yq$UMCSEN_TX[i] <- mean(tidy_yx_yq$UMCSEN_TX[i-1]:tidy_yx_yq$UMCSEN_TX[i+1])
  } 
  
}


## Stationary data ========================================================
### Stationarity preparation ##############################################
# Adjustemnts of transformation recommendations
# AWHMAN: Average Weekly Hours of Production and Nonsupervisory Employees: Manufacturing
trans_info_yq <- trans_info_yq %>% 
  mutate(TLBSNNCBBDIx = 2,                                                # Nonfinancial Corporate Business Sector Liabilities to Disposable Business Income (Percent)
         TLBSNNBBDIx = 2,                                                 # Nonfinancial Noncorporate Business Sector Liabilities to Disposable Business Income (Percent)
         #AWHMAN = 2,
         NWPIx = 5)                                                       # Households and nonprofit organizations; net worth as a percentage of disposable personal income, Level



tidy_yx_yq_stat <- tidy_yx_yq %>% 
  select(-c(NOMINAL_GDP, REAL_GDP, REAL_GDP_GROWTH,                        # drop GDP-related variables which have been differenced already
            REAL_GDP_GROWTH_A, GDPC1, GDPC1_GROWTH, 
            GDPC1_GROWTH_ANNUALIZED)) %>% 
  function_stationary(data = ., tcode = trans_info_yq,                     # make variables stationary
                      date_index = "DATE_QUARTER", ppt = TRUE) %>% 
  left_join(tidy_yx_yq %>% 
              select(c(DATE_QUARTER,NOMINAL_GDP, REAL_GDP, 
                       REAL_GDP_GROWTH, REAL_GDP_GROWTH_A, 
                       GDPC1, GDPC1_GROWTH, GDPC1_GROWTH_ANNUALIZED)), 
            by = c("DATE_QUARTER")) %>%                                    # rejoin gdp data by DATE_QUARTER
  mutate(DATE_QUARTER = yearquarter(as.yearqtr(DATE_QUARTER, 
                                               "%m/%d/%Y"))) %>%           # define DATE_QUARTER as qtr type again since the join command does not preserve the data type
  select(DATE_QUARTER, NOMINAL_GDP, REAL_GDP, 
         REAL_GDP_GROWTH, REAL_GDP_GROWTH_A,
         GDPC1, GDPC1_GROWTH, GDPC1_GROWTH_ANNUALIZED, everything())


### Check non-stationary variables ########################################
stationarity_testing <- function_stationary_tests(tidy_yx_yq_stat)         # create tibble with detailed stationarity results


stationarity_testing_short <- stationarity_testing %>% 
  filter(!(VARIABLE %in% c("NOMINAL_GDP", "REAL_GDP", "GDPC1"))) %>%       # deselect level variables
  select(VARIABLE, ADF_NONE, ADF_DRIFT, ADF_TREND1, ADF_TREND2, KPSS) %>%  # select only variable name and stationarity classifier
  replace(.=="stationary", 0) %>%                                          # make classifier numeric: stationary = 0, non-stationary = 1
  replace(.=="non-stationary", 1) %>%                                      
  mutate_at(.vars = c("ADF_NONE", "ADF_DRIFT", "ADF_TREND1", "ADF_TREND2", "KPSS"), 
            .funs = function(x) as.numeric(x)) %>%                         # change variable type to numeric 
  mutate(NONSTATIONARY_CLASS = rowSums(x = .[,-1]))                        # calculate how many of the classifiers indicate non-stationarity

  


# Inspection of variables which have been detected as non-stationary by all of the tests
nonstat_5 <- stationarity_testing_short %>% 
  filter(NONSTATIONARY_CLASS == 5) %>% 
  select(VARIABLE) %>% 
  as_vector() %>% 
  paste()


tidy_yx_yq_stat %>% 
  select(DATE_QUARTER, nonstat_5) %>% 
  melt(id.vars = "DATE_QUARTER", variable.name = "ESTIMATES") %>% 
  as_tibble() %>% 
  ggplot() +
  geom_line(aes(x = DATE_QUARTER, y = value, color = ESTIMATES), alpha = 0.5) +
  facet_wrap("ESTIMATES", scales = "free") +
  scale_color_viridis(discrete = TRUE) +
  theme_thesis +
  theme(legend.position = "none")

# Comments:
# - HW_IX: Help-Wanted Advertising in Newspapers for United States (tcode: 1)
#          Clearly the time series is non-stationary. However, while online only data is available up to 1966, the series in FRED-QD publishes 
#          values up to today without clear reference where the values come from
#          -> drop
# - NWP_IX: Households and nonprofit organizations; net worth as a percentage of disposable personal income, Level (tcode: 1)
#           Clearly non-stationary. Transformation: 5 instead of 1
#           -> keep 



# Inspection of variables which have been detected as non-stationary by 4 of the tests
nonstat_4 <- stationarity_testing_short %>% 
  filter(NONSTATIONARY_CLASS == 4) %>% 
  select(VARIABLE) %>% 
  as_vector() %>% 
  paste()
# there are none


# Inspection of variables which have been detected as non-stationary by 3 of the tests
nonstat_3 <- stationarity_testing_short %>% 
  filter(NONSTATIONARY_CLASS == 3) %>% 
  select(VARIABLE) %>% 
  as_vector() %>% 
  paste()
# there are none


tidy_yx_yq_stat %>% 
  select(DATE_QUARTER, nonstat_3) %>% 
  melt(id.vars = "DATE_QUARTER", variable.name = "ESTIMATES") %>% 
  as_tibble() %>% 
  ggplot() +
  geom_line(aes(x = DATE_QUARTER, y = value, color = ESTIMATES), alpha = 0.5) +
  facet_wrap("ESTIMATES", scales = "free") +
  scale_color_viridis(discrete = TRUE) +
  theme_thesis +
  theme(legend.position = "none")

# Comments:
# - DRIWCIL: FRB Senior Loans Officer Opions. Net Percentage of Domestic Respondents Reporting Increased Willingness to Make Consumer Installment Loans (tcode: 1)
#            Data only available from 1982 on. 
#            -> drop
# - SPCS10RSA: S&P/Case-Shiller 10-City Composite Home Price Index (Index January 2000 = 100) (tcode: 5)
#              Data only available from 1982 on.
#              -> drop 
# - SPCS20RSA: S&P/Case-Shiller 10-City Composite Home Price Index (Index January 2000 = 100) (tcode: 5)
#              Data only available from beginning of 2000s on.
#              -> drop 
# - UMCSEN_TX: University of Michigan: Consumer Sentiment (Index 1st Quarter 1966=100) (tcode: 1)
#              Looks stationary (at least no trend). KPSS suggests stationarity. No changes
#              -> keep
# - TLBSNNCBBD_IX: Nonfinancial Corporate Business Sector Liabilities to Disposable Business Income (Percent) (tcode: 1)
#                  Looks slightly trending. KPSS suggests non-stationary. Transformation: 2 instead of 1
#                  -> keep
# - TLBSNNBBD_IX: Nonfinancial Noncorporate Business Sector Liabilities to Disposable Business Income (Percent)
#                 Looks slightly trending. KPSS suggests non-stationary. Transformation: 2 instead of 1
#                 -> keep



# Inspection of variables which have been detected as non-stationary by 3 of the tests
nonstat_2 <- stationarity_testing_short %>% 
  filter(NONSTATIONARY_CLASS == 2) %>% 
  select(VARIABLE) %>% 
  as_vector() %>% 
  paste()
# there are none


tidy_yx_yq_stat %>% 
  select(DATE_QUARTER, nonstat_2) %>% 
  melt(id.vars = "DATE_QUARTER", variable.name = "ESTIMATES") %>% 
  as_tibble() %>% 
  ggplot() +
  geom_line(aes(x = DATE_QUARTER, y = value, color = ESTIMATES), alpha = 0.5) +
  facet_wrap("ESTIMATES", scales = "free") +
  scale_color_viridis(discrete = TRUE) +
  theme_thesis +
  theme(legend.position = "none")


# Comments:
# - TCU: Capacity Utilization: Total Industry (Percent of Capacity) (tcode: 1)
#        Data series starts later. 
#        -> drop
# - AHETPIx: Real Average Hourly Earnings of Production and Nonsupervisory Employees: Total Private (2012 Dollars per Hour), deflated by Core PCE (tcode: 5)
#            Data series starts later. 
#            -> drop 
# - SPCS20RSA: All-Transactions House Price Index for the United States (Index 1980 Q1=100) (tcode: 5)
#              Data series starts later. 
#              -> drop 




# Define no change variables
nochange_variables <- c("UMCSEN_TX",
                        "")


# Define dropping variables
drop_variables <- c("HW_IX",                                               # drop because of unclear data availability
                    "DRIWCIL",
                    "SPCS10RSA",
                    "SPCS20RSA",
                    "TCU",
                    "AHETP_IX",
                    "USSTHPI")












# -------------------------------------------------------------------------













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



## Final data ==============================================================
# Dropping features as series has only started later in time (missings at beginning of series) or due to substantial time lag in publishing (missings at end of series)
sapply(tidy_yx_yq_stat %>% 
         filter(!(row.names(.) %in% c(1,2))), function(x) which(is.na(x))) %>% 
  lapply(., function(x) length(x)>2) %>% 
  as_tibble() %>% 
  t() %>% 
  as_tibble(rownames = "FEATURES") %>% 
  filter(V1) %>% 
  View()

tidy_yx_yq_stat <- tidy_yx_yq_stat %>% 
  filter(!(row.names(.) %in% c(1,2))) %>%                                   # drop first to rows since these will be missing for most variables due to differencing
  drop_cols_any_na()                                                        # drop all remaining columns with any NA (see step above)



# The following leading indicators exist (only NASDAQ)
tidy_yx_yq_stat$HOUST
tidy_yx_yq_stat$AMDMN_OX
tidy_yx_yq_stat$S_P_500
tidy_yx_yq_stat$UMCSEN_TX
tidy_yx_yq_stat$AWHMAN
tidy_yx_yq_stat$CPF3MTB3MX


  



# Save dataset
# save(tidy_yx_yq, file = file.path(getwd(), "Results", "tidy_data", "tidy_yx_yq.RData"))
# save(tidy_yx_yq_stat, file = file.path(getwd(), "Results", "tidy_data", "tidy_yx_yq_stat.RData"))







## VAR ====================================================================
### Model selection #######################################################
tidy_yx_yq %>% 
  as_tibble() %>% 
  select(DATE_QUARTER, REAL_GDP_GROWTH, FEDFUNDS, UNRATE, CPIAUCSL) %>% 
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
# REAL_GDP_GROWTH (Annualized GDP Growth):     first difference of logarithm * 4 (this has already been done)

# Define dataset for VAR estimation according to McCracken 
data_VAR <- tidy_yx_yq %>% 
  select(DATE_QUARTER, REAL_GDP_GROWTH, FEDFUNDS, UNRATE, CPIAUCSL) %>%          
  mutate(FEDFUNDS_STAT = c(NA, diff(FEDFUNDS)),                         # first difference of interest rate
         UNRATE_STAT = c(NA, diff(UNRATE)),                             # first difference of unemployment rate
         CPIAUCSL_STAT = c(NA, NA, diff(log(CPIAUCSL), 
                                        differences = 2)*100)) %>%      # second difference of log of inflation rate
  na.omit()                                                             # drop years for wich no GDP growth rate exists (balanced panel)


data_VAR %>%   
  select(REAL_GDP_GROWTH, FEDFUNDS_STAT, UNRATE_STAT, CPIAUCSL_STAT) %>% 
  VARselect(type = "const", lag.max = 10)
# According to Schwarz-Criterion (BIC) two lags are preferred. But compare with other information criteria!

### Stationarity testing ##################################################
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


### Model estimation ######################################################




model_VAR <- data_VAR %>% 
  select(REAL_GDP_GROWTH, FEDFUNDS_STAT, UNRATE_STAT, CPIAUCSL_STAT) %>% 
  VAR(p = 2, type = "const")

summary(model_VAR$varresult$REAL_GDP_GROWTH)

# Compare:
model_ADL <- data_VAR %>% 
  select(DATE_QUARTER, REAL_GDP_GROWTH, FEDFUNDS_STAT, UNRATE_STAT, CPIAUCSL_STAT) %>% 
  ts(start = c(year(.$DATE_QUARTER[1]), quarter(.$DATE_QUARTER[1])),
     end = c(year(.$DATE_QUARTER[nrow(.)]), quarter(.$DATE_QUARTER[nrow(.)])),
     frequency = 4) %>% 
  dynlm(formula = REAL_GDP_GROWTH ~ L(REAL_GDP_GROWTH, 1:2) + L(FEDFUNDS_STAT, 1:2) + L(UNRATE_STAT, 1:2) + L(CPIAUCSL_STAT, 1:2))
summary(model_ADL)



### Granger causality #####################################################
# Tests whether independent variables (FEDFUNDS_STAT, UNRATE_STAT, CPIAUCSL_STAT) granger cause dependent variable (REAL_GDP_GROWTH).
# Basically this is a F-Test comparing explanatory power of an unrestricted model with all variables and the smaller model
# without the independent variable whose "causality" should be tested.
# Multivariate granger causality test
data_VAR %>% 
  select(REAL_GDP_GROWTH, FEDFUNDS_STAT, UNRATE_STAT, CPIAUCSL_STAT) %>%
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
  select(REAL_GDP_GROWTH, FEDFUNDS_STAT, UNRATE_STAT, CPIAUCSL_STAT) %>%
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
                                                 "%Y Q%q"))) %>% 
  filter(!(row.names(.) %in% 1:3))


tidy_yx_yq %>% 
  select(DATE_QUARTER, REAL_GDP) %>% 
  #left_join(raw_recession, by = "DATE_QUARTER") %>% 
  ggplot() +
  geom_line(aes(x = DATE_QUARTER, y = REAL_GDP), color = "#E7B800") +  
  geom_rect(data = recessions,
            aes(xmin = start_recession,
                xmax = end_recession,
                ymin = -Inf,
                ymax = Inf),
            fill = "grey",
            alpha = 0.3) +
  scale_color_manual(values = "#E7B800") +  
  xlab("Date") +
  ylab("Real GDP (seasonal adjusted)") +
  theme_economist()
  
tidy_yx_yq %>% 
  select(DATE_QUARTER, REAL_GDP_GROWTH) %>% 
  #left_join(raw_recession, by = "DATE_QUARTER") %>% 
  ggplot() +
  geom_line(aes(x = DATE_QUARTER, y = REAL_GDP_GROWTH), color = "#E7B800") +  
  geom_rect(data = recessions,
            aes(xmin = start_recession,
                xmax = end_recession,
                ymin = -Inf,
                ymax = Inf),
            fill = "grey",
            alpha = 0.3) +
  scale_color_manual(values = "#E7B800") +  
  xlab("Date") +
  ylab("Real GDP growth (seasonal adjusted)") +
  theme_economist()
  