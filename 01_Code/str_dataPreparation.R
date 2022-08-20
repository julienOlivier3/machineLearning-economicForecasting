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

# Actual GDP growth value for 2018 Q4 by vintage
gdp_vintages <- tibble(GDP = c(-0.965,
                               -1.625,
                               -1.371,
                               -1.371,
                               -1.371,
                               -1.371,
                               -1.738,
                               -1.738,
                               -1.738,
                               -1.738,
                               -2.301,
                               -2.301,
                               -2.301,
                               -2.301,
                               -2.301,
                               -2.301,
                               -2.301,
                               -2.301,
                               -2.152,
                               -2.152,
                               -2.152,
                               -2.152,
                               -2.113,
                               -2.113,
                               -2.113,
                               -2.113,
                               -2.113,
                               -2.113,
                               -2.113,
                               -2.113,
                               -2.113,
                               -2.113,
                               -2.113,
                               -2.113,
                               -2.113,
                               -2.113,
                               -2.113,
                               -2.113,
                               -2.164,
                               -2.164,
                               -2.164,
                               -2.164,
                               -2.164), 
                       DATE_QUARTER = yearquarter(seq.Date(yearquarter("2009 Q1"), yearquarter("2019 Q3"), by = "quarter")))

gdp_vintages <- tibble(GDP = c(0.149 	,
                               0.218 	,
                               0.218 	,
                               0.218 	,
                               0.218 	,
                               -0.181 	,
                               -0.181 	,
                               -0.181 	,
                               -0.181 	,
                               -0.182 	,
                               -0.182 	,
                               -0.182 	,
                               -0.182 	,
                               -0.444 	,
                               -0.444 	,
                               -0.444 	,
                               -0.444 	,
                               -0.444 	,
                               -0.444 	,
                               -0.444 	,
                               -0.444 	,
                               -0.672 	,
                               -0.672 	,
                               -0.672 	,
                               -0.672 	,
                               -0.682 	,
                               -0.682 	,
                               -0.682 	,
                               -0.682 	,
                               -0.682 	,
                               -0.682 	,
                               -0.682 	,
                               -0.682 	,
                               -0.682 	,
                               -0.682 	,
                               -0.682 	,
                               -0.682 	,
                               -0.682 	,
                               -0.682 	,
                               -0.682 	,
                               -0.682 	,
                               -0.575 	,
                               -0.575 	,
                               -0.575 	,
                               -0.575 	,
                               -0.575), 
                       DATE_QUARTER = yearquarter(seq.Date(yearquarter("2008 Q2"), yearquarter("2019 Q3"), by = "quarter"))) 	





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


# Same data but rearranged
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
data_recessions <- tibble(start_recession = start_recession, end_recession = end_recession) %>% 
  mutate(start_recession = yearquarter(as.yearqtr(format(start_recession),       # change data types: convert first to lubridate::yearqtr and the to tsibble::yearquarter as the tsibble format is required for later operations
                                                  "%Y Q%q")),
         end_recession = yearquarter(as.yearqtr(format(end_recession),       # change data types: convert first to lubridate::yearqtr and the to tsibble::yearquarter as the tsibble format is required for later operations
                                                "%Y Q%q")))


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
# tidy_yx_yq %>% 
#   select(DATE_QUARTER, GDPC1, GDPC1_GROWTH, GDPC1_GROWTH_ANNUALIZED,       # select GDP variables only
#          REAL_GDP_GROWTH, REAL_GDP_GROWTH_A) %>%                           # select actual GDP growth variables
#   select(DATE_QUARTER, GDPC1_GROWTH, GDPC1_GROWTH_ANNUALIZED, 
#        REAL_GDP_GROWTH, REAL_GDP_GROWTH_A) %>% 
#   melt(id.vars = "DATE_QUARTER", variable.name = "GDP_GROWTH") %>% 
#   as_tibble() %>% 
#   ggplot() +
#   geom_line(aes(x = DATE_QUARTER, y = value, 
#                 color = GDP_GROWTH, linetype = GDP_GROWTH)) +
#   scale_color_viridis(discrete = TRUE) +
#   theme_thesis

## Impute data =============================================================
# Imputation of consumer sentiment UMCSEN_TX
for (i in 1:nrow(tidy_yx_yq)){
  
  if((is.na(tidy_yx_yq$UMCSEN_TX[i]) & i!=1)){                              # imputation by the mean of preceding and succeeding observation
    tidy_yx_yq$UMCSEN_TX[i] <- mean(tidy_yx_yq$UMCSEN_TX[i-1]:tidy_yx_yq$UMCSEN_TX[i+1])
  } 
  
}

# Dropping of incomplete series (missings at beginning of series)
incomplete_variables <- c("OUTMS",
                          "TCU",
                          "LNS13023621",
                          "LNS13023557",
                          "LNS13023705",
                          "LNS13023569",
                          "HOAMS",
                          "AWHNONAG",
                          "PERMIT",
                          "ACOGN_OX",
                          "ANDEN_OX",
                          "INVCQRMTSPL",
                          "WPU0531",
                          "AHETP_IX", 
                          "COMPRMS", 
                          "OPHMFG", 
                          "ULCMFG", 
                          "MORTGAGE30US", 
                          "MORTG10Y_RX", 
                          "IMFS_LX", 
                          "REVOLS_LX", 
                          "DRIWCIL", 
                          "VXOCL_SX", 
                          "USSTHPI", 
                          "SPCS10RSA", 
                          "SPCS20RSA", 
                          "TWEXMMTH", 
                          "EXUSEU", 
                          "USEPUINDXM", 
                          "GFDEGDQ188S", 
                          "GFDEBT_NX", 
                          "PERMITNE", 
                          "PERMITMW", 
                          "PERMITS", 
                          "PERMITW", 
                          "NASDAQCOM", 
                          "CUSR0000SEHC")

tidy_yx_yq <- tidy_yx_yq %>% 
  select(-incomplete_variables)

## Stationary data ========================================================
### Stationarity preparation ##############################################
# Adjustemnts of transformation recommendations
trans_info_yq <- trans_info_yq %>% 
  mutate(TLBSNNCBBDIx = 2,                                                # Nonfinancial Corporate Business Sector Liabilities to Disposable Business Income (Percent)
         TLBSNNBBDIx = 2,                                                 # Nonfinancial Noncorporate Business Sector Liabilities to Disposable Business Income (Percent)
         CUMFNS = 2,                                                      # Capacity Utilization: Manufacturing (SIC) (Percent of Capacity)
         AWHMAN = 2,                                                      # Average Weekly Hours of Production and Nonsupervisory Employees: Manufacturing (Hours)
         BAA10YM = 5,                                                     # Moody's Seasoned Baa Corporate Bond Yield Relative to Yield on 10-Year Treasury Constant Maturity (Percent)
         GS10TB3MX = 2,                                                   # 10-Year Treasury Constant Maturity Minus 3-Month Treasury Bill, secondary market (Percent)
         GS1TB3Mx = 2,                                                    # 1-Year Treasury Constant Maturity Minus 3-Month Treasury Bill, secondary market (Percent)
         UMCSENTx = 5,                                                    # University of Michigan: Consumer Sentiment (Index 1st Quarter 1966=100)
         COMPAPFF = 2,                                                    # 3-Month Commercial Paper Minus Federal Funds Rate
         TB3SMFFM = 2,                                                    # 3-Month Treasury Constant Maturity Minus Federal Funds Rate
         A014RE1Q156NBEA = 2,                                             #	Shares of gross domestic product: Gross private domestic investment: Change in private inventories (Percent)
         CPF3MTB3Mx = 2,                                                  #	3-Month Commercial Paper Minus 3-Month Treasury Bill, secondary market (Percent)
         AAAFFM	= 2,                                                      # Moody's x Aaa Corporate Bond Minus Federal Funds Rate
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
  filter(!(row.names(.) %in% c(1, 2))) %>%                                 # drop first two rows which include missings due to differencing
  select(DATE_QUARTER, NOMINAL_GDP, REAL_GDP, 
         REAL_GDP_GROWTH, REAL_GDP_GROWTH_A,
         GDPC1, GDPC1_GROWTH, GDPC1_GROWTH_ANNUALIZED, everything())


### Check non-stationary variables ########################################
stationarity_testing <- function_stationary_tests(tidy_yx_yq_stat)         # create tibble with detailed stationarity results
                                                  


stationarity_testing_short <- stationarity_testing %>% 
  filter(!(VARIABLE %in% c("NOMINAL_GDP", "REAL_GDP", "GDPC1"))) %>%       # deselect level variables
  select(VARIABLE, ADF_NONE, ADF_DRIFT, ADF_TREND1,                        # select only variable name and stationarity classifier
         ADF_TREND2, KPSS_LEVEL, KPSS_TREND) %>%                           
  replace(.=="stationary", 0) %>%                                          # make classifier numeric: stationary = 0, non-stationary = 1
  replace(.=="non-stationary", 1) %>%                                      
  mutate_at(.vars = c("ADF_NONE", "ADF_DRIFT", "ADF_TREND1", 
                      "ADF_TREND2", "KPSS_LEVEL", "KPSS_TREND"), 
            .funs = function(x) as.numeric(x)) %>%                         # change variable type to numeric 
  mutate(NONSTATIONARY_CLASS = rowSums(x = .[,-1]),                        # calculate how many of the classifiers indicate non-stationarity
         NONSTATIONARY_CLASS_TREND = rowSums(x = .[, c("ADF_TREND1",
                                                       "ADF_TREND2",
                                                       "KPSS_TREND")]),
         NONSTATIONARY_CLASS_LEVEL = rowSums(x = .[, c("ADF_NONE",
                                                       "ADF_DRIFT",
                                                       "KPSS_LEVEL")]),)



# 6 -----------------------------------------------------------------------
# Inspection of variables which have been detected as non-stationary by all of the tests
nonstat_6 <- stationarity_testing_short %>% 
  filter(NONSTATIONARY_CLASS == 6) %>% 
  select(VARIABLE) %>% 
  as_vector() %>% 
  paste()
# there are none


# 5 -----------------------------------------------------------------------
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

# More detailed check
stationarity_testing_short %>% 
  filter(VARIABLE == "UMCSEN_TX")

# Comments:
# - HW_IX: Help-Wanted Advertising in Newspapers for United States (tcode: 1)
#          Clearly the time series is non-stationary. However, while online only data is available up to 1966, the series in FRED-QD publishes 
#          values up to today without clear reference where the values come from
#          -> drop
# - NWP_IX: Households and nonprofit organizations; net worth as a percentage of disposable personal income, Level (tcode: 1)
#           Clearly non-stationary. Transformation: 5 instead of 1
#           -> keep 
# - UMCSEN_TX: University of Michigan: Consumer Sentiment (Index 1st Quarter 1966=100) (tcode: 1)
#              Volatility clusters. Transformation: 5 instead of 1
#              -> keep 



# 4 -----------------------------------------------------------------------
# Inspection of variables which have been detected as non-stationary by 4 of the tests
nonstat_4 <- stationarity_testing_short %>% 
  filter(NONSTATIONARY_CLASS == 4) %>% 
  select(VARIABLE) %>% 
  as_vector() %>% 
  paste()
# there are none



# 3 -----------------------------------------------------------------------
# Inspection of variables which have been detected as non-stationary by 3 of the tests
nonstat_3 <- stationarity_testing_short %>% 
  filter(NONSTATIONARY_CLASS == 3) %>% 
  select(VARIABLE) %>% 
  as_vector() %>% 
  paste()



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

# More detailed check
stationarity_testing_short %>% 
  filter(VARIABLE %in% nonstat_3)

trans_info_yq %>% 
  clean_names(case = "all_caps") %>% 
  select(nonstat_3)

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
# - CES2000000008X: Real Average Hourly Earnings of Production and Nonsupervisory Employees: Construction (2012 Dollars per Hour), deflated by Core PCE (tcode: 5)
#                   No constant mean. no change
#                   -> keep
# - CES3000000008X: Real Average Hourly Earnings of Production and Nonsupervisory Employees: Construction (2012 Dollars per Hour), deflated by Core PCE (tcode: 5)
#                   No constant mean. no change
#                   -> keep
# - GS1TB3MX: 1-Year Treasury Constant Maturity Minus 3-Month Treasury Bill, secondary market (Percent) (tcode: 1)
#             No constant mean. Transformation: 2 instead of 1
#             -> keep     






# 2 -----------------------------------------------------------------------
# Inspection of variables which have been detected as non-stationary by 2 of the tests
nonstat_2 <- stationarity_testing_short %>% 
  filter(NONSTATIONARY_CLASS == 2) %>% 
  select(VARIABLE) %>% 
  as_vector() %>% 
  paste()

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

# More detailed check
stationarity_testing_short %>% 
  filter(VARIABLE %in% nonstat_2)

trans_info_yq %>% 
  clean_names(case = "all_caps") %>% 
  select(nonstat_2)

# Comments:
# - TCU: Capacity Utilization: Total Industry (Percent of Capacity) (tcode: 1)
#        Data series starts later. 
#        -> drop
# - AHETPIx: Real Average Hourly Earnings of Production and Nonsupervisory Employees: Total Private (2012 Dollars per Hour), deflated by Core PCE (tcode: 5)
#            Data series starts later. 
#            -> drop 
# - USSTHPI: All-Transactions House Price Index for the United States (Index 1980 Q1=100) (tcode: 5)
#            Data series starts later. 
#            -> drop
# - CUMFNS: Capacity Utilization: Manufacturing (SIC) (Percent of Capacity) (tcode: 1)
#           Clearly trending: Transformation: 2 instead of 1
#           -> keep
# - USEHS: All Employees: Education & Health Services (Thousands of Persons) (tcode: 5)
#          Neither trend stationary nor variance stationary. Series is already transformed as percentage change
#          -> drop
# - AWHMAN: Average Weekly Hours of Production and Nonsupervisory Employees: Manufacturing (Hours) (tcode: 5)
#           Series is trending. Transformation: 2 instead of 1
#           -> keep
# - BAA10YM: Moody's Seasoned Baa Corporate Bond Yield Relative to Yield on 10-Year Treasury Constant Maturity (Percent) (tcode: 1)
#            Series is trending. Transformation: 5 instead of 1
#            -> keep
# - TB3SMFFM: 3-Month Treasury Constant Maturity Minus Federal Funds Rate
#             Volatility clusters. Transformation: 2 instead of 1
#             -> keep
# - COMPAPFF: 3-Month Commercial Paper Minus Federal Funds Rate
#             Volatility clusters. Transformation: 2 instead of 1
#             -> keep












# 1 -----------------------------------------------------------------------
# Inspection of variables which have been detected as non-stationary by 1 of the tests
nonstat_1 <- stationarity_testing_short %>% 
  filter(NONSTATIONARY_CLASS == 1) %>% 
  select(VARIABLE) %>% 
  as_vector() %>% 
  paste()


tidy_yx_yq_stat %>% 
  select(DATE_QUARTER, nonstat_1) %>% 
  melt(id.vars = "DATE_QUARTER", variable.name = "ESTIMATES") %>% 
  as_tibble() %>% 
  ggplot() +
  geom_line(aes(x = DATE_QUARTER, y = value, color = ESTIMATES), alpha = 0.5) +
  facet_wrap("ESTIMATES", scales = "free") +
  scale_color_viridis(discrete = TRUE) +
  theme_thesis +
  theme(legend.position = "none")



# From nonstat1 check the variables where the KPSS test hints to rejecting trend stationarity at the 1% level
nonstat_1_trend <- stationarity_testing %>% 
  filter(VARIABLE%in%nonstat_1) %>%                                        # filter variables in nonstat_1
  filter(KPSS_TREND=="non-stationary"|KPSS_LEVEL=="non-stationary") %>%    # filter those variables which were classified as non-stationary by KPSS test
  select(VARIABLE,contains("PVALUE")) %>%                                  # select p-value of KPSS test
  filter(KPSS_MODEL_TREND_PVALUE==0.01) %>%                                # select variables with p-value of 0.01 in trend stationarity test (sure to reject trend stationarity)
  select(VARIABLE) %>% 
  as_vector() %>% 
  paste()

tidy_yx_yq_stat %>% 
  select(DATE_QUARTER, nonstat_1_trend) %>% 
  melt(id.vars = "DATE_QUARTER", variable.name = "ESTIMATES") %>% 
  as_tibble() %>% 
  ggplot() +
  geom_line(aes(x = DATE_QUARTER, y = value, color = ESTIMATES), alpha = 0.5) +
  facet_wrap("ESTIMATES", scales = "free") +
  scale_color_viridis(discrete = TRUE) +
  theme_thesis +
  theme(legend.position = "none")

# More detailed check
stationarity_testing_short %>% 
  filter(VARIABLE %in% nonstat_1_trend)

trans_info_yq %>% 
  clean_names(case = "all_caps") %>% 
  select(nonstat_1_trend)




# From nonstat1 check the variables where the KPSS test hints to to rejecting level stationarity at the 1% level
nonstat_1_level <- stationarity_testing %>% 
  filter(VARIABLE%in%nonstat_1) %>%                                        # filter variables in nonstat_1
  filter(KPSS_TREND=="non-stationary"|KPSS_LEVEL=="non-stationary") %>%    # filter those variables which were classified as non-stationary by KPSS test
  select(VARIABLE,contains("PVALUE")) %>%                                  # select p-value of KPSS test
  filter(KPSS_MODEL_LEVEL_PVALUE==0.01) %>%                                # select variables with p-value of 0.01 in level stationarity test (sure to reject level stationarity)
  select(VARIABLE) %>% 
  as_vector() %>% 
  paste()

tidy_yx_yq_stat %>% 
  select(DATE_QUARTER, nonstat_1_level) %>% 
  melt(id.vars = "DATE_QUARTER", variable.name = "ESTIMATES") %>% 
  as_tibble() %>% 
  ggplot() +
  geom_line(aes(x = DATE_QUARTER, y = value, color = ESTIMATES), alpha = 0.5) +
  facet_wrap("ESTIMATES", scales = "free") +
  scale_color_viridis(discrete = TRUE) +
  theme_thesis +
  theme(legend.position = "none")

# More detailed check
stationarity_testing_short %>% 
  filter(VARIABLE %in% nonstat_1_level)

trans_info_yq %>% 
  clean_names(case = "all_caps") %>% 
  select(nonstat_1_level)



# From nonstat1 remaining variables
nonstat_1_remain <- stationarity_testing %>% 
  filter(VARIABLE%in%nonstat_1) %>%                                        # filter variables in nonstat_1
  filter(KPSS_TREND=="non-stationary"|KPSS_LEVEL=="non-stationary") %>%    # filter those variables which were classified as non-stationary by KPSS test
  select(VARIABLE,contains("PVALUE")) %>%                                  # select p-value of KPSS test
  filter(VARIABLE %in% setdiff(nonstat_1,                                  # select remaining variables
                               c(nonstat_1_level, nonstat_1_trend))) %>%                               
  select(VARIABLE) %>% 
  as_vector() %>% 
  paste()

tidy_yx_yq_stat %>% 
  select(DATE_QUARTER, nonstat_1_remain) %>% 
  melt(id.vars = "DATE_QUARTER", variable.name = "ESTIMATES") %>% 
  as_tibble() %>% 
  ggplot() +
  geom_line(aes(x = DATE_QUARTER, y = value, color = ESTIMATES), alpha = 0.5) +
  facet_wrap("ESTIMATES", scales = "free") +
  scale_color_viridis(discrete = TRUE) +
  theme_thesis +
  theme(legend.position = "none")

# More detailed check
stationarity_testing_short %>% 
  filter(VARIABLE %in% nonstat_1_remain)

stationarity_testing %>% 
  filter(VARIABLE%in%nonstat_1_remain) %>%
  select(VARIABLE,contains("PVALUE"))

trans_info_yq %>% 
  clean_names(case = "all_caps") %>% 
  select(nonstat_1_level)



### Non-stationarity handling ---------------------------------------------

# Define no change variables
nochange_variables <- c("UMCSEN_TX",
                        "USEHS",
                        "PCECC96",
                        "DPIC96",
                        "IPFINAL",
                        "PAYEMS",
                        "NDMANEMP",
                        "USLAH",
                        "CE16OV",
                        "RCPHBS",
                        "UNLPNBS",
                        "TB6M3MX",
                        "GS10TB3MX",
                        "M1REAL",
                        "TLBSHN_OX",
                        "TB3SMFFM",
                        "COMPAPFF",
                        "PCES_VX",
                        "A014RE1Q156NBEA",
                        "SLC_EX",
                        "IPCONGD",
                        "IPNMAT",
                        "IPNCONGD",
                        "IPB51220SQ",
                        "SRVPRD",
                        "USFIRE",
                        "USSERV",
                        "USTPU",
                        "USGOVT",
                        "USTRADE",
                        "USWTRADE",
                        "CES9092000001",
                        "CES9093000001",
                        "CIVPART",
                        "RSAF_SX",
                        "CES2000000008X",
                        "CES3000000008X",
                        "ULCBS",
                        "ULCNFB",
                        "GS1TB3MX",
                        "CPF3MTB3MX",
                        "IPB51222S",
                        "AAAFFM",
                        "BUSIN_VX",
                        "LIABP_IX",
                        "M2REAL",
                        "NONREVS_LX",
                        "CONSP_IX")


# Define dropping variables
drop_variables <- c("HW_IX")                                               # drop because of unclear data availability
          

# Drop dropping variables
tidy_yx_yq_stat <- tidy_yx_yq_stat %>% 
  select(-drop_variables)
  












## Final data ==============================================================

# The following leading indicators exist (only NASDAQ drops as series starts later)
tidy_yx_yq_stat$HOUST
tidy_yx_yq_stat$AMDMN_OX
tidy_yx_yq_stat$S_P_500
tidy_yx_yq_stat$UMCSEN_TX
tidy_yx_yq_stat$AWHMAN
tidy_yx_yq_stat$CPF3MTB3MX

  



# Save dataset
# save(tidy_yx_yq, file = file.path(getwd(), "Results", "tidy_data", "tidy_yx_yq.RData"))
# save(tidy_yx_yq_stat, file = file.path(getwd(), "Results", "tidy_data", "tidy_yx_yq_stat.RData"))





# Visualizations ----------------------------------------------------------

## Leading indicators =====================================================
# HOUST
houst <- tidy_yx_yq %>% 
  select(DATE_QUARTER
         ,HOUST
         ) %>%
  melt(id.vars = "DATE_QUARTER", variable.name = "ESTIMATES") %>%
  as_tibble() %>%
  mutate(ESTIMATES = map(ESTIMATES, ~ gsub("_", "\\\\_", .))) %>% 
  unnest() %>% 
  ggplot() +
  geom_rect(data = data_recessions %>% filter(!(row.names(.) %in% 1:3)),
            aes(xmin = start_recession,
                xmax = end_recession,
                ymin = -Inf,
                ymax = Inf),
            fill = "grey60",
            alpha = 0.5) +
  geom_line(aes(x = DATE_QUARTER, y = value), color = ml_green_dark) +
  scale_y_continuous(labels = function(x) format(x, big.mark=",", decimal.mark = ".", scientific = FALSE)) +
  theme_thesis +
  theme(legend.position = "none",
        panel.grid.major.x = element_blank(),
        axis.title.y = element_text(size = 6, face = "bold", vjust = -.1),
        axis.title.x = element_text(size = 6, face = "bold", vjust = -.1),
        axis.text.x = element_text(size = 7, color = "black"),
        axis.text.y = element_text(size = 7, color = "black")) +
  xlab("Year") +
  ylab("HOUST\n (in thous. of units)")

# AMDMN_OX
amdmn <- tidy_yx_yq %>% 
  select(DATE_QUARTER
         ,AMDMN_OX
  ) %>%
  melt(id.vars = "DATE_QUARTER", variable.name = "ESTIMATES") %>%
  as_tibble() %>%
  mutate(ESTIMATES = map(ESTIMATES, ~ gsub("_", "\\\\_", .))) %>% 
  unnest() %>% 
  ggplot() +
  geom_rect(data = data_recessions %>% filter(!(row.names(.) %in% 1:3)),
            aes(xmin = start_recession,
                xmax = end_recession,
                ymin = -Inf,
                ymax = Inf),
            fill = "grey60",
            alpha = 0.5) +
  geom_line(aes(x = DATE_QUARTER, y = value), color = ml_green_dark) +
  scale_y_continuous(labels = function(x) format(x, big.mark=",", decimal.mark = ".", scientific = FALSE)) +
  theme_thesis +
  theme(legend.position = "none",
        panel.grid.major.x = element_blank(),
        axis.title.y = element_text(size = 6, face = "bold", vjust = -.1),
        axis.title.x = element_text(size = 6, face = "bold", vjust = -.1),
        axis.text.x = element_text(size = 7, color = "black"),
        axis.text.y = element_text(size = 7, color = "black")) +
  xlab("Year") +
  ylab("AMDMN\\_OX\n (in mns of 2012 U.S. \\$)")


# S_P_500
sp500 <- tidy_yx_yq %>% 
  select(DATE_QUARTER
         ,S_P_500
  ) %>%
  melt(id.vars = "DATE_QUARTER", variable.name = "ESTIMATES") %>%
  as_tibble() %>%
  mutate(ESTIMATES = map(ESTIMATES, ~ gsub("_", "\\\\_", .))) %>% 
  unnest() %>% 
  ggplot() +
  geom_rect(data = data_recessions %>% filter(!(row.names(.) %in% 1:3)),
            aes(xmin = start_recession,
                xmax = end_recession,
                ymin = -Inf,
                ymax = Inf),
            fill = "grey60",
            alpha = 0.5) +
  geom_line(aes(x = DATE_QUARTER, y = value), color = ml_green_dark) +
  scale_y_continuous(labels = function(x) format(x, big.mark=",", decimal.mark = ".", scientific = FALSE)) +
  theme_thesis +
  theme(legend.position = "none",
        panel.grid.major.x = element_blank(),
        axis.title.y = element_text(size = 6, face = "bold", vjust = -.1),
        axis.title.x = element_text(size = 6, face = "bold", vjust = -.1),
        axis.text.x = element_text(size = 7, color = "black"),
        axis.text.y = element_text(size = 7, color = "black")) +
  xlab("Year") +
  ylab("S\\_P\\_500\n (in index points)")



# UMCSEN_TX
umcsen <- tidy_yx_yq %>% 
  select(DATE_QUARTER
         ,UMCSEN_TX
  ) %>%
  melt(id.vars = "DATE_QUARTER", variable.name = "ESTIMATES") %>%
  as_tibble() %>%
  mutate(ESTIMATES = map(ESTIMATES, ~ gsub("_", "\\\\_", .))) %>% 
  unnest() %>% 
  ggplot() +
  geom_rect(data = data_recessions %>% filter(!(row.names(.) %in% 1:3)),
            aes(xmin = start_recession,
                xmax = end_recession,
                ymin = -Inf,
                ymax = Inf),
            fill = "grey60",
            alpha = 0.5) +
  geom_line(aes(x = DATE_QUARTER, y = value), color = ml_green_dark) +
  scale_y_continuous(labels = function(x) format(x, big.mark=",", decimal.mark = ".", scientific = FALSE)) +
  theme_thesis +
  theme(legend.position = "none",
        panel.grid.major.x = element_blank(),
        axis.title.y = element_text(size = 6, face = "bold", vjust = -.1),
        axis.title.x = element_text(size = 6, face = "bold", vjust = -.1),
        axis.text.x = element_text(size = 7, color = "black"),
        axis.text.y = element_text(size = 7, color = "black")) +
  xlab("Year") +
  ylab("UMCSEN\\_TX\n (in index points)")


# AWHMAN
awhman <- tidy_yx_yq %>% 
  select(DATE_QUARTER
         ,AWHMAN
  ) %>%
  melt(id.vars = "DATE_QUARTER", variable.name = "ESTIMATES") %>%
  as_tibble() %>%
  mutate(ESTIMATES = map(ESTIMATES, ~ gsub("_", "\\\\_", .))) %>% 
  unnest() %>% 
  ggplot() +
  geom_rect(data = data_recessions %>% filter(!(row.names(.) %in% 1:3)),
            aes(xmin = start_recession,
                xmax = end_recession,
                ymin = -Inf,
                ymax = Inf),
            fill = "grey60",
            alpha = 0.5) +
  geom_line(aes(x = DATE_QUARTER, y = value), color = ml_green_dark) +
  scale_y_continuous(labels = function(x) format(x, big.mark=",", decimal.mark = ".", scientific = FALSE)) +
  theme_thesis +
  theme(legend.position = "none",
        panel.grid.major.x = element_blank(),
        axis.title.y = element_text(size = 6, face = "bold", vjust = -.1),
        axis.title.x = element_text(size = 6, face = "bold", vjust = -.1),
        axis.text.x = element_text(size = 7, color = "black"),
        axis.text.y = element_text(size = 7, color = "black")) +
  xlab("Year") + 
  ylab("AWHMAN\n (in hours)")



# TB3SMFFM
tb3smffm <- tidy_yx_yq %>% 
  select(DATE_QUARTER
         ,TB3SMFFM
  ) %>%
  melt(id.vars = "DATE_QUARTER", variable.name = "ESTIMATES") %>%
  as_tibble() %>%
  mutate(ESTIMATES = map(ESTIMATES, ~ gsub("_", "\\\\_", .))) %>% 
  unnest() %>% 
  ggplot() +
  geom_rect(data = data_recessions %>% filter(!(row.names(.) %in% 1:3)),
            aes(xmin = start_recession,
                xmax = end_recession,
                ymin = -Inf,
                ymax = Inf),
            fill = "grey60",
            alpha = 0.5) +
  geom_line(aes(x = DATE_QUARTER, y = value), color = ml_green_dark) +
  scale_y_continuous(labels = function(x) format(x, big.mark=",", decimal.mark = ".", scientific = FALSE)) +
  theme_thesis +
  theme(legend.position = "none",
        panel.grid.major.x = element_blank(),
        axis.title.y = element_text(size = 6, face = "bold", vjust = -.1),
        axis.title.x = element_text(size = 6, face = "bold", vjust = -.1),
        axis.text.x = element_text(size = 7, color = "black"),
        axis.text.y = element_text(size = 7, color = "black")) +
  xlab("Year") +
  ylab("TB3SMFFM\n (in \\%)")




tikz("plot_li.tex",
     height = 6,
     width = 6)
grid.arrange(houst, amdmn, sp500, awhman, umcsen, tb3smffm)
dev.off()








## Real GDP growth ========================================================
# Secondary axis
ylim.prim <- c(floor(min(tidy_yx_yq$REAL_GDP_GROWTH)),ceiling(max(tidy_yx_yq$REAL_GDP_GROWTH)))   
ylim.sec <- ceiling(range(tidy_yx_yq$REAL_GDP))   

# Rescaling parameters
b <- diff(ylim.prim)/diff(ylim.sec)
a <- b*(ylim.prim[1] - ylim.sec[1])


tikz("plot_gdp.tex",
     height = 3,
     width = 6)
tidy_yx_yq %>% 
  select(DATE_QUARTER, REAL_GDP_GROWTH, REAL_GDP) %>%
  #as_tsibble(index = DATE_QUARTER) %>% 
  mutate(SET = ifelse(as.numeric(row.names(.)) < which(as.yearqtr(.$DATE_QUARTER)==as.yearqtr("2007 Q2")) , "Training", "Test"),
         SET = factor(SET, levels = c("Training", "Test"))) %>% 
  ggplot() +
  geom_rect(data = data_recessions %>% filter(!(row.names(.) %in% 1:3)),
            aes(xmin = start_recession,
                xmax = end_recession,
                ymin = -Inf,
                ymax = Inf),
            fill = "grey60",
            alpha = 0.5) +
  geom_line(aes(x = DATE_QUARTER, y = a + REAL_GDP*b, color = "Level"), linetype = 1) +
  geom_line(aes(x = DATE_QUARTER, y = REAL_GDP_GROWTH, color = "Growth Rate")) + 
  geom_vline(xintercept = as.numeric(tidy_yx_yq$DATE_QUARTER[194]), linetype = 3, color = bb_red_dark, size = 1) +
  scale_y_continuous("Real GDP Growth\n (in \\%)", 
                     sec.axis = sec_axis(~ (. - a)/b, name = "Real GDP\n (in bns 2012 U.S. \\$)", 
                                         labels = function(x) format(x, big.mark=",", decimal.mark = ".", scientific = FALSE))) +
  scale_color_manual(name = "",
                     values = c("Growth Rate" = ml_green_dark, "Level" = "black") 
  ) +
  xlab("Year") +
  scale_x_date(breaks = pretty_breaks(n = 20)) +
  theme_thesis +
  theme(panel.grid.major.x = element_line(color = "grey90", size = 0.5),
        legend.position = "top") +
  annotate(geom="label", x=as.Date("1981-01-01"), y=8, label="Training set",
           color="white", label.r = unit(0, "lines"), fill = bb_red_dark, label.size = 0) +
  annotate(geom="label", x=as.Date("2015-01-01"), y=8, label="Test set",
           color="white", label.r = unit(0, "lines"), fill = bb_red_dark, label.size = 0)
dev.off()


## GDP revisions ==========================================================



tikz("plot_vintages.tex",
     height = 3,
     width = 6)
gdp_vintages %>% 
  mutate(REVISION = as.character(c("no",ifelse(diff(GDP)!=0,"yes","no"))),
         REVISION = factor(REVISION, levels = c("yes", "no"), labels = c("yes", "no"))) %>% 
  ggplot() + 
  geom_line(aes(x = DATE_QUARTER, y = GDP), color = ml_green_dark, size = 1) +
  geom_point(aes(x = DATE_QUARTER, y = GDP, shape = REVISION, alpha = REVISION, color = REVISION),  size = 2) +
  scale_alpha_manual(name = "Revision",
                     labels = c("yes", "no"),
                     values = c("yes" = 1, "no" = 0.2) 
  ) +
  scale_color_manual(name = "Revision",
                     labels = c("yes", "no"),
                     values = c("yes" = bb_red_dark, "no" = ml_green_dark) 
  ) +
  scale_shape_manual(name = "Revision",
                     labels = c("yes", "no"),
                     values = c("yes" = 17, "no" = 16) 
  ) +
  #geom_hline(yintercept = 0, color = "red", alpha = 0.5, linetype = 3) +
  #geom_point(aes(x = yearquarter("2008 Q4"), y = -0.965), size = 2, color = ml_green_medium) + 
  #annotate("text", x= yearquarter("2008 Q4"), y= -0.85, label= "2008-Q4") +
  #ylim(-3, 0.5) +
  theme_thesis +
  xlab("Vintage") +
  ylab("Q1-2018 GDP Growth\n (in \\%)") +
  theme(panel.grid.major.x = element_line(color = "grey90", size = 0.5),
        legend.position = "top") +
  scale_x_date(breaks = scales::pretty_breaks(n = 10))
dev.off()
