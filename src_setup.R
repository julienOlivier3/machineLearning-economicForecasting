# Setup -------------------------------------------------------------------

# Remove workspace
#remove(list = ls())

# Read packages -----------------------------------------------------------
## Package install and load function ======================================
function_package <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}


## Package list ===========================================================
packages <- c("xlsx",                                                      # read excel files
              "reshape2",                                                  # data reshaping such as melt $ cast
              "imputeTS",                                                  # imputation methods for time series data
              "lubridate",                                                 # simple year, month, ... functions
              "zoo",                                                       # another time series package
              "forecast",                                                  # another time series package
              "tseries",                                                   # another time series package
              "timetk",                                                    # yet another time series package (suitable for time series machine learning)
              "dynlm",                                                     # estimating time series models
              "lmtest",                                                    # test statistics for parameter estimates
              "urca",                                                      # for stationarity testing
              "car",                                                       # for testing of granger causality (funtion linearHypothesis())
              "vars",                                                      # vector autoregressive modelling (VAR)
              "tree",                                                      # machine learning: constructs classification and rgression trees
              "rpart",                                                     # machine learning: recursive partitioning for decision trees
              "rpart.plot",                                                # plotting decision trees
              "caret",                                                     # plotting decision trees
              "randomForest",                                              # machine learning: random forest
              "randomForestSRC",                                           # machine learning: random forest
              "ranger",                                                    # machine learning: random forest
              "h2o",                                                       # machine learning: random forest
              "gbm",                                                       # machine learning: gradient boosting machine
              "xgboost",                                                   # machine learning: extreme gradient boosting
              "e1071",                                                     # machine learning: support vector regression
              "kernlab",                                                   # machine learning: support vector regression
              "mlr",                                                       # machine learning environment
              "Matrix",                                                    # coercing to sparse matrix (required as input data for some ML algorithms (e.g. xgboost))
              "irace",                                                     # iterated F-race tuning
              "janitor",                                                   # upper case colnames
              "gridExtra",                                                 # allow several ggplots to appear in one window
              "viridis",                                                   # color palette
              "tidyverse",                                                 # open up the universe ...
              "tsibble",                                                   # ... and expand it to time series analyses
              "tidyimpute",                                                # ... and data imputation
              "tidyquant",                                                 # ... and even more functions which can be applied in a tidy fashion
              "beepr",                                                     # sound indicating that code execution has finished
              "ggthemes"                                                   # additional ggplot themes (e.g. economist theme)
              )

function_package(packages)



# Colors ------------------------------------------------------------------
bb_blue_dark <- rgb(0, 69, 125, maxColorValue = 255)
bb_blue_medium <- rgb(102, 144, 177, maxColorValue = 255)
bb_blue_light <- rgb(204, 218, 229, maxColorValue = 255)

function_gradient_blue <- colorRampPalette(c(bb_blue_light, bb_blue_dark))




# Theme -------------------------------------------------------------------


theme_thesis <- theme(
  panel.background = element_blank(),
  panel.grid.major.y = element_line(color = "grey90", size = 0.5),
  panel.grid.minor.y = element_line(color = "grey90", size = 0.5),
  panel.grid.major.x = element_blank(),
  panel.grid.minor.x = element_blank(),
  #panel.grid.major.x = element_blank(),
  #panel.border = element_rect(fill = NA, color = "grey20"),
  #axis.text.x = element_text(family = "Arial", angle = 45, hjust = 1),
  axis.text.x = element_text(color = "black"),
  axis.text.y = element_text(color = "black"),
  axis.title = element_text(size = 12, face = "bold"),
  #axis.title.y = element_text(family = "Arial", margin = margin(t = 0, r = 20, b = 0, l = 0)), # not working?
  #axis.title.x = element_text(family = "Arial", margin = margin(t = 0, r = 0, b = 20, l = 0)),
  plot.title = element_text(size = 15, hjust = 0),
  legend.key = element_blank(),
  axis.ticks = element_blank())

# Specify geom to update, and list attibutes you want to change appearance of
update_geom_defaults("line", list(size = 1))



# Functions ---------------------------------------------------------------
## Color gradient function ================================================
function_gradient_blue <- colorRampPalette(c(bb_blue_light, bb_blue_dark))
## Stationary function ====================================================
function_stationary <- function(data, tcode, date_index = "DATE_QUARTER", ppt = TRUE){
  
  # Store date vaiable
  date_index_c <- data %>% 
    select(date_index)
  
  # Create name variables
  tcode <- tcode %>% 
    clean_names(case = "all_caps")
  
  data <- data %>% 
    clean_names(case = "all_caps")
  
  # Check if variable names of data and tcode match
  if(!all(names(tcode) %in% (data %>% select(-date_index) %>% names()))){
    warning("Names of data and tcode do not match. Only matching columns from data selected")
    
    # Select only matching columns
    names_match <- intersect(names(data), names(tcode))
    data <- data %>% 
      select(date_index, names_match)
  }
  
  # Create help variable for percentage point conversion
  if(ppt) ppt <- 100
  else    ppt <- 1
  
  trans_data <- data %>%
    select(-date_index) %>% 
    imap(function(x, name){
      if(tcode[,name] == 2){
        c(NA, diff(x, lag = 1, differences = 1))
      }
      
      else if(tcode[,name] == 3){
        c(NA, NA, diff(x, lag = 1, differences = 2))
      }
      
      else if(tcode[,name] == 4){
        log(x)
      }
      
      else if(tcode[,name] == 5){
        c(NA, diff(log(x), lag = 1, differences = 1)*ppt)
      }
      
      else if(tcode[,name] == 6){
        c(NA, NA, diff(log(x), lag = 1, differences = 2)*ppt)
      }
      
      else if(tcode[,name] == 7){
        c(NA, diff(x/lag(x)-1, lag = 1, differences = 1)*ppt)
      }
      
      else {
        x
      }
    }) %>%
    as_tibble() %>% 
    bind_cols(date_index_c) %>% 
    select(date_index, everything())
  
  return(trans_data)
  
}
## Stationarity testing function ==========================================
function_stationary_tests <- function(df_input, n_lag = 2){
  
  df <- df_input %>% 
    select(-contains("DATE_QUARTER"))                                      # deselect date column
  
  df_multi <- tibble(.rows = ncol(df)) %>%                                 # create tibble with number of rows being equal to the number of columns in dataframe input
    mutate(VARIABLE = colnames(df)) %>%                                    # create string column with column names as input
    mutate(SERIES = map(df, ~ (c(t(.))))) %>%                              # create column with nested time series of respective variable
    
    # conduct the stationary tests and create column with respective results
    mutate(ADF_MODEL_NONE = map(SERIES, function(v) ur.df(ts(v[!is.na(v)]), type = "none", lags = n_lag)),
           ADF_MODEL_DRIFT = map(SERIES, function(v) ur.df(ts(v[!is.na(v)]), type = "drift", lags = n_lag)),
           ADF_MODEL_TREND1 = map(SERIES, function(v) ur.df(ts(v[!is.na(v)]), type = "trend", lags = n_lag)),
           ADF_MODEL_TREND2 = map(SERIES, function(v) adf.test(ts(v[!is.na(v)]), alternative = "stationary", k = n_lag)),
           KPSS_MODEL = map(SERIES, function(v) kpss.test(ts(v[!is.na(v)]), null = "Level", lshort = TRUE))
    ) %>% 
   
    # extract p-values from the stationarity tests
    mutate(ADF_MODEL_NONE_PVALUE = map(ADF_MODEL_NONE,
                                       ~ { if((.@testreg$coefficients["z.lag.1","t value"] < .@cval["tau1","10pct"])&
                                              (.@testreg$coefficients["z.lag.1","t value"] > .@cval["tau1","5pct"])) {"<0.1"} 
                                         else if ((.@testreg$coefficients["z.lag.1","t value"] < .@cval["tau1","5pct"])&
                                                  (.@testreg$coefficients["z.lag.1","t value"] > .@cval["tau1","1pct"]))  {"<0.05"}
                                         else if ((.@testreg$coefficients["z.lag.1","t value"] < .@cval["tau1","1pct"]))  {"<0.01"}
                                         else {">0.1"}}),
           ADF_MODEL_DRIFT_PVALUE = map(ADF_MODEL_DRIFT,
                                        ~ { if((.@testreg$coefficients["z.lag.1","t value"] < .@cval["tau2","10pct"])&
                                               (.@testreg$coefficients["z.lag.1","t value"] > .@cval["tau2","5pct"])) {"<0.1"} 
                                          else if ((.@testreg$coefficients["z.lag.1","t value"] < .@cval["tau2","5pct"])&
                                                   (.@testreg$coefficients["z.lag.1","t value"] > .@cval["tau2","1pct"]))  {"<0.05"}
                                          else if ((.@testreg$coefficients["z.lag.1","t value"] < .@cval["tau2","1pct"]))  {"<0.01"}
                                          else {">0.1"}}),
           ADF_MODEL_TREND1_PVALUE = map(ADF_MODEL_TREND1,
                                         ~ { if((.@testreg$coefficients["z.lag.1","t value"] < .@cval["tau3","10pct"])&
                                                (.@testreg$coefficients["z.lag.1","t value"] > .@cval["tau3","5pct"])) {"<0.1"} 
                                           else if ((.@testreg$coefficients["z.lag.1","t value"] < .@cval["tau3","5pct"])&
                                                    (.@testreg$coefficients["z.lag.1","t value"] > .@cval["tau3","1pct"]))  {"<0.05"}
                                           else if ((.@testreg$coefficients["z.lag.1","t value"] < .@cval["tau3","1pct"]))  {"<0.01"}
                                           else {">0.1"}}),
           ADF_MODEL_TREND2_PVALUE = map(ADF_MODEL_TREND2, ~ .$p.value),
           KPSS_MODEL_PVALUE = map(KPSS_MODEL, ~ .$p.value),
    
           # check if ur.df(type = "trend") and adf.test() yield the same results (based on package description they should do so)
           TREND_CHECK = map(ADF_MODEL_TREND2_PVALUE,
                             ~ { if((. < 0.1)&
                                    (. > 0.05)) {"<0.1"} 
                               else if ((. < 0.05)&
                                        (. > 0.01))  {"<0.05"}
                               else if (. <= 0.01)  {"<0.01"}
                               else {">0.1"}})) %>% 
    # unnest p-values
    unnest(ADF_MODEL_NONE_PVALUE,
           ADF_MODEL_DRIFT_PVALUE,
           ADF_MODEL_TREND1_PVALUE,
           ADF_MODEL_TREND2_PVALUE,
           KPSS_MODEL_PVALUE) %>% 
    # create a stationarity flag
    mutate(ADF_NONE = ifelse(ADF_MODEL_NONE_PVALUE %in% c("<0.1", ">0.1"), "non-stationary", "stationary"),
           ADF_DRIFT = ifelse(ADF_MODEL_DRIFT_PVALUE %in% c("<0.1", ">0.1"), "non-stationary", "stationary"),
           ADF_TREND1 = ifelse(ADF_MODEL_TREND1_PVALUE %in% c("<0.1", ">0.1"), "non-stationary", "stationary"),
           ADF_TREND2 = ifelse(ADF_MODEL_TREND2_PVALUE > 0.05, "non-stationary", "stationary"),
           KPSS = ifelse(KPSS_MODEL_PVALUE < 0.05, "non-stationary", "stationary"))
  
  
                         
                         # Note that for the Dickey Fuller Test the model is reformulated as follows:
                         # Y_t = b_0 + b_1*Y_t-1 + u_t =>
                         # d Y_t = b_0 + (b_1 - 1)*Y_t-1 + u_t
                         # H_0: b_1 - 1 = 0
                         # if coefficient of Y_t-1 is significantly different from 0 (rejecting H_0), the series has no unit root (stationary).
                         # if coefficient of Y_t-1 is not significantly different from 0 (not rejecting H_0), the series has a unit root (non stationary)
                          
                         # Including more lags in the model the augmented Dickey Fuller Test looks as follows:
                         # Y_t = b_0 + b_1*Y_t-1 + b_2*Y_t-2 + u_t =>
                         # d Y_t = b_0 + (b_1 + b_2 - 1)*Y_t-1 - b_2*d Y_t-1 + u_t
                         # if coefficient of Y_t-1 is significantly different from 0 (rejecting H_0), the series has no unit root (stationary).
                         # if coefficient of Y_t-1 is not significantly different from 0 (not rejecting H_0), the series has a unit root (non stationary)                         
                         
                         # Y_t = b_0 + b_1*Y_t-1 + gamma*t + u_t =>
                         # d Y_t = b_0 + (b_1 - 1)*Y_t-1 + gamma*t + u_t
                         # H_0: b_1 - 1 = 0
                         # The null hypothesis is the same but the testing regression is different (now including time trend)
                         # if coefficient of Y_t-1 is significantly different from 0 (rejecting H_0), the series has no unit root (trend stationary).
                         # if coefficient of Y_t-1 is not significantly different from 0 (not rejecting H_0), the series has a unit root (non stationary)
                         

  


  return(df_multi)
}


