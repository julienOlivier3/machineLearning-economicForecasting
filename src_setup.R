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
              "beepr"                                                      # sound indicating that code execution has finished
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
  panel.grid.major.x = element_blank(),
  panel.border = element_rect(fill = NA, color = "grey20"),
  axis.text.x = element_text(family = "Arial", angle = 45, hjust = 1),
  axis.text.y = element_text(family = "Arial"),
  axis.title = element_text(family = "Arial"),
  plot.title = element_text(size = 15, hjust = 0, family = "Arial"),
  legend.key = element_blank())

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
  if(!all(names(tcode) %in% names(data))){
    warning("Names of data and tcode do not match. Only matching columns from data selected")
    
    # Select only matching columns
    names_match <- intersect(names(data), names(tcode))
    data <- data %>% 
      select(names_match)
  }
  
  # Create help variable for percentage point conversion
  if(ppt) ppt <- 100
  else    ppt <- 1
  
  trans_data <- data %>%
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
        (x/diff(x, lag = 1, differences = 1)-1)*ppt
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
function_stationary_tests <- function(df){
  p <- ncol(df)
  df_multi <- data.frame(VAR = names(df),
                         #DF_PVALUE = sapply(df, function(v) ur.df(ts(v[!is.na(v)]), type = "drift", selectlags = "BIC"))  # no direct p-value (see below)
                         ADF_PVALUE = sapply(df, function(v) adf.test(ts(v[!is.na(v)]), alternative = "stationary")$p.value),
                         KPSS_PVALUE = sapply(df, function(v) kpss.test(ts(v[!is.na(v)]), null = "Level")$p.value)
  )
  df_multi$ADF <- df_multi$ADF_PVALUE < 0.05
  df_multi$KPSS <- df_multi$KPSS_PVALUE > 0.05
  row.names(df_multi) <- c()
  return(df_multi)
}
