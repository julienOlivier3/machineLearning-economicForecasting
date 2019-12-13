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
              "scales",                                                    # for nice formatting options
              "imputeTS",                                                  # imputation methods for time series data
              "lubridate",                                                 # simple year, month, ... functions
              "zoo",                                                       # another time series package
              "forecast",                                                  # another time series package
              "tseries",                                                   # another time series package
              "timetk",                                                    # yet another time series package (suitable for time series machine learning)
              "multDM",                                                    # Diebold-Mariano test for equal predictive accuracy of two forecasting models
              "dynlm",                                                     # estimating time series models
              "lmtest",                                                    # test statistics for parameter estimates
              "urca",                                                      # for stationarity testing
              "portes",                                                    # univariate and multivariate portmanteau test statistics
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
              "pls",                                                       # machine learning: principal component analysis
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
              "ggthemes",                                                  # additional ggplot themes (e.g. economist theme)
              "corrplot",                                                  # correlation plot
              "ggcorrplot",                                                # correlation plot
              "tikzDevice"                                                 # for latex visualiztions
              )

function_package(packages)



# Colors ------------------------------------------------------------------
bb_blue_dark <- rgb(0, 69, 125, maxColorValue = 255)
bb_blue_medium <- rgb(102, 144, 177, maxColorValue = 255)
bb_blue_light <- rgb(204, 218, 229, maxColorValue = 255)

bb_red_dark <- rgb(230, 68, 79, maxColorValue = 255)
bb_red_medium <- rgb(235, 105, 114, maxColorValue = 255)
bb_red_light <- rgb(240, 143, 149, maxColorValue = 255)

bb_green_dark <- rgb(151, 191, 13, maxColorValue = 255)
bb_green_medium <- rgb(172, 204, 61, maxColorValue = 255)
bb_green_light <- rgb(193, 216, 110, maxColorValue = 255)

ml_green_dark <- "seagreen4"
ml_green_medium <- "seagreen3"
ml_green_light <- "seagreen2"
# ml_green_dark <- "aquamarine4"
# ml_green_medium <- "aquamarine3"
# ml_green_light <- "aquamarine2"

function_gradient_blue <- colorRampPalette(c(bb_blue_light, bb_blue_dark))
function_gradient_green <- colorRampPalette(c(ml_green_light, ml_green_dark))
function_gradient_redTOgreen <- colorRampPalette(c(bb_red_dark, ml_green_dark))
function_gradient_redTOwhiteTOgreen <- colorRampPalette(c(bb_red_dark, "white", ml_green_dark))
function_gradient_redTOblueTOgreen <- colorRampPalette(c(bb_red_dark, bb_blue_dark, ml_green_dark))



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
  #axis.title = element_text(size = 12, face = "bold", margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt")),  # not working?
  axis.title.y = element_text(size = 10, face = "bold", vjust = -.1),
  axis.title.x = element_text(size = 10, face = "bold", vjust = -.1),
  plot.title = element_text(size = 12, hjust = 0),
  legend.key = element_blank(),
  axis.ticks = element_blank(),
  strip.text.x = element_text(size = 10, color = "white", face = "bold"),    # changes facet labels
  strip.text.y = element_text(size = 10, color = "white", face = "bold"),
  #strip.background = element_rect(color="black", fill="grey", size=1, linetype="solid")
  strip.background = element_rect(fill="grey"),
  #plot.margin = unit(c(1,1,1,1), "cm")
  )

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
function_stationary_tests <- function(df_input){
  
  df <- df_input %>% 
    select(-contains("DATE_QUARTER"))                                      # deselect date column
  
  df_multi <- tibble(.rows = ncol(df)) %>%                                 # create tibble with number of rows being equal to the number of columns in dataframe input
    mutate(VARIABLE = colnames(df)) %>%                                    # create string column with column names as input
    mutate(SERIES = map(df, ~ (c(t(.))))) %>%                              # create column with nested time series of respective variable
    
    # conduct the stationary tests and create column with respective results
    mutate(
      # RW vs stationary AR w.o. intercept
      ADF_MODEL_NONE = map(SERIES, function(v) ur.df(ts(v[!is.na(v)]), type = "none", selectlags = "BIC")),
      
      # RW vs stationary AR w. intercept (F-test)
      ADF_MODEL_DRIFT = map(SERIES, function(v) ur.df(ts(v[!is.na(v)]), type = "drift", selectlags = "BIC")),
      
      # RW with drift (stochastic trend) against trend stationarity (deterministic trend) (F-test)
      ADF_MODEL_TREND1 = map(SERIES, function(v) ur.df(ts(v[!is.na(v)]), type = "trend", selectlags = "BIC")),
      
      # Unit root against trend stationarity (deterministic trend) 
      ADF_MODEL_TREND2 = map(SERIES, function(v) adf.test(ts(v[!is.na(v)]), alternative = "stationary")),
      
      # 
      KPSS_MODEL_LEVEL = map(SERIES, function(v) kpss.test(ts(v[!is.na(v)]), null = "Level", lshort = TRUE)),
      
      #
      KPSS_MODEL_TREND = map(SERIES, function(v) kpss.test(ts(v[!is.na(v)]), null = "Trend", lshort = TRUE))
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
                                        ~ { if((.@teststat["statistic","phi1"] > .@cval["phi1","10pct"])&
                                               (.@teststat["statistic","phi1"] < .@cval["phi1","5pct"])) {"<0.1"} 
                                          else if ((.@teststat["statistic","phi1"] > .@cval["phi1","5pct"])&
                                                   (.@teststat["statistic","phi1"] < .@cval["phi1","1pct"]))  {"<0.05"}
                                          else if ((.@teststat["statistic","phi1"] > .@cval["phi1","1pct"]))  {"<0.01"}
                                          else {">0.1"}}),
           ADF_MODEL_TREND1_PVALUE = map(ADF_MODEL_TREND1,
                                         ~ { if((.@teststat["statistic","phi3"] > .@cval["phi3","10pct"])&
                                                (.@teststat["statistic","phi3"] < .@cval["phi3","5pct"])) {"<0.1"} 
                                           else if ((.@teststat["statistic","phi3"] > .@cval["phi3","5pct"])&
                                                    (.@teststat["statistic","phi3"] < .@cval["phi3","1pct"]))  {"<0.05"}
                                           else if ((.@teststat["statistic","phi3"] > .@cval["phi3","1pct"]))  {"<0.01"}
                                           else {">0.1"}}),
           ADF_MODEL_TREND2_PVALUE = map(ADF_MODEL_TREND2, ~ .$p.value),
           KPSS_MODEL_LEVEL_PVALUE = map(KPSS_MODEL_LEVEL, ~ .$p.value),
           KPSS_MODEL_TREND_PVALUE = map(KPSS_MODEL_TREND, ~ .$p.value),
    
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
           KPSS_MODEL_LEVEL_PVALUE,
           KPSS_MODEL_TREND_PVALUE) %>% 
    # create a stationarity flag
    mutate(ADF_NONE = ifelse(ADF_MODEL_NONE_PVALUE %in% c("<0.1", ">0.1"), "non-stationary", "stationary"),
           ADF_DRIFT = ifelse(ADF_MODEL_DRIFT_PVALUE %in% c("<0.1", ">0.1"), "non-stationary", "stationary"),
           ADF_TREND1 = ifelse(ADF_MODEL_TREND1_PVALUE %in% c("<0.1", ">0.1"), "non-stationary", "stationary"),
           ADF_TREND2 = ifelse(ADF_MODEL_TREND2_PVALUE > 0.05, "non-stationary", "stationary"),
           KPSS_LEVEL = ifelse(KPSS_MODEL_LEVEL_PVALUE < 0.05, "non-stationary", "stationary"),
           KPSS_TREND = ifelse(KPSS_MODEL_TREND_PVALUE < 0.05, "non-stationary", "stationary"))
  
  
                         
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



## Stepwise Granger causality function ====================================

function_granger_stepwise <- function(df, target = "REAL_GDP_GROWTH", time_var = "DATE_QUARTER", lags = final_p){
  
  # Extract variable names included in the VAR model 
  # (except the name of the target which is provided as function argument)  
  var_names <- df %>% 
    select(-target) %>% 
    select(-time_var) %>% 
    names()
  
  # Define dataframe as dataframe of type ts 
  # (required for later estimation via dynlm())
  df_ts <- df %>%
    ts(start = c(lubridate::year(df %>% select(time_var) %>% filter(row.names(.)==1) %>% .[[1]]), 
                 lubridate::quarter(df %>% select(time_var) %>% filter(row.names(.)==1) %>% .[[1]])),
       end = c(lubridate::year(df %>% select(time_var) %>% filter(row.names(.)==nrow(.)) %>% .[[1]]), 
               lubridate::quarter(df %>% select(time_var) %>% filter(row.names(.)==nrow(.)) %>% .[[1]])),
       frequency = 4)
  
  
  # Create formula object related only to the target variable
  formel1 <- paste(target, "~", "L(", target, ",", "1:", lags,")")
  
  # Create character vector of formula objects for each additional variable 
  # in the vAR model
  formel2 <- NULL
  for (i in 1:length(var_names)){
    formel2[i] <- paste("+", "L(", var_names[i], ",", "1:", lags,")")
  }
  
  
  # Estimate the target variable model including all additional
  # variables
  full_model <- df_ts %>%
    dynlm(formula = eval(parse(text = paste(formel1, paste(formel2, collapse = "")))))
  
  # Initialize empty objects
  reduced_model <- vector(mode = "list", length = length(var_names))
  ANOVA <- vector(mode = "list", length = length(var_names))
  P_VALUE <- NULL
  
  # Estimate reduced models where in each step of the loop one
  # of the additional varibales is left out from the model 
  for (i in 1:length(var_names)){
    reduced_model[[i]] <- df_ts %>%
      dynlm(formula = eval(parse(text = paste(formel1, paste(formel2[-i], collapse = "")))))
  
  # Calculate F-test and extract p-value indicating whether the inclusion
  # of the respective variable leads to a significant improvement
  # in R^2  
    ANOVA[[i]] <- anova(full_model, reduced_model[[i]])
    P_VALUE[i] <- anova(full_model, reduced_model[[i]])$`Pr(>F)`[2]
  }
  
  # Create final dataframe with column VARIABLE indicating which model
  # has been left out in the reduced model (or alternatively which has
  # been included in the full model)
  res <- var_names %>% 
    enframe(name = NULL, value = "VARIABLE") %>% 
    mutate(ANOVA = ANOVA) %>% 
    mutate(PVALUE = P_VALUE)  
     
  
  return(res)
}
## Principal Component regression cross validation function ===============




function_pcr_cv <- function(data_training,                                                             # dataframe needs to include time stamp variable and target variable
                            max_PC = 10,                                                               # maximum number of principal components taken into consideration
                            final_p_cv = 1,                                                            # number of lags in the vAR model
                            go_back = 51,                                                              # number of indices to go back in the training data to start the first window
                            horizon = 1,                                                               # number of observations in the test set (default = 1, since we are doing one-step ahead forecasts)
                            skip = 5,                                                                  # number of training/validation sets which should be skipped (choose one of: 5, 10 & 25; the higher the less CVs)
                            forecasting_intervals = 95){
  
  # Define index where first window starts
  initial.window <- nrow(data_training) - go_back
  
  # Create matrix which captures forecast errors
  FAVAR_results_cv <- matrix(nrow = (nrow(data_training)-initial.window-1)/skip, ncol = 2+max_PC*2) %>%
    as_tibble() 
  
  colnames(FAVAR_results_cv) <- c("DATE_QUARTER", 
                                  "TRUE_VALUE", 
                                  paste(rep(paste0("PC", 1:max_PC), each=2), 
                                        rep(c("FORECAST", "ERROR"), times=max_PC),
                                        sep = "_"))
  
  FAVAR_results_cv <- FAVAR_results_cv %>% 
    mutate_at(.vars = "DATE_QUARTER", .funs = as.Date)
  
  # FAVAR_results_cv <- matrix(nrow = (nrow(data_FAVAR)-initial.window-1)/skip, ncol = max_PC) %>% 
  #   as_tibble()
  # 
  # colnames(FAVAR_results_cv) <- c(paste0("PC", 1:max_PC))
  
  
  
  # Start loop for growing window cv
  for (i in 1:nrow(FAVAR_results_cv)){
    
    # Filter data
    data_training_cv <- data_training %>% 
      filter(row.names(.)%in% 1:(initial.window+skip*i))
    
    
    # Extract DATE_QUARTER
    FAVAR_results_cv[i,1] <- data_training %>% 
      filter(row.names(.)==initial.window+skip*i+1) %>% 
      select(DATE_QUARTER) %>% 
      mutate(DATE_QUARTER=as.Date(DATE_QUARTER, format="YYYY qq"))
    
    
    
    # Select feature space
    feature_space_cv <- data_training_cv %>% 
      select(-contains("REAL_GDP_GROWTH")) %>%                                 # drop target
      select(-"DATE_QUARTER") 
    
    
    # Conduct PCA
    pca_cv <- prcomp(feature_space_cv,                                               # Note there are min(T-1,p) distinct principal components 
                     center = TRUE,  
                     scale. = TRUE, 
                     rank. = max_PC)
    
    
    # Extract principal component scores
    pca_scores_cv <- pca_cv$x %>% 
      as_tibble()
    
    
    # Extract true value
    true_cv <- data_training %>% 
      filter(row.names(.) == initial.window+skip*i+1) %>% 
      select(REAL_GDP_GROWTH)
    
    FAVAR_results_cv[i,2] <- data_training %>% 
      filter(row.names(.)==initial.window+skip*i+1) %>% 
      select(REAL_GDP_GROWTH)
    
    
    # Start loop for different number of factors
    for (factor_n_cv in 1:max_PC){
      
      
      
      # Create dataset for model
      data_FAVAR_cv <- data_training_cv %>% 
        select(-c(colnames(feature_space_cv))) %>%                                  # drop all variables which have been considered for PCA
        bind_cols(pca_scores_cv %>% select(1:factor_n_cv))                             # bind the number of factors specified by factor_n with their respective scores
      
      model_FAVAR_cv <- data_FAVAR_cv %>%
        select(-DATE_QUARTER) %>% 
        as.ts(.) %>% 
        VAR(p = final_p_cv, type = "const")
      
      pred_FAVAR_cv <- forecast(model_FAVAR_cv, h = horizon, 
                                level = forecasting_intervals)
      
      
      FAVAR_results_cv[i,2*factor_n_cv+1] <- pred_FAVAR_cv$forecast$REAL_GDP_GROWTH$mean[1]
      
      FAVAR_results_cv[i,2*factor_n_cv+2] <- pred_FAVAR_cv$forecast$REAL_GDP_GROWTH$mean[1] - true_cv
      
      
    }
  } 
  return(FAVAR_results_cv)
} 
## Spread multiple columns function =======================================
spread_n <- function(df, key, value) {
  # quote key
  keyq <- rlang::enquo(key)
  # break value vector into quotes
  valueq <- rlang::enquo(value)
  s <- rlang::quos(!!valueq)
  df %>% gather(variable, value, !!!s) %>%
    unite(temp, !!keyq, variable) %>%
    spread(temp, value)
}


## Extract results from resample object ===================================


help_function <- function(df, id_name, flag_se, horizon){
  df<-df
  df %>%
    .$pred %>%
    .$data %>%
    as_tibble() %>% 
    filter(set == "test") %>% 
    rename(TRUE_VALUE=truth,
           MEAN=response) %>% 
    { if(flag_se)
      mutate(.,LOWER=MEAN-se,
             UPPER=MEAN+se)
      else 
        mutate(.,LOWER=NA,
               UPPER=NA)
    } %>%
    { if(horizon == "Q")
      mutate(.,PERIODS_AHEAD = 1)
      else 
        mutate(.,PERIODS_AHEAD = 4)
    } %>%
    { if(horizon == "Q")
      mutate(.,TRAINING_END=unique(ECO_Q$TRAINING_END),
             FORECAST_PERIOD=unique(ECO_Q$FORECAST_PERIOD))
      else 
        mutate(.,TRAINING_END=unique(ECO_Y$TRAINING_END),
               FORECAST_PERIOD=unique(ECO_Y$FORECAST_PERIOD))
    } %>%
    mutate(.,MODEL_ID=id_name,
           ERROR=TRUE_VALUE-MEAN
    ) %>% 
    select(c(colnames(ECO_Q)))
}


## Ggplot helpers =========================================================

reorder_within <- function(x, by, within, fun = mean, sep = "___", ...) {
  new_x <- paste(x, within, sep = sep)
  stats::reorder(new_x, by, FUN = fun)
}

scale_x_reordered <- function(..., sep = "___") {
  reg <- paste0(sep, ".+$")
  ggplot2::scale_x_discrete(labels = function(x) gsub(reg, "", x), ...)
}
