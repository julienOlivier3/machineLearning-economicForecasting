# FAVAR
# Steering ----------------------------------------------------------------
setwd("J:\\Studium\\Master\\Masterthesis")

# Training test split
#training_start <- "1987 Q2"       # Set the first quarter of training data
training_end <- "2007 Q1"         # Set the last quarter of training data, the subsequent quarter is the first quarter to be forecasted (Note: Go to "2007 Q1" once the latest observation is "2019 Q2")
forecasting_periods <- 1          # Set how many periods ahead shall be forecasted (Note if forecasting_periods = 1: features will be lagged with lag order 1:max_lag, if forecasting_periods = 2: features will be lagged with lag order 2:max_lag, and so on)
#n_ahead <- 4                      # Set how many periods ahead shall be forecasted (Note if n_ahead = 1: features will be lagged with lag order 1:max_lag, if n_ahead = 2: features will be lagged with lag order 2:max_lag, and so on)
max_lag <- 1                      # Set how many lags of each variable shall be included in the feature space


# FAVAR specific
selective <- TRUE                # set true if only selected features shall be considered (refers to the variables considered for PCA in Stock and Watson (2012))
factor_n <- 2                     # define ultimate number of factors to consider

ic <- "bic"                       # select information criteria used for order selection ("aic", "bic" or "aicc")
max_p <- 10                       # select maximum number of AR terms 
lag_max <- 10                     # maximum number of lags considered in ACF plots
final_p <- 3                      # define AR parameters of final model


forecasting_intervals <- 95       # Set forecasting confidence intervals
#testing_end <- "2016 Q4"          # Set the last period in which test data starts  
# in the iterative process of model estimation 
# (e.g. training_end = "2007 Q1", testing_end = "2007 Q2",
# forecasting_periods = 10 forecasts 10 periods ahead with
# only based on training data training_start ~ training_end.
# It does not extend the training sample iteratively.)


# Sourcing ----------------------------------------------------------------
source(file = file.path(getwd(), "Code", "src_setup.R"))

# Read data ---------------------------------------------------------------
load(file = file.path(getwd(), "Results", "tidy_data", "tidy_yx_yq_stat.RData"))
load(file = file.path(getwd(), "Results", "tidy_data", "tidy_yx_yq.RData"))
feature_info <- as_tibble(read.xlsx(file = file.path(getwd(), "Results", "table_features.xlsx"), 
                               sheetIndex = 1, 
                               startRow = 1, 
                               header = TRUE)) %>% 
  mutate_if(is.factor, as.character)  %>%                                  # characters are read as factors -> convert respectively
  mutate(MNEMONIC_FRED_CAPS = make_clean_names(MNEMONIC_FRED, 
                                               case = "all_caps"))         # capitalize all variable names


# Filter data -------------------------------------------------------------

# Drop level variables
tidy_yx_yq_stat <- tidy_yx_yq_stat %>% 
  select(-c(NOMINAL_GDP,                                                   # drop all GDP figures except target variable
            REAL_GDP,
            REAL_GDP_GROWTH_A,
            GDPC1,
            GDPC1_GROWTH, 
            GDPC1_GROWTH_ANNUALIZED)) %>% 
  { if(selective)                                                          # compromise feature space if selective = TRUE
    select(., DATE_QUARTER, REAL_GDP_GROWTH, intersect(feature_info %>%    # then only take variables which have been considered in Stock and Watson's (2012) analysis ...
                                               filter(FACTORS==1) %>% 
                                               select(MNEMONIC_FRED_CAPS) %>% 
                                               as_vector(),
                                               colnames(.)))               # ... given that they have not been dropped in the data preprocessing part
    else . }


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


# Select feature space for PCA
feature_space <- data_training %>% 
  select(-contains("REAL_GDP_GROWTH")) %>%                                 # drop target
  select(-"DATE_QUARTER")                                                  # drop time stamp


# Conduct overall PCA -----------------------------------------------------

## Plot correlation matrix ================================================
# Reshape training data (variables as rows) and add grouping variable
data_training_t <- data_training %>% 
  select(-c(DATE_QUARTER, REAL_GDP_GROWTH)) %>% 
  t() %>% 
  as_tibble(rownames = "VARIABLE") %>% 
  left_join(feature_info %>% select(MNEMONIC_FRED_CAPS, GROUP), 
            by = c("VARIABLE" = "MNEMONIC_FRED_CAPS")) %>% 
  select(VARIABLE, GROUP, everything())

# Group by the grouping variable and count how often each group appears
data_group <- data_training_t %>% 
  select(GROUP) %>% 
  group_by(GROUP) %>% 
  summarise(COUNTS=n()) %>% 
  ungroup()

# Create axis id for the horizontal and vertical lines
id <- cumsum(data_group[order(factor(data_group$GROUP, levels = unique(data_training_t$GROUP))),"COUNTS"]) %>% 
  c() %>% 
  .$COUNTS

# Create label variable (the group name)
id_label <- data_group[order(factor(data_group$GROUP, levels = unique(data_training_t$GROUP))),] %>% 
  select(GROUP) %>% 
  c() %>% 
  .$GROUP

# Create label id which determines where the lable is plotted  
id2 <- c(id[1]/2,id[-length(id)]+diff(id)/2)+0.5
  
# Calculate correlation matrix
corr_matrix <- cor(data_training %>% 
                     select(-c(DATE_QUARTER, REAL_GDP_GROWTH)) %>% 
                     rename_at(.vars = vars(contains("_")),
                               .funs = funs(gsub("_", "\\\\_", .))))

# Calculate p-values for the correlation coefficients
corr_p <- cor_pmat(corr_matrix)

#Create correlation plot (ggplot style)
tikz("plot_corrM1.tex",
     height = 6,
     width = 6)
corr_plot <- ggcorrplot(corr = corr_matrix,
                        method = "square",
                        type = "lower",
                        ggtheme = theme_thesis,
                        title = "",
                        show.legend = TRUE,
                        legend.title = "Corr",
                        show.diag = FALSE,
                        p.mat = corr_p,
                        colors = c(ml_green_dark, "white", bb_red_dark),
                        insig = "pch",
                        pch = 4,
                        pch.col = "darkgrey",
                        pch.cex = 1,
                        hc.order = FALSE) +                                           # this keeps order of variables in the plot
  theme(axis.text.x = element_text(color = "black", size = 5),
        axis.text.y = element_text(color = "black", size = 5))

#Add labels and lines to correlation plot
corr_plot +
  annotate("segment", x=rep(-50, length(id)), y=id+0.5, xend=id+0.5, yend=id+0.5,
           col="black") +
  annotate("segment", x=id+0.5, y=id+0.5, xend=dim(corr_matrix)[1], yend=id+0.5,
           col="black", lty=2) +
  annotate("segment", x=id+0.5, y=rep(0, length(id)), xend=id+0.5, yend=id+0.5,
           col="black") +
  annotate("text", x=rep(-49, length(id)), y=id2,
           label=id_label, size = 2, hjust = "left",
           fontface = "bold") +
  # coord_fixed(ratio = 1, xlim = c(1,dim(corr_matrix)[1]), ylim = c(1,dim(corr_matrix)[1]), expand = TRUE,
  #             clip = "off") +
  theme(plot.margin = unit(c(0.25,0.25,0.25,4.5), "cm"))
dev.off()


#Create correlation plot (nice style)
# tikz("plot_corrM.tex",
#      height = 4,
#      width = 6)
# 
# corrplot(corr_matrix,
#          diag = FALSE,
#          method = "square",
#          type = "lower",
#          order = "FPC",
#          tl.pos	= "ld",
#          tl.col = "black",
#          tl.cex = 0.4,
#          tl.srt = 45,
#          p.mat = corr_p,
#          sig.level = 0.05,
#          insig = "pch",
#          pch = "X",
#          pch.cex = 0.5,
#          col = function_gradient_redTOwhiteTOgreen(100)
#          )
# 
# dev.off()



## Principal component analysis ===========================================

# Conduct principal component analysis on training feature space
# Note there are min(T-1,p) distinct principal components 
pca <- prcomp(feature_space,                                               # Note there are min(T-1,p) distinct principal components 
              center = TRUE,  
              scale. = TRUE, 
              #rank. = 3
              )

# Extract principal component scores
pca_scores <- pca$x %>% 
  as_tibble()


# Extract loading matrix
pca_loadings <- pca$rotation %>% 
  as_tibble(rownames = "VARIABLE") %>% 
  left_join(feature_info %>% select(GROUP, MNEMONIC_FRED_CAPS), 
            by = c("VARIABLE" = "MNEMONIC_FRED_CAPS")) %>% 
  select(VARIABLE, GROUP, everything())








# Determine optimal number of PCs -----------------------------------------

## Proportion of varinace explainend ======================================

# Calculate proportion of variance explained (PVE)
pca_pve <- tibble(PRINCIPAL_COMPONENT = colnames(pca_scores)) %>% 
  mutate(PVE = pca$sdev^2/sum(pca$sdev^2),
         CPVE = cumsum(PVE))


# Visualize PVE with respect to the number of PCs (Scree plot)
tikz("plot_scree.tex",
     height = 3,
     width = 6)
pca_pve %>% 
  mutate(PC_NUMBER = 1:nrow(.)) %>% 
  filter(PC_NUMBER %in% 1:10) %>% 
  ggplot() +
  geom_line(aes(x=PC_NUMBER, y = PVE, group = 1), color = ml_green_dark) + 
  geom_point(aes(x=PC_NUMBER, y = PVE, group = 1), color = ml_green_dark, size = 2, alpha = 0.2) +
  # geom_line(aes(x=PC_NUMBER, y = CPVE, group = 1), color = bb_blue_medium) + 
  # geom_point(aes(x=PC_NUMBER, y = CPVE, group = 1), color = bb_blue_medium) +
  theme_thesis +
  theme(panel.grid.major.x = element_line(color = "grey90", size = 0.5)) +
  scale_x_continuous(breaks = 1:10, labels = 1:10) +
  ylab("Proportion of Variance Explained") +
  xlab("Number of Principal Components")
dev.off()



## Cross validation =======================================================

FAVAR_results_cv <- function_pcr_cv(data_training = data_training,
                                    max_PC = 10,                           # maximum number of principal components taken into consideration
                                    final_p_cv = 1,                        # number of lags in the vAR model
                                    go_back = 51,                          # number of indices to go back in the training data to start the first window
                                    horizon = 1,                           # number of observations in the test set (default = 1, since we are doing one-step ahead forecasts)
                                    skip = 5,                              # number of training/validation sets which should be skipped (choose one of: 5, 10 & 25; the higher the less CVs)
                                    forecasting_intervals = 95)            # interval forecast band


# Calculate aggregate error results
FAVAR_results_cv %>% 
  mutate(DATE_QUARTER = as.yearqtr(DATE_QUARTER)) %>%                      # redefine time stamp variable as yearqtr 
  select(contains("ERROR")) %>%                                            # only select columns which entail error information
  melt(value.name = "ERROR") %>%                                           # make df long
  group_by(variable) %>%                                                   # group by the number of PCs
  summarise(MSE = mean(ERROR^2),                                           # calculate mean squared error (MSE)
            RMSE = sqrt(mean(ERROR^2)))



# Create dataset for model with optimal number of factors
data_FAVAR <- data_training %>% 
  select(-c(colnames(feature_space))) %>%                                  # drop all variables which have been considered for PCA
  bind_cols(pca_scores %>% select(1:factor_n))                             # bind the number of factors specified by factor_n with their respective scores


## Visualize loadings =====================================================
# Visualizations via Loading
pca_loadings_relevant <- pca_loadings %>%
  mutate(VARIABLE = map(VARIABLE, ~ gsub("_", "\\\\_", .))) %>% 
  unnest() %>% 
  select(VARIABLE, GROUP, everything()) %>% 
  mutate(VARIABLE = factor(VARIABLE, levels = VARIABLE),
         GROUP = factor(GROUP, levels = unique(GROUP))) %>%
  select(1:(factor_n+2)) %>%
  melt(value.name = "LOADING") %>%
  as_tibble() %>%
  rename(PC=variable)

tikz("plot_load.tex",
     height = 6,
     width = 6)
pca_loadings_relevant %>%
  #filter(PC == "PC1") %>%
  ggplot()+
  geom_bar(aes(x = VARIABLE, y = LOADING, fill = GROUP),
           color = "white", 
           stat = "identity",
           width = 1
           ) +
  theme_thesis +
  theme(axis.text.x = element_text(angle = 45,
                                   hjust = 1,
                                   #vjust= 0.25,
                                   size = 3),
        panel.grid.major.x = element_line(color = "grey90", size = 0.5),
        legend.position="top",
        legend.text=element_text(size=6)) +
  facet_wrap(~PC, nrow = 2) +
  ylab("Loadings") +
  xlab("Features") +
  #scale_fill_viridis(discrete = TRUE)
  scale_fill_manual(values = function_gradient_redTOblueTOgreen(12)) +
  guides(fill=guide_legend(title = "Group",
                           title.position = "left",
                           nrow=3,
                           byrow=TRUE))

dev.off()




















# Visualizations via R^2
data_R2 <- pca_scores %>%
  select(1:factor_n) %>%
  bind_cols(feature_space)

data_R2_PC1 <- tibble("VARIABLE" = colnames(feature_space)) %>%
  mutate(DATA = map(VARIABLE, function(x) data_R2[,c("PC1",x)]),
         LM = map(DATA, function(x) lm(PC1~., data = x)),
         R2 = map(LM, function(x) summary(x)$r.squared)) %>%
  select(VARIABLE, R2) %>%
  unnest()

data_R2_PC1 %>%
  ggplot()+
  geom_bar(aes(x = VARIABLE, y = R2), stat = "identity") +
  theme_thesis +
  theme(axis.text.x = element_text(angle = 90,
                                   hjust = 1,
                                   vjust= 0.25,
                                   size = 5),
        plot.margin=unit(c(1,1,4,1.2),"cm"),
        panel.grid.major.x = element_line(color = "grey90", size = 0.5),
        axis.title.x = element_blank()) +
  annotate("segment", x=id+0.5, y=rep(-0.5, length(id)), xend=id+0.5, yend=rep(0.8, length(id)),
           col="black") +
  annotate("text", x=id2, y=rep(-0.49, length(id)),
           label=id_label, size = 2, hjust = "bottom",
           fontface = "bold", angle = 90) +
  coord_cartesian(xlim = c(0,dim(data_R2_PC1)[1]),
                  ylim = c(min(data_R2_PC1 %>% select(R2)),
                           max(data_R2_PC1 %>% select(R2))),
                  expand = TRUE,
                  clip = "off")

data_R2_PC2 <- tibble("VARIABLE" = colnames(feature_space)) %>%
  mutate(DATA = map(VARIABLE, function(x) data_R2[,c("PC2",x)]),
         LM = map(DATA, function(x) lm(PC2~., data = x)),
         R2 = map(LM, function(x) summary(x)$r.squared)) %>%
  select(VARIABLE, R2) %>%
  unnest()

data_R2_PC2 %>%
  ggplot()+
  geom_bar(aes(x = VARIABLE, y = R2), stat = "identity") +
  theme_thesis +
  theme(axis.text.x = element_text(angle = 90,
                                   hjust = 1,
                                   vjust= 0.25,
                                   size = 5),
        plot.margin=unit(c(1,1,4,1.2),"cm"),
        panel.grid.major.x = element_line(color = "grey90", size = 0.5),
        axis.title.x = element_blank()) +
  annotate("segment", x=id+0.5, y=rep(-0.2, length(id)), xend=id+0.5, yend=rep(0.35, length(id)),
           col="black") +
  annotate("text", x=id2, y=rep(-0.19, length(id)),
           label=id_label, size = 2, hjust = "bottom",
           fontface = "bold", angle = 90) +
  coord_cartesian(xlim = c(0,dim(data_R2_PC1)[1]),
                  ylim = c(min(data_R2_PC2 %>% select(R2)),
                           max(data_R2_PC2 %>% select(R2))),
                  expand = TRUE,
                  clip = "off")

# Model selection ---------------------------------------------------------
# Plot non-stationary time series
data_FAVAR %>% 
  melt(id.vars = "DATE_QUARTER", variable.name = "SERIES") %>% 
  as_tibble() %>% 
  ggplot() +
  geom_line(aes(x = DATE_QUARTER, y = value, color = SERIES, linetype = SERIES)) +
  scale_color_viridis(discrete = TRUE) +
  theme_thesis 



data_FAVAR %>%
  select(-DATE_QUARTER) %>% 
  VARselect(type = "const", lag.max = 10)




# Model estimation --------------------------------------------------------

model_FAVAR <- data_FAVAR %>%
  select(-DATE_QUARTER) %>% 
  VAR(p = final_p, type = "const")

summary(model_FAVAR$varresult$REAL_GDP_GROWTH)




# Granger causality -------------------------------------------------------
# Tests whether independent variables granger cause dependent variable (REAL_GDP_GROWTH).
# Basically this is a F-Test comparing explanatory power of an unrestricted model with all variables (Y & X) and the smaller model
# without the variable whose "causality" should be tested.
# In its core the Granger causality test is a F-Test with H_0: R^2 of the unrestricted model including the lagged values 
# of Y and X is equal to the R^2 of the smaller model with only lagged values of Y. The ratio of both R^2 builds the F-statistic.
# For large values of the F-statitistic H_0 (no "granger causality") can be rejected.



## Joint Granger causality ================================================

# Joint granger causality test (causality of one variable on all other variables)
granger_FAVAR_joint1 <- data_FAVAR %>% 
  select(-DATE_QUARTER) %>%
  colnames() %>% 
  enframe(name = NULL, value = "SERIES") %>% 
  mutate(GRANGER = map(SERIES, function(x) causality(model_FAVAR, cause = x, vcov. = vcovHC(model_FAVAR))$Granger),
         P_VALUE = map(GRANGER, ~ .$p.value[1,1])) %>% 
  unnest(P_VALUE)

#granger_FAVAR_joint1


# Joint granger causality test (causality of all variables on one variables)
variables_FAVAR <- data_FAVAR %>% 
  select(-DATE_QUARTER) %>% 
  names()

granger_FAVAR_joint2 <- data_FAVAR %>% 
  select(-DATE_QUARTER) %>%
  colnames() %>% 
  enframe(name = NULL, value = "SERIES") %>% 
  mutate(GRANGER = map(SERIES, function(x) causality(model_FAVAR, cause = variables_FAVAR[!(variables_FAVAR == x)], vcov. = vcovHC(model_FAVAR))$Granger),
         P_VALUE = map(GRANGER, ~ .$p.value[1,1])) %>% 
  unnest(P_VALUE)

granger_FAVAR_joint2






# Granger causality of all features on traget variable
model_FAVAR %>% 
  causality(cause = variables_FAVAR[!str_detect(string = variables_FAVAR, pattern = "REAL_GDP_GROWTH")], vcov. = vcovHC(.)) %>% 
  .$Granger


## Individual Granger causality ===========================================


# Test whether the inclusion of the respective variable improves
# the minimal model with only lagged values of the target (AR model)
granger_FAVAR_ind1 <- tibble(.rows = ncol(data_FAVAR)) %>%                 # create tibble with number of rows being equal to the number of columns in dataframe input
  mutate(VARIABLE = colnames(data_FAVAR)) %>%                           # create string column with column names as input
  mutate(SERIES = map(data_FAVAR, ~ (c(t(.))))) %>%                     # create column with nested time series of respective variable
  filter(VARIABLE != "DATE_QUARTER") %>% 
  filter(VARIABLE != "REAL_GDP_GROWTH") %>% 
  mutate(GRANGER = map(SERIES, function(v) grangertest(x = data_FAVAR$REAL_GDP_GROWTH, y = v, order = final_p)),
         P_VALUE = map(GRANGER, ~ .$`Pr(>F)`[2])
  ) %>% 
  unnest(P_VALUE)

granger_FAVAR_ind1


# Test whether the inclusion of the respective variable improves
# the full model including all variables
granger_FAVAR_ind2 <- function_granger_stepwise(df = data_FAVAR, lags = final_p)
#granger_FAVAR_ind2




# Analysis residuals ------------------------------------------------------
## Complete VAR model =====================================================
# Define residuals tibbel
residual <- model_FAVAR %>% 
  residuals() %>%                                                          # extract residuals from model
  as_tibble() %>% 
  bind_cols(data_training %>%                                              # bind the respective date to the residuals (required for time series plot of residuals)
              select(DATE_QUARTER) %>% 
              filter(!(row.names(.) %in% 1:final_p))) %>%                  # drop the residuals which are meaningless (=0) according to the number of AR components in the model   
  select(DATE_QUARTER, everything())

# Residual ACF (compress plots into one figure here)
for (v in colnames(data_FAVAR)){
  print(residual %>% 
          select(v) %>% 
          ggAcf(main = "", lag.max = lag_max) +                                     # plot empirical ACF
          theme_thesis)
  
}



# Multivate Portmanteau tests to test for serial correlation 
serial.test(model_FAVAR, 
            lags.pt = 40,
            lags.bg = 50,
            type = "PT.asymptotic")


BoxPierce(model_FAVAR, lags = seq(10,50,5)) %>%                              # identical to serial.test above
  as_tibble()
LjungBox(model_FAVAR, lags = seq(10,40,5))

# Additional Test
Hosking(model_FAVAR, lags = seq(10,40,5))


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




# Miscellaneous -----------------------------------------------------------

## prcomp =================================================================

pca <- prcomp(data_training[-c(1,2)], center = TRUE,  scale. = TRUE)
# PC Loading matrix
loadings1 <- pca$rotation %>% 
  as_tibble()
  #as_tibble(rownames="VARIABLE")

# PC score matrix
score1 <- pca$x %>% 
  as_tibble() 

# Plot first two PCs
biplot(pca, scale = 0)


# Check how loadings and score relate
dim(score1)
dim(loadings1)
dim(data_training[-c(1,2)])

temp <- as.matrix(score1)%*%t(as.matrix(loadings1))
colnames(temp) <- colnames(data_training[-c(1,2)])
temp1 <- temp %>%   
  as_tibble()

temp2 <- data_training[-c(1,2)] %>% 
  mutate_all(scale)

check <- colMeans(temp1)-colMeans(temp2) %>% 
  as_tibble() 
  
format(check$value, scientific=FALSE)
format(round(check$value, 1), scientific=FALSE)
# all equal, i.e. X = loading*score

# What does this mean in reverse?
# score=loading^-1*X?
# Let's see
dim(temp2)
dim(loadings1)

temp3 <- as.matrix(temp2)%*%as.matrix(loadings1)
temp3 %>% 
  as_tibble()
score1
# all equal, i.e. score = X*loading


# This can only be possible if loadings'*loadings=I
# Check!!!
temp13 <- t(as.matrix(loadings1))%*%as.matrix(loadings1)
temp13 %>% as_tibble() %>% 
  mutate_all(.funs = round)



# Let's do it manually
standard_X <- data_training[-c(1,2)] %>% 
  mutate_all(scale)
cov_X <- cov(standard_X)
cor_X <- cor(data_training[-c(1,2)])
eig <- eigen(cov_X)
eig_cor <- eigen(cor_X)

eigenVec <- eig$vectors
eigenVec_cor <- eig_cor$vectors
eig <- eig$values # eigenvalues are ordered 

dim(standard_X)
dim(eigenVec)

-as.matrix(standard_X)%*%eigenVec %>% 
  as_tibble()
score1
## all equal, i.e. score (PC) = X*loading (matrix of eigenvectors)

-as.matrix(standard_X)%*%eigenVec_cor %>% 
  as_tibble()
score1
## all equal, i.e. score (PC) = X*loading (matrix of eigenvectors)

-as.matrix(standard_X)%*%eigenVec[,1] %>% 
  as_tibble()
score1[,1]





# Check if normalization holds
dim(score1)
temp9 <- 1/nrow(score1)*(t(as.matrix(score1))%*%as.matrix(score1))
temp9 %>% as_tibble() %>% 
  mutate_all(.funs = round)
# yes it does




# Now calculate components aka factors according to Bernanke (2015)
dim(loadings1)
dim(score1)

eigenVec %>% as_tibble()

temp10 <- sqrt(nrow(score1))*eigenVec 
temp10 %>% as_tibble()
score1












# Calculate proportion of variance explained (PVE)
pve <- pca$sdev^2/sum(pca$sdev^2)

# Plot pve
plot(pve , xlab=" Principal Component ", ylab="Proportion of Variance Explained ", ylim=c(0,1) ,type='b')
plot(cumsum(pve ), xlab="Principal Component ", ylab=" Cumulative Proportion of Variance Explained ", ylim=c(0,1) ,
       type='b')






# prcomp + lm
prcomp_pca <- prcomp(data_training[-1], center = TRUE,  scale. = TRUE, rank. = 3)

# Extract scores
prcomp_score <- prcomp_pca$x %>% 
  as_tibble()


# Extract loadings
prcomp_load <- prcomp_pca$rotation %>% 
  as_tibble()
dim(prcomp_load)
prcomp_load_matrix <- as.matrix(prcomp_load)

# Mean of training set
prcomp_mean <- prcomp_pca$center %>% 
  as_tibble()
dim(prcomp_mean)
prcomp_mean_matrix <- as.matrix(prcomp_mean)


# Test data set with loadings as predictors
X <- data_test[-1] %>% 
  mutate_all(scale)

X_demean <- matrix(nrow = nrow(X), ncol = ncol(X)) %>% 
  as_tibble() 
colnames(X_demean) <- colnames(X)
for (i in 1:nrow(prcomp_mean)){
  for (j in 1:nrow(X)){
     X_demean[j,i] <- X[j,i]-prcomp_mean[i,1]
  }
  
}
X_demean
dim(X)
X_matrix <- as.matrix(X)




model_data <- data_training %>% 
  select(REAL_GDP_GROWTH) %>% 
  bind_cols(prcomp_score)

prcomp_model_pca <- lm(REAL_GDP_GROWTH~., data = model_data)
beta_PC <- coef(model_pca)[2:4] %>% 
  as_vector()

alpha_PC <- coef(model_pca)[1]

est1 <- X_matrix%*%prcomp_load_matrix%*%beta_PC + alpha_PC 




## pcr ====================================================================
# Conduct principal component regression
set.seed(333)
temp <- pcr(REAL_GDP_GROWTH~., data=data_training, center=TRUE, scale = TRUE, validation="CV")
temp2 <- pcr(REAL_GDP_GROWTH~REAL_GDP_GROWTH..1+., data=data_training, center=TRUE, scale = TRUE, ncomp=3)
temp3 <- pcr(REAL_GDP_GROWTH~., data=data_training, center=TRUE, scale = TRUE, ncomp=3)
# Investiagte results
summary(temp2)
temp2$coefficients

# Analyze number of components via cross validation
validationplot(temp)


# PC Loading matrix
loadings2 <- temp2$projection %>% as_tibble()
loadings1
# PC score matrix
score2 <- temp2$scores[1:dim(temp2$scores)[1], 1:dim(temp2$scores)[2]] %>% 
  as_tibble()
score1

# Prediction in test set
est2 <- predict(temp2, newdata=data_test[-1], scale=TRUE, center=TRUE, ncomp=3) %>% 
  as_tibble()
est3 <- predict(temp3, newdata=data_test[-1], scale=TRUE, center=TRUE, ncomp=3) %>% 
  as_tibble()

est2 %>% 
  rename(MODEL=`REAL_GDP_GROWTH.3 comps`) %>% 
  bind_cols(OWN=est1) %>% 
  mutate(DIFF = abs(MODEL-OWN)) %>% 
  mutate(MAX=max(DIFF),
         MIN =min(DIFF))

## Visualization loadings (old) ===========================================


pca_loadings_relevant %>%
  filter(PC == "PC1") %>%
  ggplot()+
  geom_bar(aes(x = VARIABLE, y = LOADING), stat = "identity", fill = ml_green_dark) +
  theme_thesis +
  theme(axis.text.x = element_text(angle = 90,
                                   hjust = 1,
                                   vjust= 0.25,
                                   size = 5),
        plot.margin=unit(c(0.5,0.5,4,0.5),"cm"),
        panel.grid.major.x = element_line(color = "grey90", size = 0.5),
        axis.title.x = element_blank()) +
  ylab("Loadings") +
  xlab("Features") +
  annotate("segment", x=id+0.5, y=rep(-0.45, length(id)), xend=id+0.5, yend=rep(-0.23, length(id)),
           col="black") +
  annotate("text", x=id2, y=rep(-0.44, length(id)),
           label=id_label, size = 1.5, hjust = "bottom",
           fontface = "bold", angle = 90) +
  coord_cartesian(xlim = c(0,dim(pca_loadings)[1]),
                  ylim = c(min(pca_loadings %>% select(2:max(factor_n,2))),
                           max(pca_loadings %>% select(2:max(factor_n,2)))),
                  expand = TRUE,
                  clip = "off")