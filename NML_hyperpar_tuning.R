# Set global R options
options(scipen = 999)

# Set the graphical theme
ggplot2::theme_set(ggplot2::theme_light())

# Set global knitr chunk options
knitr::opts_chunk$set(
  cache = FALSE,
  warning = FALSE, 
  message = FALSE
)

#---------------
# Load required packages
#---------------

library(readxl)
library(tidyr)
library(rpart)
library(rpart.plot)  # for plotting decision trees
library(vip)         # for feature importance
library(pdp)         # for feature effects
library(ggplot2)
library(rpart)       # direct engine for decision tree application
library(caret)       # meta engine for decision tree application
# Helper packages
library(dplyr)    # for general data wrangling needs
# Modeling packages
library(gbm)      # for original implementation of regular and stochastic GBMs
library(h2o)      # for a java-based implementation of GBM variants
library(xgboost)  # for fitting extreme gradient boosting

#---------------
# 1. Import data
#---------------

# import data of changeable experiment condition 
df_expe <- read_excel("experiment_data.xlsx", sheet = "PredictOH")%>%
  as.data.frame()
df_expe[is.na(df_expe)] <- 0
# import data of element attribute
df_elem <- read.csv("elements_data.csv") %>% 
  as.data.frame()

df_elem %>% head(5)

# specify the input sample
# Can be used as the input interface
(df_expe_colname <- df_expe %>% colnames()) 
AA1BB1O3_sample <- data.frame(matrix(ncol = length(df_expe_colname), nrow = 0,
                              dimnames = list(NULL, df_expe_colname)))
AA1BB1O3_sample <- df_expe


#-----------------------------
# 2. Descriptor customization
#-----------------------------

# Customize the descriptors based on the oxide composition and element information
source("descriptor_customize.R")
df_sample_descriptor <- descriptor_customize(AA1BB1O3_sample) 

df_sample <- cbind(AA1BB1O3_sample[c("Composition_AA1BB1O3", "proton_concentration")], df_sample_descriptor)

# write.csv(df_sample, file = "df_sample.csv", row.names = FALSE)

#-----------------------------
#3. Regression model training 
#-----------------------------

# import sample data including experimental condition and customized descriptors
df_sample <- read.csv("df_sample.csv", check.names=FALSE) %>% 
  as.data.frame()

df_sample %>% colnames()
dim(df_sample)

######
# 3.1 data splitting
######

set.seed(0714)
trainIndex <- createDataPartition(df_sample$proton_concentration, 
                                  p = .8, 
                                  list = FALSE, 
                                  times = 1)

pc_train <- df_sample[trainIndex,][,-1]
pc_test <- df_sample[-trainIndex,][,-1]

write.csv(pc_train, file = "pc_train_0714.csv")
write.csv(pc_test, file = "pc_test_0714.csv")


pc_train <- read.csv("pc_train_0714.csv", header = TRUE, row.names = 1) %>% as.data.frame()
trainIndex <- as.numeric(rownames(pc_train))
pc_test <- read.csv("pc_test_0714.csv", header = TRUE, row.names = 1) %>% as.data.frame()
(testIndex <- as.numeric(rownames(pc_test)))


# find correlated variables by correlation analysis
library(corrplot)
cor_mat <- cor(pc_train[-1])
# save correlation plot
png(height=1800, width=1800, file="corplot.png", type = "cairo")
corrplot(cor_mat, tl.cex=1.8, tl.col="black",cl.cex=2.5) # plot correlation matrix
dev.off()



#
# specify the resampling type
fitControl <- caret::trainControl( # 10-fold CV
  method = "cv", # cross-validation
  number = 10, # nbr of folds
  returnResamp = "final",
  savePredictions = TRUE,
  preProcOptions = list(cutoff = 0.90),
  allowParallel = TRUE)


######
# 3.2 Model training with Random Forest method
###

# getModelInfo("rf")$rf$grid
# number of features
n_features <- length(setdiff(names(pc_train), "proton_concentration"))
# Create hyperparameter grid
rf_grid <- expand.grid(
  mtry = seq(2,26, by = 2),
  splitrule = c("variance", "extratrees","maxstat","beta"),
  #replace = c(TRUE),
  min.node.size = c(1,3,5,10)
  )

start_time <- Sys.time() # 25 minutes for one run
# Tune a random forest model using grid search
set.seed(0714)
rf_tune_fit_ranger <- caret::train(
  proton_concentration ~ .,
  data = pc_train,
  method = "ranger", 
  preProc = c("range","nzv", "corr"),
  verbose = FALSE,
  trControl = fitControl,
  tuneGrid = rf_grid,
  metric = "RMSE",
  importance = 'permutation'
)
end_time <- Sys.time()
end_time - start_time

rf_tune_fit <- rf_tune_fit_ranger

saveRDS(rf_tune_fit,"hyperpar_rf_fit_0714_20241019.rds") # save the model to disk
rf_tune_fit <- readRDS("hyperpar_rf_fit_0714_20241019.rds") # load the model
rf_tune_fit

mean(rf_tune_fit$results$RMSE)
sd(rf_tune_fit$results$RMSE)


mean(rf_tune_fit$results$Rsquared)
sd(rf_tune_fit$results$Rsquared)


mean(rf_tune_fit$results$MAE)
sd(rf_tune_fit$results$MAE)

#
# rf_tune_fit <- readRDS("rf_fit_0714_0719tune.rds") # load the model
# randomForest::getTree(rf_tune_fit$finalModel, k = tree_num)

rf_tune_fit$results$RMSE

library(ggpubr)

trellis.par.set(caretTheme())
par(mfrow = c(2,2))
densityplot(rf_tune_fit, pch = "|", metric = "RMSE") -> den1
den1
densityplot(rf_tune_fit, pch = "|", metric = "Rsquared") -> den2
densityplot(rf_tune_fit, pch = "|", metric = "MAE") -> den3
plot(varImp(rf_tune_fit, scale = FALSE), top = 10) -> imp4


# rmse_density
png('rmse_density.png', width = 6, height=8, units='cm', res=600, pointsize = 5)
densityplot(rf_tune_fit, pch = "|", metric = "RMSE")
dev.off()

# r2_density
png('r2_density.png', width = 6, height=8, units='cm' ,res=600, pointsize = 5)
densityplot(rf_tune_fit, pch = "|", metric = "Rsquared") 
dev.off()

# mae_density
png('mae_density.png', width = 6, height=8, units='cm' ,res=600, pointsize = 5)
densityplot(rf_tune_fit, pch = "|", metric = "MAE") 
dev.off()

library(cowplot)
plot_grid(den1, den2, den3, imp4, ncol=2, nrow =2)
dev.off()


# choose the data with the best tuned parameters
rf_tune_fit$bestTune # the best tuned parameters
df_rf = rf_tune_fit$pred
df_rf_best = filter(df_rf, mtry == 24 & splitrule == 'beta'& min.node.size ==3)  
df_rf_best %>% ggplot(aes(x = obs, y = pred, color = Resample)) +
  geom_point()

df_rf_best %>% head(5)
rf_tune_fit %>% head(5)


df_rf_best_error = filter(rf_tune_fit$results, mtry == 24 & splitrule == 'beta'& min.node.size ==3) 
df_rf_best_error

# RMSE, 95% confidence interval
ME_rmse = 1.96*df_rf_best_error$RMSESD/sqrt(636) # margin of error
(mean_rmse = round(df_rf_best_error$RMSE, 4))
(up_rmse = round(df_rf_best_error$RMSE + ME, 4))
(low_rmse = round(df_rf_best_error$RMSE - ME, 4))

## Rsquared, 95% confidence interval
ME_r2 = 1.96*df_rf_best_error$RsquaredSD/sqrt(636)
(mean_r2 = round(df_rf_best_error$Rsquared,4))
(up_r2 = round(df_rf_best_error$Rsquared + ME, 4))
(low_r2 = round(df_rf_best_error$Rsquared - ME, 4))

## MAE, 95% confidence interval
ME_mae = 1.96*df_rf_best_error$MAESD/sqrt(636) #
(mean_mae = round(df_rf_best_error$MAE, 4))
(up_mae = round(df_rf_best_error$MAE + ME, 4))
(low_mae = round(df_rf_best_error$MAE - ME, 4))

# table of 95% confidence interval of RMSE, R2, MAE
CI_df <- data.frame(col1 = c("", "RMSE", "Rsquared", "MAE"),
                    col2 = c("mean", mean_rmse, mean_r2, mean_mae),
                    col3 = c("up", up_rmse, up_r2, up_mae),
                    col4 = c("low", low_rmse, low_r2, low_mae))
CI_df



# qqplot for predicted values
ggqqplot(rf_tune_fit$pred, x = "pred", color = "Resample", size = 0.3) +
  theme(axis.text = element_text(size = 12, color = "black"),
        axis.title=element_text(size=13),
        #panel.grid= element_blank(),
        plot.background = element_rect(fill = "white", color = NA),
        legend.background = element_rect(fill="transparent", linewidth = 0.3),
        legend.box.background = element_rect(colour = NA),
        # legend.position = c(0.58,0.90),
        axis.line.x.bottom = element_line(linewidth = 0.1),
        axis.line.y.left = element_line(linewidth = 0.1),
        panel.border = element_rect(colour = "black", fill=NA, linewidth= 1)
  )  -> fig_pred

#
ggsave(
  filename = "fig_pred_qqplot.tiff",
  plot = fig_pred,
  width = 7,  # <=19.05cm
  height = 7.5, # <=22.225cm
  units= "cm",
  dpi= 500,
  compression = "lzw")


#
# qqplot for observed values
ggqqplot(rf_tune_fit$pred, x = "obs", color = "Resample", size = 0.3) +
  theme(axis.text = element_text(size = 12, color = "black"),
      axis.title=element_text(size=13),
      #panel.grid= element_blank(),
      plot.background = element_rect(fill = "white", color = NA),
      legend.background = element_rect(fill="transparent", linewidth = 0.3),
      legend.box.background = element_rect(colour = NA),
      # legend.position = c(0.58,0.90),
      axis.line.x.bottom = element_line(linewidth = 0.1),
      axis.line.y.left = element_line(linewidth = 0.1),
      panel.border = element_rect(colour = "black", fill=NA, linewidth= 1)
)  -> fig_obs

#
ggsave(
  filename = "fig_obs_qqplot.tiff",
  plot = fig_obs,
  width = 7,  # <=19.05cm
  height = 7.5, # <=22.225cm
  units= "cm",
  dpi= 500,
  compression = "lzw")



##
# predicted vs observed by using the best tuned parameters
ggplot() +
  geom_point(data = rf_tune_fit$pred, aes(x = obs, y = pred), size = 0.3, color = "gray80", shape=1) + 
  geom_point(data = df_rf_best, aes(x = obs, y = pred), color = "purple", shape = 17) +
  geom_abline(slope = 1, intercept = 0, color = 'black', linewidth = 0.6) +
  scale_x_continuous(expand = c(0, 0), limits = c(-0.01, 0.41)) +
  scale_y_continuous(expand = c(0, 0), limits = c(-0.01, 0.41)) +
  theme(axis.text = element_text(size = 12, color = "black"),
        axis.title=element_text(size=13),
        #panel.grid= element_blank(),
        panel.grid.major = element_line(color = "gray90", size = 0.2, linetype = 1),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "white", color = NA),
        legend.background = element_rect(fill="transparent", linewidth = 0.3),
        legend.box.background = element_rect(colour = NA),
        axis.line.x.bottom = element_line(linewidth = 0.1),
        axis.line.y.left = element_line(linewidth = 0.1),
        panel.border = element_rect(colour = "black", fill=NA, linewidth= 0.8)
  ) -> fig_pred_obs_best

fig_pred_obs_best
# 
ggsave(
  filename = "fig_pred_obs_best.tiff",
  plot = fig_pred_obs_best,
  width = 7,  # <=19.05cm
  height = 9, # <=22.225cm
  units= "cm",
  dpi= 500,
  compression = "lzw")

rf_tune_fit



# #

tiff(file="RMSE_hyper_rf.tiff",
     width=12, height=8, units="in", res=300)
plot(rf_tune_fit, metric = "RMSE",
     xlab = list("m_try", font=1, cex=1.5),
     ylab = list(font=1, cex=1.5),
     scales = list(x = list(font=1, cex=1.2), 
                   y = list(font=1, cex=1.2))) 
dev.off()


tiff(file="Rsquared_hyper_rf.tiff",
     width=12, height=8, units="in", res=300)
plot(rf_tune_fit, metric = "Rsquared",
     xlab = list("m_try", font=1, cex=1.5),
     ylab = list(font=1, cex=1.5),
     scales = list(x = list(font=1, cex=1.2), 
                   y = list(font=1, cex=1.2))) 
dev.off()

tiff(file="MAE_hyper_rf.tiff",
     width=12, height=8, units="in", res=300)
plot(rf_tune_fit, metric = "MAE",
     xlab = list("m_try", font=1, cex=1.5),
     ylab = list(font=1, cex=1.5),
     scales = list(x = list(font=1, cex=1.2), 
                   y = list(font=1, cex=1.2))) 
dev.off()


rf_fit$finalModel$num.trees





