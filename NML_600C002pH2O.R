## predict the pc with the temperature range and water pressure
# being 600C and 0.02 (pH2O) for different composites
# LaCa10CoNi10, et al.
library(readxl)
# library(xlsx)
library(tidyr)
library(plyr)
library(ggplot2)
library(MASS)

setwd("/Users/baoyinyuan/Nutstore Files/AdvancedMaterials2022/FirstRevision_AM_20220614/script_1stRevisedFig20220714_github_20240403zheng")

df_elem <- read.csv("elements_data.csv") %>% 
  as.data.frame()
source("descriptor_customize.R", local = TRUE)


df_expe_user = as.data.frame(matrix(nrow = 1, ncol = 2))
colnames(df_expe_user) = c("temperature_C", "pH2O")
df_expe_user$temperature_C = 500 ##  For change
df_expe_user$pH2O = 0.02


A_vector <- c("La")
A_valence_vector <- c(3)
A_fraction_vector <- seq(0.9, 0.6, by = - 0.1)
A1_vector <- c("Ca", "Ba", "Al",  "Mg")
A1_valence_vector <- c(2, 2, 3, 2)
A1_fraction_vector <- 1 - A_fraction_vector
B_vector <- c("Co")
B_valence_vector <- c(3)
B_fraction_vector <- seq(0.9, 0.6, by = - 0.1)
B1_vector <- c("Ni", "Mn", "Fe", "Zn", "Y","Yb","Cu","Bi")
B1_valence_vector <- c(2, 3, 3, 2, 3, 3, 2, 3)
B1_fraction_vector <- 1 - B_fraction_vector

num_A_vector <- length(A_vector)
num_A_fraction <- length(A_fraction_vector)
num_A1_vector <- length(A1_vector)
num_B_vector <- length(B_vector)
num_B_fraction <- length(B_fraction_vector)
num_B1_vector <- length(B1_vector)

num_col_table <- num_A_fraction * num_A1_vector
num_row_table <- num_B_fraction * num_B1_vector
(num_composite <- num_row_table * num_col_table)

# data.frame of different composites combined by the above components
df_elem_user = as.data.frame(matrix(nrow = num_composite, ncol = 12))
colnames(df_elem_user) = c( "A", "A_valence", "A_fraction",
                            "A1", "A1_valence", "A1_fraction",
                            "B", "B_valence", "B_fraction", 
                            "B1", "B1_valence", "B1_fraction")
df_elem_user$A <- rep(A_vector, length.out = num_composite)
df_elem_user$A_valence <- rep(A_valence_vector, length.out = num_composite)
df_elem_user$A_fraction <- rep(A_fraction_vector, each = num_B_vector*num_B_fraction*num_B1_vector)
df_elem_user$A1 <- rep(A1_vector, each = num_A_fraction*num_B_vector*num_B_fraction*num_B1_vector)
df_elem_user$A1_valence <- rep(A1_valence_vector, each = num_A_fraction*num_B_vector*num_B_fraction*num_B1_vector)
df_elem_user$A1_fraction <- rep(A1_fraction_vector, each = num_B_vector*num_B_fraction*num_B1_vector)

df_elem_user$B <- rep(B_vector, each = num_B_fraction*num_B1_vector)
df_elem_user$B_valence <- rep(B_valence_vector, each = num_B_fraction*num_B1_vector)
df_elem_user$B_fraction <- rep(B_fraction_vector, length.out = num_composite)
df_elem_user$B1 <- rep(B1_vector, each = num_B_fraction)
df_elem_user$B1_valence <- rep(B1_valence_vector, each =  num_B_fraction)
df_elem_user$B1_fraction <- rep(B1_fraction_vector, length.out = num_composite)

df_expe_elem_user <- data.frame() 
for(i in seq(num_composite)){
  expe_elem_user = cbind(df_expe_user, df_elem_user[i,])
  # print(expe_elem_user)
  df_expe_elem_user = rbind(df_expe_elem_user, expe_elem_user)
} 


rf_fit <- readRDS("rf_fit_0714_0719tune.rds") # tune parameters for rf
# rf_fit <- readRDS("rf_fit_0714.rds") # load the trained model of random forest
# gbm_fit <- readRDS("gbm_fit_0714.rds") # load the trained model of gradient boosting machine
# gbm_logfit <- readRDS("gbm_logfit_0714.rds") # load log-transformed model of gbm
predict_pc_composites <- numeric(num_composite)
df_expe_elem_tosave <- data.frame()
start_time <-  Sys.time()
for(i in seq(num_composite)){
  newdata = descriptor_customize(df_expe_elem_user[i,])
  # df_expe_elem_tosave = rbind(df_expe_elem_tosave, cbind(df_expe_elem_user[i,], newdata))
  df_expe_elem_tosave = rbind(df_expe_elem_tosave, newdata)
  predict_pc_composites[i] <- predict(rf_fit, newdata = newdata) # for rf model
  # predict_pc_composites[i] <- predict(gbm_fit, newdata = newdata) # for gbm model
  # predict_logpc_composites[i] <- predict(gbm_logfit, newdata = newdata) # for gbm model
  print(i)
}
end_time <-  Sys.time()
(time_elapsed <- end_time - start_time)
# save experiment, element data and predicted PC to csv
df_expe_elem_tosave$predictedPC =  predict_pc_composites

## for log transformed pc
# predict_pc_composites = exp(predict_logpc_composites)
# df_expe_elem_tosave$predictedPC =  exp(predict_pc_composites)

# for tuning par of rf
# write.csv(df_expe_elem_tosave,"df_expe_elem_tosave_rf_600_02_0714_0719tune.csv", row.names=FALSE)
# write.csv(df_expe_elem_tosave,"df_expe_elem_tosave_rf_550_02_0714_0719tune.csv", row.names=FALSE)
write.csv(df_expe_elem_tosave,"df_expe_elem_tosave_rf_500_02_0714_20241018tune.csv", row.names=FALSE)
# write.csv(df_expe_elem_tosave,"df_expe_elem_tosave_rf_450_02_0714_20240622tune.csv", row.names=FALSE)
# write.csv(df_expe_elem_tosave,"df_expe_elem_tosave_rf_400_02_0714_0719tune.csv", row.names=FALSE)

# write.csv(df_expe_elem_tosave,"df_expe_elem_tosave_rf_600_02_0714.csv", row.names=FALSE) 
# write.csv(df_expe_elem_tosave,"df_expe_elem_tosave_rf_550_02_0714.csv", row.names=FALSE)
# write.csv(df_expe_elem_tosave,"df_expe_elem_tosave_rf_500_02_0714.csv", row.names=FALSE) 
# write.csv(df_expe_elem_tosave,"df_expe_elem_tosave_rf_450_02_0714.csv", row.names=FALSE)
# write.csv(df_expe_elem_tosave,"df_expe_elem_tosave_rf_400_02_0714.csv", row.names=FALSE) 

# write.csv(df_expe_elem_tosave,"df_expe_elem_tosave_gbm_600_02_0714.csv", row.names=FALSE) 
# write.csv(df_expe_elem_tosave,"df_expe_elem_tosave_gbm_550_02_0714.csv", row.names=FALSE)
# write.csv(df_expe_elem_tosave,"df_expe_elem_tosave_gbm_500_02_0714.csv", row.names=FALSE) 
# write.csv(df_expe_elem_tosave,"df_expe_elem_tosave_gbm_450_02_0714.csv", row.names=FALSE)
# write.csv(df_expe_elem_tosave,"df_expe_elem_tosave_gbm_400_02_0714.csv", row.names=FALSE) 

# write.csv(df_expe_elem_tosave,"df_expe_elem_tosave_loggbm_600_02_0714.csv", row.names=FALSE) 
# write.csv(df_expe_elem_tosave,"df_expe_elem_tosave_loggbm_550_02_0714.csv", row.names=FALSE)
# write.csv(df_expe_elem_tosave,"df_expe_elem_tosave_loggbm_500_02_0714.csv", row.names=FALSE) 
# write.csv(df_expe_elem_tosave,"df_expe_elem_tosave_loggbm_450_02_0714.csv", row.names=FALSE)
# write.csv(df_expe_elem_tosave,"df_expe_elem_tosave_loggbm_400_02_0714.csv", row.names=FALSE) 


# read rf
# predict_pc_composites <- read.csv("df_expe_elem_tosave_rf_600_02_0714_0719tune.csv") %>% 
#   as.data.frame() # for tuning parameter of rf
# predict_pc_composites <- read.csv("df_expe_elem_tosave_rf_550_02_0714_0719tune.csv") %>% 
#   as.data.frame() # for tuning parameter of rf
predict_pc_composites <- read.csv("df_expe_elem_tosave_rf_500_02_0714_20241018tune.csv") %>%
  as.data.frame() # for tuning parameter of rf
# predict_pc_composites <- read.csv("df_expe_elem_tosave_rf_450_02_0714_20240622tune.csv") %>% 
#   as.data.frame() # for tuning parameter of rf
# predict_pc_composites <- read.csv("df_expe_elem_tosave_rf_400_02_0714_0719tune.csv") %>% 
#   as.data.frame() # for tuning parameter of rf


# predict_pc_composites <- read.csv("df_expe_elem_tosave_rf_600_02_0714.csv") %>% 
#   as.data.frame()
# predict_pc_composites <- read.csv("df_expe_elem_tosave_rf_550_02_0714.csv") %>% 
#   as.data.frame()
# predict_pc_composites <- read.csv("df_expe_elem_tosave_rf_500_02_0714.csv") %>% 
#   as.data.frame()
# predict_pc_composites <- read.csv("df_expe_elem_tosave_rf_450_02_0714.csv") %>% 
#   as.data.frame()
# predict_pc_composites <- read.csv("df_expe_elem_tosave_rf_400_02_0714.csv") %>% 
#   as.data.frame()

# # read gbm
# predict_pc_composites <- read.csv("df_expe_elem_tosave_gbm_600_02_0714.csv") %>% 
#   as.data.frame()
# predict_pc_composites <- read.csv("df_expe_elem_tosave_gbm_550_02_0714.csv") %>% 
#   as.data.frame()
# predict_pc_composites <- read.csv("df_expe_elem_tosave_gbm_500_02_0714.csv") %>% 
#   as.data.frame()
# predict_pc_composites <- read.csv("df_expe_elem_tosave_gbm_450_02_0714.csv") %>% 
#   as.data.frame()
# predict_pc_composites <- read.csv("df_expe_elem_tosave_gbm_400_02_0714.csv") %>% 
#   as.data.frame()

# # read loggbm
# predict_pc_composites <- read.csv("df_expe_elem_tosave_loggbm_600_02_0714.csv") %>% 
#   as.data.frame()
# predict_pc_composites <- read.csv("df_expe_elem_tosave_loggbm_550_02_0714.csv") %>% 
#   as.data.frame()
# predict_pc_composites <- read.csv("df_expe_elem_tosave_loggbm_500_02_0714.csv") %>% 
#   as.data.frame()
# predict_pc_composites <- read.csv("df_expe_elem_tosave_loggbm_450_02_0714.csv") %>% 
#   as.data.frame()
# predict_pc_composites <- read.csv("df_expe_elem_tosave_loggbm_400_02_0714.csv") %>% 
#   as.data.frame()


# reshape the predicted PC to matrix
predict_pc_composites_matrix <- matrix(predict_pc_composites$predictedPC, nrow = num_row_table)

# column names of matrix 
colnames_composites_matrix <- unique(paste("L",
                                           df_expe_elem_user$A1,
                                           format(df_expe_elem_user$A1_fraction*100, nsmall = 0),
                                           sep = "")
                                     )

# row names of matrix
rownames_composites_matrix <- unique(paste("C",
                                           df_expe_elem_user$B1,
                                           format(df_expe_elem_user$B1_fraction*100, nsmall = 0),
                                           sep = "")
                                     )  

rownames(predict_pc_composites_matrix) <- rownames_composites_matrix
colnames(predict_pc_composites_matrix) <- colnames_composites_matrix
   


write.table(predict_pc_composites_matrix, file = "predict_pc_composites_matrix_rf_500_02_0714_20241018tune.txt", row.names = TRUE)
hpc_mat = read.table("predict_pc_composites_matrix_rf_500_02_0714_20241018tune.txt", row.names = 1, header = TRUE)

write.csv(predict_pc_composites_matrix, file = "predict_pc_composites_matrix_rf_500_02_0714_20241018tune.csv", row.names = TRUE)

## ggplot

df_predict_pc_composites <- reshape2::melt(predict_pc_composites_matrix)
df_predict_pc_composites %>% head(2)
colnames(df_predict_pc_composites) <- c("B Site","A Site","Predicted PC")

df_predict_pc_composites$`A Site` <- factor(df_predict_pc_composites$`A Site`,levels=unique(df_predict_pc_composites$`A Site`))
df_predict_pc_composites$`B Site` <- factor(df_predict_pc_composites$`B Site`,levels=unique(df_predict_pc_composites$`B Site`))

predict_pc_composites_plt <- ggplot(data = df_predict_pc_composites, aes(x = `A Site`, y = `B Site`)) +
  geom_tile(aes(fill = `Predicted PC`)) +
  xlab("Composite at A site") +
  ylab("Composite at B site") +
  scale_x_discrete(expand = c(0,0)) +
  scale_y_discrete(expand = c(0,0)) +
  scale_fill_gradient2("Predicted\nHPC", 
                       low = "purple3", 
                       mid = "lightgrey",
                       high = "red2",
                       # midpoint = .028) + # temp：600
                       # midpoint = .032) + # temp：550
                       # midpoint = .036) + # temp：500
                       midpoint = .042) + # temp：450
                       # midpoint = .045) + # temp：400
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1,color="black", size = 10),
        axis.text.y = element_text(vjust = 0.5, hjust=1, color="black", size= 10),
        axis.title.x = element_text(face="bold", colour="black", size = 12),
        axis.title.y = element_text(face="bold", colour="black", size = 12),
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 10)
        )
predict_pc_composites_plt
#


#
#
ggsave(# filename = "predict_pc_composites_plt_loggbm_600_02_0714.tiff",
       filename = "predict_pc_composites_plt_rf_500_023_0714_20240808tune.tiff",
       plot = predict_pc_composites_plt,
       width = 19,  # <=19.05cm
       height = 25, # <=22.225cm
       units= "cm",
       dpi= 600,
       compression = "lzw")




#####
## violin plot

colnames(hpc_mat)
rownames(hpc_mat)
dim(hpc_mat)

hpc_mat %>% head(3)

hpc_byAdopant = data.frame(hpc = predict_pc_composites$predictedPC, 
                           A1 = rep(A1_vector, each = num_B1_vector*num_B_fraction*num_B_vector*num_A_fraction))
hpc_byAdopant$A1 = as.factor(hpc_byAdopant$A1)

# median value for each dopant
(hpc_median_Al = median(subset(hpc_byAdopant, A1=='Al')$hpc))
(hpc_median_Ca = median(subset(hpc_byAdopant, A1=='Ca')$hpc))
(hpc_median_Ba = median(subset(hpc_byAdopant, A1=='Ba')$hpc))
(hpc_median_Mg = median(subset(hpc_byAdopant, A1=='Mg')$hpc))
median_mat = matrix(c(hpc_median_Al, hpc_median_Ca, hpc_median_Ba, hpc_median_Mg), ncol= 4, nrow = 1)
colnames(median_mat) = c('Al', 'Ba', 'Ca', 'Mg')
rownames(median_mat) = c('Median')
median_table = as.table(median_mat)



theme_set(theme_minimal())
violin_plt_A1 <- ggplot(hpc_byAdopant, aes(x = A1, y = hpc)) +
  geom_violin(aes(color = A1, fill = A1),
              trim = FALSE,
              alpha = 0.9, 
              show.legend = FALSE) +
  # new_scale_fill() +
  geom_boxplot(width = 0.1, fill = "#EFCB68", color = "#8F2D56", alpha = 0.8) +
  labs(x = "A-site dopant", y = "Predicted HPC", title = "Violin Plot with Box Plot") +
  theme(axis.text = element_text(size = 12, color = "black"),
        axis.title=element_text(size=13),
        #panel.grid= element_blank(),
        plot.background = element_rect(fill = "white", color = NA),
        legend.background = element_rect(fill="transparent", linewidth = 0.3),
        legend.box.background = element_rect(colour = NA),
        # legend.position = c(0.58,0.90),
        #plot.margin = margin(20, 20, 20, 20),
        panel.border = element_rect(colour = "black", fill=NA, size=0.8)
        ) 
  
violin_plt_A1

#
ggsave(
  filename = "violin_plt_A1_rf_500_023_0714_20240808tune.tiff",
  plot = violin_plt_A1,
  width = 18,  # <=19.05cm
  height = 15, # <=22.225cm
  units= "cm",
  dpi= 300,
  compression = "lzw")









