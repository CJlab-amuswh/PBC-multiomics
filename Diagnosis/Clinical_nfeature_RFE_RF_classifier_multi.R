rm(list=ls())
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

library(dplyr)
library(tidyr)
library(glmnet)
library(caret)
library(e1071)
library(pROC)
library(openxlsx)
library(ggthemes)
library(tidyverse)
library(gridExtra)
library(ggpubr)
library(future)
library(randomForest)
library(ROSE)
library(ComplexHeatmap)
library(UBL)
library(caret)
library(readxl)
library(multiROC)


######## 1. ########
annotation = read.xlsx("C:/Users/WXH-PC/Desktop/20240918 final data/Clinical laboratory tests ZD-2024-10-29.xlsx",
                       rowNames = FALSE)
row.names(annotation) = annotation$sample

annotation <- annotation %>% dplyr::rename(group = Group)
annotation <- annotation %>% dplyr::rename(`AMA_M2` = `AMA-M2`)


clinical_features = c("ALP","GGT","ALT","AST","ALB","GLO","Age","Sex","BMI","DBIL","AMA_M2")

annotation = annotation[c("group","cohort",clinical_features)]

annotation[-c(1:2)] = apply(annotation[-c(1:2)],2,as.numeric)

annotation_dis = subset(annotation, cohort == "discovery")
annotation_vali = subset(annotation, cohort == "validation")

annotation_dis_num = as.matrix(annotation_dis[-c(1:2)])
annotation_vali_num = as.matrix(annotation_vali[-c(1:2)])

annotation_dis_impute = impute::impute.knn(annotation_dis_num,k = 10, rowmax = 0.5, colmax = 0.8, rng.seed=2024)
annotation_dis_impute = as.data.frame(annotation_dis_impute[["data"]])

annotation_vali_impute = impute::impute.knn(annotation_vali_num,k = 10, rowmax = 0.5, colmax = 0.8, rng.seed=2024)
annotation_vali_impute = as.data.frame(annotation_vali_impute[["data"]])


annotation_dis_final = cbind(annotation_dis[1],annotation_dis_impute)
  
annotation_vali_final = cbind(annotation_vali[1],annotation_vali_impute)

write.table(annotation_dis_final,"sample_clinical_discovery_knn.txt",row.names = TRUE, col.names = NA, sep = "\t")
write.table(annotation_vali_final,"sample_clinical_validation_knn.txt",row.names = TRUE, col.names = NA, sep = "\t")


annotation_dis_final$group = as.factor(annotation_dis_final$group)
annotation_vali_final$group = as.factor(annotation_vali_final$group)


###穷举 ###
unique_combinations <- unlist(
  lapply(1:length(clinical_features), function(i) {
    combn(clinical_features, i, function(x) paste(x, collapse = "-"))
  })
)

write.table(unique_combinations,"RF_unique_combinations_clinical.txt",
            row.names = TRUE,col.names = NA, sep = "\t")


###### 建模和评估 #######
source("C:/Users/WXH-PC/Desktop/20240918 final data/models for CJ/multi_RF_modeling_evaluation_update.R")
table(annotation_dis$group)

Modeling_result = Modeling_evaluation(discovery = annotation_dis_final,
                                      valiData = annotation_vali_final,
                                      smote = TRUE,
                                      unique_combinations = unique_combinations,
                                      p = 0.7,
                                      RFE = FALSE,
                                      metric = "AUC",
                                      rds = "RF_clinical_RFE_1.rds")

model_performance = Modeling_result$results_df 

Modeling_result$RFE_result
ggsave("RF_RFE_result_1.pdf", Modeling_result$RFE_result, width = 4, height = 3)

write.table(model_performance,"RF_nfeatures_performance_table_clinical.txt",
            row.names = TRUE,col.names = NA, sep = "\t")

##########模型性能评估###############################

performance_summary <- model_performance %>%
  group_by(i) %>%
  summarise(
    mean_AUC = mean(AUC),
    mean_F1 = mean(Mean_F1)
  ) %>%
  ungroup()

max_AUC_i <- filter(performance_summary,mean_AUC == max(mean_AUC))$i
max_AUC_i

model_tuned_best = Modeling_result[["models"]][[max_AUC_i]]
tuned_feature = colnames(model_tuned_best[["ptype"]]) 

write.table(tuned_feature,"RF_tuned_features_1.txt",row.names = FALSE,col.names = "features")

trainData = Modeling_result$trainData
testData = Modeling_result$testData
validation_group = annotation_vali_final
valiData = annotation_vali_final

source("C:/Users/WXH-PC/Desktop/20240918 final data/models for CJ/ROC_CM_train_test_validation.R")
source("C:/Users/WXH-PC/Desktop/20240918 final data/models for CJ/plot_roc_avg_train.R")
source("C:/Users/WXH-PC/Desktop/20240918 final data/models for CJ/plot_roc_avg.R")

train_ROC_avg <- plot_roc_with_auc_train(trainData, model_tuned_best, group_column = colnames(trainData)[1])
test_ROC_avg <- plot_roc_with_auc(testData, model_tuned_best, group_column = colnames(trainData)[1])
vali_ROC_avg <- plot_roc_with_auc(validation_group, model_tuned_best, group_column = colnames(trainData)[1])

combined_plot_val <- grid.arrange(train_ROC, train_ROC_avg$plot, train_CM_plot,
                                  test_ROC, test_ROC_avg$plot, test_CM_plot,
                                  vali_ROC,vali_ROC_avg$plot, vali_CM_plot,ncol =3)

ggsave("RF_nfeatures_performance_1.pdf",plot = combined_plot_val, width = 9.5, height = 9)




