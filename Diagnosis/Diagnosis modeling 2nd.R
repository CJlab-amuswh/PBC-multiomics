rm(list=ls())

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

# 定义要运行的 nfeature 值
nfeature_values <- c(1)

####### 循环运行不同的 nfeature ##############
for (nfeature in nfeature_values) {
 

####
annotation = read.xlsx("C:/Users/WXH-PC/Desktop/20240918 final data/Clinical laboratory tests ZD-2024-10-29.xlsx",
                       rowNames = FALSE)
row.names(annotation) = annotation$sample

annotation_dis = subset(annotation, cohort == "discovery")
annotation_vali = subset(annotation, cohort == "validation")

discovery_pro = read.table("C:/Users/WXH-PC/Desktop/20240918 final data/Diagnosis/DA/XA04703B2_DA_median_KNN_combat_discovery.txt", header = 1, row.names = 1)
validation_pro = read.table("C:/Users/WXH-PC/Desktop/20240918 final data/Diagnosis/DA/XA04703B2_DA_median_KNN_combat_validation.txt", header = 1, row.names = 1)


both_sample_dis = (colnames(discovery_pro))
both_sample_vali = (colnames(validation_pro))

discovery_combine = rbind(discovery_pro[both_sample_dis])
validation_combine = rbind(validation_pro[both_sample_vali])


sample_dis = intersect(rownames(annotation_dis), colnames(discovery_combine))
sample_vali = intersect(rownames(annotation_vali), colnames(validation_combine))

discovery_combine_scale = as.data.frame(t(scale(t(discovery_combine[sample_dis]))))
validation_combine_scale = as.data.frame(t(scale(t(validation_combine[sample_vali]))))

annotation_dis = annotation_dis[sample_dis,]
annotation_vali = annotation_vali[sample_vali,]

####
features_DA = read.table("./Data/DA_feature_combine.txt", header = 1, row.names = NULL)

features_DA = features_DA$features

features = features_DA

discovery_combine_scale = discovery_combine_scale[features,]


######剔除高相关特征##########

matrix_clean = as.data.frame(t(discovery_combine_scale))


matrix_clean_group = cbind(annotation_dis["Group"], matrix_clean)
matrix_clean_group$Group =  as.factor(matrix_clean_group$Group)
matrix_clean_group <- matrix_clean_group %>%
  dplyr::rename(group = Group)



validation_group = cbind(annotation_vali["Group"], as.data.frame(t(validation_combine_scale)))
validation_group$Group =  as.factor(validation_group$Group)
validation_group <- validation_group %>%
  dplyr::rename(group = Group)

table(validation_group$group)
table(matrix_clean_group$group)



  # 创建每个 nfeature 的文件夹
  result_dir <- paste0("nfeature_", nfeature)
  dir.create(result_dir, showWarnings = FALSE)
  setwd(result_dir)
  
  ##### 获得2000种特征组合 ######
  source("./bin/combo_generate.R")
  
  combo_result <- combo_generate(features = features, nfeatures = nfeature,
                                 ncombo = min(choose(length(features), nfeature), 2000))
  unique_combinations <- combo_result[["unique_combinations"]]
  combo_df <- combo_result[["combo_df"]]
  
  # 保存特征组合到对应文件夹
  write.table(unique_combinations, "RF_unique_combinations_DA.txt",
              row.names = TRUE, col.names = NA, sep = "\t")
  
  source("./bin/multi_RF_modeling_evaluation_update.R")
  
  # 模型评估
  Modeling_result <- Modeling_evaluation(discovery = matrix_clean_group,
                                         valiData = validation_group,
                                         smote = TRUE,
                                         unique_combinations = unique_combinations,
                                         p = 0.7,
                                         RFE = FALSE,
                                         metric = "AUC",
                                         rds =  "RF_nfeatures.rds")
  
  
  ggsave("RF_RFE_result.pdf", 
         plot = Modeling_result$RFE_plot, width = 4, height = 3)
  
  # 保存模型性能评估结果
  model_performance <- Modeling_result$results_df
  write.table(model_performance, "RF_nfeatures_performance_table.txt",
              row.names = TRUE, col.names = NA, sep = "\t")

  
  # 性能汇总
  performance_summary <- model_performance %>%
    group_by(i) %>%
    summarise(
      mean_AUC = mean(AUC),
      mean_F1 = mean(Mean_F1)
    ) %>%
    ungroup()
  
  max_AUC_i <- filter(performance_summary, mean_AUC == max(mean_AUC))$i
  model_tuned_best <- Modeling_result[["models"]][[max_AUC_i]]
  
  # 保存调优的特征
  tuned_feature <- colnames(model_tuned_best[["ptype"]]) 
  write.table(tuned_feature,  "RF_tuned_features.txt",
              row.names = FALSE, col.names = "features")
  
  # 生成 ROC 曲线
  trainData <- Modeling_result$trainData
  testData <- Modeling_result$testData
  valiData <- Modeling_result$valiData
  validation_group = Modeling_result$valiData
  
  
  source("./bin/ROC_CM_train_test_validation.R")
  source("./bin/plot_roc_avg_train.R")
  source("./bin/plot_roc_avg.R")
  
  train_ROC_avg <- plot_roc_with_auc_train(trainData, model_tuned_best, group_column = colnames(trainData)[1])
  test_ROC_avg <- plot_roc_with_auc(testData, model_tuned_best, group_column = colnames(trainData)[1])
  vali_ROC_avg <- plot_roc_with_auc(validation_group, model_tuned_best, group_column = colnames(trainData)[1])
  
  combined_plot_val <- grid.arrange(train_ROC, train_ROC_avg$plot, train_CM_plot,
                                    test_ROC, test_ROC_avg$plot, test_CM_plot,
                                    vali_ROC, vali_ROC_avg$plot, vali_CM_plot, ncol = 3)
  
  ggsave("RF_nfeatures_performance.pdf",
         plot = combined_plot_val, width = 9.5, height = 9)
  
  RF_prob_test = test_ROC_avg$prob
  RF_prob_vali = vali_ROC_avg$prob
  RF_prob = rbind(RF_prob_test,RF_prob_vali)
  
  write.table(RF_prob,  "RF_test_vali_prob.txt",
              row.names = TRUE, col.names = NA)

  source("./bin/Diagnosis_subgroup_performance")
  
  
}



#########################################
########################################
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

# 定义要运行的 nfeature 值
nfeature_values <- c(2,3,5,7,10)

####### 循环运行不同的 nfeature ##############
for (nfeature in nfeature_values) {
  
  
  ####
  annotation = read.xlsx("C:/Users/WXH-PC/Desktop/20240918 final data/Clinical laboratory tests ZD-2024-10-29.xlsx",
                         rowNames = FALSE)
  row.names(annotation) = annotation$sample
  
  annotation_dis = subset(annotation, cohort == "discovery")
  annotation_vali = subset(annotation, cohort == "validation")
  
  discovery_pro = read.table("C:/Users/WXH-PC/Desktop/20240918 final data/Diagnosis/DA/XA04703B2_DA_median_KNN_combat_discovery.txt", header = 1, row.names = 1)
  validation_pro = read.table("C:/Users/WXH-PC/Desktop/20240918 final data/Diagnosis/DA/XA04703B2_DA_median_KNN_combat_validation.txt", header = 1, row.names = 1)
  
  
  both_sample_dis = (colnames(discovery_pro))
  both_sample_vali = (colnames(validation_pro))
  
  discovery_combine = rbind(discovery_pro[both_sample_dis])
  validation_combine = rbind(validation_pro[both_sample_vali])
  
  
  sample_dis = intersect(rownames(annotation_dis), colnames(discovery_combine))
  sample_vali = intersect(rownames(annotation_vali), colnames(validation_combine))
  
  discovery_combine_scale = as.data.frame(t(scale(t(discovery_combine[sample_dis]))))
  validation_combine_scale = as.data.frame(t(scale(t(validation_combine[sample_vali]))))
  
  annotation_dis = annotation_dis[sample_dis,]
  annotation_vali = annotation_vali[sample_vali,]
  
  ####
  features_DA = read.table("C:/Users/WXH-PC/Desktop/20240918 final data/20241121 diagnosis/DA/DA_feature_combine.txt", header = 1, row.names = NULL)
  
  features_DA = features_DA$features
  
  features = features_DA
  
  discovery_combine_scale = discovery_combine_scale[features,]
  
  
  ######剔除高相关特征##########
  
  matrix_clean = as.data.frame(t(discovery_combine_scale))
  
  
  matrix_clean_group = cbind(annotation_dis["Group"], matrix_clean)
  matrix_clean_group$Group =  as.factor(matrix_clean_group$Group)
  matrix_clean_group <- matrix_clean_group %>%
    dplyr::rename(group = Group)
  
  
  
  validation_group = cbind(annotation_vali["Group"], as.data.frame(t(validation_combine_scale)))
  validation_group$Group =  as.factor(validation_group$Group)
  validation_group <- validation_group %>%
    dplyr::rename(group = Group)
  
  table(validation_group$group)
  table(matrix_clean_group$group)
  
  
  
  
  
  # 创建每个 nfeature 的文件夹
  result_dir <- paste0("nfeature_", nfeature)
  dir.create(result_dir, showWarnings = FALSE)
  setwd(result_dir)
  
  ##### 获得1000种特征组合 ######
  source("./bin/combo_generate.R")
  
  combo_result <- combo_generate(features = features, nfeatures = nfeature,
                                 ncombo = min(choose(length(features), nfeature), 2000))
  unique_combinations <- combo_result[["unique_combinations"]]
  combo_df <- combo_result[["combo_df"]]
  
  # 保存特征组合到对应文件夹
  write.table(unique_combinations, "RF_unique_combinations_DA.txt",
              row.names = TRUE, col.names = NA, sep = "\t")
  
  source("./bin/multi_RF_modeling_evaluation_update.R")
  
  # 模型评估
  Modeling_result <- Modeling_evaluation(discovery = matrix_clean_group,
                                         valiData = validation_group,
                                         smote = TRUE,
                                         unique_combinations = unique_combinations,
                                         p = 0.7,
                                         RFE = TRUE,
                                         metric = "AUC",
                                         rds =  "RF_nfeatures.rds")
  
  
  ggsave("RF_RFE_result.pdf", 
         plot = Modeling_result$RFE_plot, width = 4, height = 3)
  
  # 保存模型性能评估结果
  model_performance <- Modeling_result$results_df
  write.table(model_performance, "RF_nfeatures_performance_table.txt",
              row.names = TRUE, col.names = NA, sep = "\t")
  
  
  # 性能汇总
  performance_summary <- model_performance %>%
    group_by(i) %>%
    summarise(
      mean_AUC = mean(AUC),
      mean_F1 = mean(Mean_F1)
    ) %>%
    ungroup()
  
  max_AUC_i <- filter(performance_summary, mean_AUC == max(mean_AUC))$i
  model_tuned_best <- Modeling_result[["models"]][[max_AUC_i]]
  
  # 保存调优的特征
  tuned_feature <- colnames(model_tuned_best[["ptype"]]) 
  write.table(tuned_feature,  "RF_tuned_features.txt",
              row.names = FALSE, col.names = "features")
  
  # 生成 ROC 曲线
  trainData <- Modeling_result$trainData
  testData <- Modeling_result$testData
  valiData <- Modeling_result$valiData
  validation_group = Modeling_result$valiData
  
  
  source("./bin/ROC_CM_train_test_validation.R")
  source("./bin/plot_roc_avg_train.R")
  source("./bin/plot_roc_avg.R")
  
  train_ROC_avg <- plot_roc_with_auc_train(trainData, model_tuned_best, group_column = colnames(trainData)[1])
  test_ROC_avg <- plot_roc_with_auc(testData, model_tuned_best, group_column = colnames(trainData)[1])
  vali_ROC_avg <- plot_roc_with_auc(validation_group, model_tuned_best, group_column = colnames(trainData)[1])
  
  combined_plot_val <- grid.arrange(train_ROC, train_ROC_avg$plot, train_CM_plot,
                                    test_ROC, test_ROC_avg$plot, test_CM_plot,
                                    vali_ROC, vali_ROC_avg$plot, vali_CM_plot, ncol = 3)
  
  ggsave("RF_nfeatures_performance.pdf",
         plot = combined_plot_val, width = 9.5, height = 9)
  
  RF_prob_test = test_ROC_avg$prob
  RF_prob_vali = vali_ROC_avg$prob
  RF_prob = rbind(RF_prob_test,RF_prob_vali)
  
  write.table(RF_prob,  "RF_test_vali_prob.txt",
              row.names = TRUE, col.names = NA)
  
  source("./bin/Diagnosis_subgroup_performance")
  
  
}





