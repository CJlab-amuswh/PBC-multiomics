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
nfeature_values <- c(1,2,3,5,7,10)

####### 循环运行不同的 nfeature ##############
for (nfeature in nfeature_values) {
 

####
  annotation = read.xlsx("./Data/Clinical laboratory tests ZD-2025-1-16.xlsx",
                         rowNames = FALSE)
  annotation = subset(annotation, annotation$`UDCA-response_ALD` %in% c("BR","BNR"))
  annotation$`UDCA-response_ALD` <- gsub("-", "_", annotation$`UDCA-response_ALD`)
  
  row.names(annotation) = annotation$sample
  
  annotation_dis = subset(annotation, cohort == "discovery")
  annotation_vali = subset(annotation, cohort == "validation")
  
  discovery_pro = read.table("./Data/XA04703B2_DA_median_KNN_combat_discovery.txt", header = 1, row.names = 1)
  validation_pro = read.table("./Data/XA04703B2_DA_median_KNN_combat_validation.txt", header = 1, row.names = 1)
  
  discovery_IGP = read.table("./Data/XA04703B2_LPIg_median_KNN_combat_discovery.txt", header = 1, row.names = 1)
  validation_IGP = read.table("./Data/XA04703B2_LPIg_median_KNN_combat_validation.txt", header = 1, row.names = 1)
  
  
  discovery_combine = bind_rows(discovery_pro, discovery_IGP)
  validation_combine = bind_rows(validation_pro, validation_IGP)
  
  
  features_DA = read.xlsx("./Data/XA04703B2 DA UDCA DEP.xlsx", sheet = 2)
  features_DA = subset(features_DA, Regulated.Type %in% c("Up", "Down"))
  features_DA = features_DA$Protein.accession
  
  features_LPIg = read.xlsx("./Data/XA04703B2 LPIg UDCA DEP.xlsx", sheet = 2)
  features_LPIg = subset(features_LPIg, Regulated.Type %in% c("Up", "Down"))
  features_LPIg$Glyco_peptide = paste0(features_LPIg$Protein.accession,"_",
                                       features_LPIg$Amino.acid,features_LPIg$Position,"_",
                                       features_LPIg$Observed.Modification)
  features_LPIg = features_LPIg$Glyco_peptide
  
  
  features = c(features_DA)
  #features = intersect(features,rownames(validation_DA))
  
  discovery_combine = discovery_combine[features,]
  
  
  matrix_clean = as.data.frame(t(discovery_combine))
  features = colnames(matrix_clean)
  
  
  matrix_clean_group = merge(annotation_dis[c("UDCA-response_ALD")], matrix_clean, by = "row.names")
  rownames(matrix_clean_group) = matrix_clean_group$Row.names
  matrix_clean_group = matrix_clean_group[-1]
  matrix_clean_group$`UDCA-response_ALD` =  as.factor(matrix_clean_group$`UDCA-response_ALD`)
  matrix_clean_group <- matrix_clean_group %>%
    dplyr::rename(group = `UDCA-response_ALD`)
  
  
  validation_group = merge(annotation_vali[c("UDCA-response_ALD")], t(validation_combine[features,]), by = "row.names")
  rownames(validation_group) = validation_group$Row.names
  validation_group = validation_group[-1]
  validation_group$`UDCA-response_ALD` =  as.factor(validation_group$`UDCA-response_ALD`)
  validation_group <- validation_group %>%
    dplyr::rename(group = `UDCA-response_ALD`)
  
  table(validation_group$group)
  table(matrix_clean_group$group)
  
  discovery_final = na.omit(matrix_clean_group)
  validation_final = na.omit(validation_group)
  
  table(discovery_final$group)
  table(validation_final$group)
  
  
  discovery_final[-1] = scale(discovery_final[-1])
  validation_final[-1] = scale(validation_final[-1])

  # 创建每个 nfeature 的文件夹
  result_dir = paste0(dirname(rstudioapi::getSourceEditorContext()$path),"/nfeature_", nfeature)
  dir.create(result_dir, showWarnings = FALSE)
  setwd(result_dir)
  
  ##### 获得2000种特征组合 ######
  source("./bin/combo_generate.R")
  #features = features_omics
  combo_result <- combo_generate(features = features, nfeatures = nfeature,
                                 ncombo = min(choose(length(features), nfeature), 2000))
  unique_combinations <- combo_result[["unique_combinations"]]
  
  #clinical_combinations <- paste(sort(features_cli$features), collapse = "-")
  #unique_combinations = paste0(clinical_combinations, "-", unique_combinations)
  
  combo_df <- combo_result[["combo_df"]]
  
  # 保存特征组合到对应文件夹
  write.table(unique_combinations, "RF_unique_combinations.txt",
              row.names = TRUE, col.names = NA, sep = "\t")
  
  source("./bin/RF_modeling_evaluation_update.R")
  
  # 模型评估
  Modeling_result = Modeling_evaluation(discovery = discovery_final,
                                        valiData = validation_final,
                                        smote = TRUE,
                                        unique_combinations = unique_combinations,
                                        p = 1,
                                        RFE = FALSE,
                                        metric = "AUC",
                                        rds = "RF_nfeatures.rds")
  
  
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
      mean_AUC = mean(AUC,na.rm = TRUE),
      mean_F1 = mean(F1,na.rm = TRUE)
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
  
  
  source("./bin/ROC_CM_train_validation.R")
  source("./bin/plot_roc_avg_train.R")
  source("./bin/plot_roc_avg.R")
  
  train_ROC_avg <- plot_roc_with_auc_train(trainData, model_tuned_best, group_column = colnames(trainData)[1])
  vali_ROC_avg <- plot_roc_with_auc(validation_group, model_tuned_best, group_column = colnames(trainData)[1])
  
  combined_plot_val <- grid.arrange(train_ROC, train_ROC_avg$plot, train_CM_plot,
                                    #test_ROC, test_ROC_avg$plot, test_CM_plot,
                                    vali_ROC, vali_ROC_avg$plot, vali_CM_plot, ncol = 3)
  
  ggsave("RF_nfeatures_performance.pdf",
         plot = combined_plot_val, width = 9.5, height = 6)
 
  RF_prob_vali = vali_ROC_avg$prob
  write.table(RF_prob_vali,  "RF_vali_prob.txt",
              row.names = TRUE, col.names = NA)
  RF_prob_train = train_ROC_avg$prob
  write.table(RF_prob_train,  "RF_train_prob.txt",
              row.names = TRUE, col.names = NA)

  source("./bin/RF_XGB_SVM_GLM_performance train-vali.R")
  
  
}


