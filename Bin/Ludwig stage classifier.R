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
  annotation = read.xlsx("./Data/Clinical laboratory tests ZD-2024-10-29.xlsx",
                         rowNames = FALSE)
  annotation = subset(annotation, annotation$Ludw_ALD %in% c("B-LI","B-LII"))
  annotation$Ludw_ALD <- gsub("-", "_", annotation$Ludw_ALD)
  
  annotation$group = annotation$Ludw_ALD
  row.names(annotation) = annotation$sample
  
  annotation_dis = subset(annotation, cohort == "discovery")
  annotation_vali = subset(annotation, cohort == "validation")
  
  discovery_DA = read.table("./Data/XA04703B2_DA_median_KNN_combat_discovery.txt", header = 1, row.names = 1)
  validation_DA = read.table("./Data/XA04703B2_DA_median_KNN_combat_validation.txt", header = 1, row.names = 1)
  
  sample_dis = intersect(rownames(annotation_dis), colnames(discovery_DA))
  sample_vali = intersect(rownames(annotation_vali), colnames(validation_DA))
  
  discovery_DA_scale = as.data.frame(t(scale(t(discovery_DA[sample_dis]))))
  validation_DA_scale = as.data.frame(t(scale(t(validation_DA[sample_vali]))))
  
  annotation_dis = annotation_dis[sample_dis,]
  annotation_vali = annotation_vali[sample_vali,]
  
  discovery_combine_scale = discovery_DA_scale
  validation_combine_scale = validation_DA_scale
  ########
  
  features_DA = read.table("XA04703B2 DA discovery CTR-BLI-BLII DEP result.txt",header = 1)
  features_DA = subset(features_DA, features_DA$B.LII.B.LI_Type %in% c("up", "down"))
  features_DA = subset(features_DA, features_DA$B.LII.B.LI_ratio > 1.5 | features_DA$B.LII.B.LI_ratio < 2/3)
  
  features_omics = c(features_DA$X)
  
  discovery_final = cbind(annotation_dis[c("group")],
                          as.data.frame(t(discovery_combine_scale[features_omics,])))
  
  validation_final = cbind(annotation_vali[c("group")],
                           as.data.frame(t(validation_combine_scale[features_omics,])))
  
  discovery_final$group = as.factor(discovery_final$group)
  validation_final$group = as.factor(validation_final$group)
  
  table(discovery_final$group)
  table(validation_final$group)
  
  discovery_final = na.omit(discovery_final)
  validation_final = na.omit(validation_final)


  # 创建每个 nfeature 的文件夹
  result_dir = paste0(dirname(rstudioapi::getSourceEditorContext()$path),"/nfeature_", nfeature)
  dir.create(result_dir, showWarnings = FALSE)
  setwd(result_dir)
  
  ##### 获得2000种特征组合 ######
  source("./bin/combo_generate.R")
  features = features_omics
  combo_result <- combo_generate(features = features, nfeatures = nfeature,
                                 ncombo = min(choose(length(features), nfeature), 2000))
  unique_combinations <- combo_result[["unique_combinations"]]
  combo_df <- combo_result[["combo_df"]]
  
  # 保存特征组合到对应文件夹
  write.table(unique_combinations, "RF_unique_combinations.txt",
              row.names = TRUE, col.names = NA, sep = "\t")
  
  source("./bin/RF_modeling_evaluation_update.R")
  
  # 模型评估
  Modeling_result = Modeling_evaluation(discovery = discovery_final,
                                        valiData = validation_final,
                                        smote = FALSE,
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
  filtered_model_performance <- model_performance %>%
    filter(cohort %in% c("Train", "Validation")) %>%
    select(i, cohort, AUC) %>%
    pivot_wider(names_from = cohort, values_from = AUC, ) %>%
    filter(
      #Train - Test > -0.05,
      Train - Validation > -0.05
    )
  
  model_performance = subset(model_performance, i %in% filtered_model_performance$i)
  model_performance = subset(model_performance, model_performance$cohort %in% c("Train", "Validation"))
  
  # 初始筛选条件
  thresholds <- c(0.05, 0.1, 0.15, 0.25, 0.5)
  
  # 循环尝试不同的筛选条件
  performance_summary <- NULL
  
  for (threshold in thresholds) {
    performance_summary <- model_performance %>%
      group_by(i) %>%
      summarise(
        mean_AUC = mean(AUC),
        sd_AUC = sd(AUC),
        CV_AUC = sd_AUC / mean_AUC,
        mean_prAUC = mean(Accuracy)
      ) %>%
      filter(CV_AUC <= threshold) %>% # 使用当前阈值筛选
      ungroup()
    
    # 如果筛选结果不为空，则停止循环
    if (nrow(performance_summary) > 0) {
      message(paste("筛选标准为 CV_AUC <=", threshold, "时，结果不为空"))
      break
    }
  }
  
  # 如果所有阈值都无法筛选到非空结果
  if (is.null(performance_summary) || nrow(performance_summary) == 0) {
    message("所有筛选标准均未获得结果，请检查数据")
  }
  
  
  max_AUC_i <- filter(performance_summary, mean_AUC == max(mean_AUC))$i
  model_tuned_best <- Modeling_result[["models"]][[max_AUC_i]]
  
  
  # 保存调优的特征
  tuned_feature <- colnames(model_tuned_best[["ptype"]]) 
  write.table(tuned_feature,  "RF_tuned_features.txt",
              row.names = FALSE, col.names = "features")
  
  # 生成 ROC 曲线
  trainData <- Modeling_result$trainData
  #testData <- Modeling_result$testData
  valiData <- Modeling_result$valiData
  validation_group = Modeling_result$valiData
  
  
  source("./bin/ROC_CM_train_validation.R")
  source("./bin/plot_roc_avg_train.R")
  source("./bin/plot_roc_avg.R")
  
  train_ROC_avg <- plot_roc_with_auc_train(trainData, model_tuned_best, group_column = colnames(trainData)[1])
  #test_ROC_avg <- plot_roc_with_auc(testData, model_tuned_best, group_column = colnames(trainData)[1])
  vali_ROC_avg <- plot_roc_with_auc(validation_group, model_tuned_best, group_column = colnames(trainData)[1])
  
  combined_plot_val <- grid.arrange(train_ROC, train_ROC_avg$plot, train_CM_plot,
                                    #test_ROC, test_ROC_avg$plot, test_CM_plot,
                                    vali_ROC, vali_ROC_avg$plot, vali_CM_plot, ncol = 3)
  
  ggsave("RF_nfeatures_performance.pdf",
         plot = combined_plot_val, width = 9.5, height = 6)
  
  #RF_prob_test = test_ROC_avg$prob
  RF_prob_vali = vali_ROC_avg$prob
  RF_prob = RF_prob_vali
  #RF_prob = rbind(RF_prob_test,RF_prob_vali)
  
  write.table(RF_prob,  "RF_vali_prob.txt",
              row.names = TRUE, col.names = NA)

  source("./bin/RF_XGB_SVM_GLM_performance train-vali.R")
  
  
}

