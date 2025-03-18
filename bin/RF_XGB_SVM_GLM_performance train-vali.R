rm(list=ls())


####### 1-0 modeling ########
model_performance = read.table("RF_nfeatures_performance_table.txt", header = 1)

# 定义一个函数来逐步放宽条件
filter_with_fallback <- function(data, initial_threshold = -0.05, fallback_thresholds = c(-0.1, -0.2,-0.5)) {
  # 初始过滤条件
  filtered_data <- data %>%
    filter(Train - Validation > initial_threshold)
  
  # 如果初始条件过滤后没有结果，逐步放宽条件
  if (nrow(filtered_data) == 0) {
    for (threshold in fallback_thresholds) {
      filtered_data <- data %>%
        filter(Train - Validation > threshold)
      
      # 如果放宽条件后有结果，退出循环
      if (nrow(filtered_data) > 0) {
        break
      }
    }
  }
  
  return(filtered_data)
}

# 使用函数进行过滤
filtered_model_performance <- model_performance %>%
  filter(cohort %in% c("Train", "Test", "Validation")) %>%
  select(i, cohort, AUC) %>%
  pivot_wider(names_from = cohort, values_from = AUC) %>%
  filter_with_fallback()

# 更新 model_performance
model_performance <- subset(model_performance, i %in% filtered_model_performance$i)

# 性能汇总
# 初始筛选条件
thresholds <- c(0.05, 0.1, 0.15,0.2,0.25,1)

# 循环尝试不同的筛选条件
performance_summary <- NULL

for (threshold in thresholds) {
  performance_summary <- model_performance %>%
    group_by(i) %>%
    summarise(
      mean_AUC = mean(AUC,na.rm = TRUE),
      sd_AUC = sd(AUC,na.rm = TRUE),
      CV_AUC = sd_AUC / mean_AUC,
      mean_prAUC = mean(Accuracy,na.rm = TRUE)
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

max_AUC_i <- min(filter(performance_summary,mean_AUC == max(mean_AUC))$i)
max_AUC_i

Modeling_result = readRDS("RF_nfeatures.rds")

trainData = Modeling_result$trainData
testData = Modeling_result$testData
valiData = Modeling_result$valiData
validation_group = valiData

model_tuned_best = Modeling_result[["models"]][[max_AUC_i]]
tuned_feature = names(model_tuned_best$ptype)

ggsave("rf_RFE_result.pdf", Modeling_result$RFE_plot, width = 4, height = 3)


write.table(tuned_feature,"rf_tuned_features.txt",row.names = FALSE,col.names = "features")

source("D:/PTMBIO/XA04703/20240918 final data/models for CJ/ROC_CM_train_validation.R")
source("D:/PTMBIO/XA04703/20240918 final data/models for CJ/plot_roc_avg_train.R")
source("D:/PTMBIO/XA04703/20240918 final data/models for CJ/plot_roc_avg.R")

trainData = Modeling_result$trainData

train_ROC_avg <- plot_roc_with_auc_train(trainData, model_tuned_best, group_column = colnames(trainData)[1])
vali_ROC_avg <- plot_roc_with_auc(validation_group, model_tuned_best, group_column = colnames(trainData)[1])

combined_plot_val <- grid.arrange(train_ROC, train_ROC_avg$plot, train_CM_plot,
                                  vali_ROC,vali_ROC_avg$plot, vali_CM_plot,ncol =3)

ggsave("rf_nfeatures_performance.pdf",plot = combined_plot_val, width = 9.5, height = 6)

####### 2-0 modeling ########
discovery_final = rbind(trainData, testData)
validation_final = valiData

source("D:/PTMBIO/XA04703/20240918 final data/models for CJ/glm_modeling_evaluation_update.R")
unique_combinations <- paste(sort(tuned_feature), collapse = "-")  # 组合并排序为字符串

Modeling_result = Modeling_evaluation(discovery = discovery_final,
                                      valiData = validation_final,
                                      smote = FALSE,
                                      unique_combinations = unique_combinations,
                                      p = 1,
                                      RFE = FALSE,
                                      metric = "AUC",
                                      rds = "glm_nfeatures.rds")

model_performance = Modeling_result$results_df 

ggsave("glm_RFE_result.pdf", Modeling_result$RFE_result, width = 4, height = 3)

write.table(model_performance,"glm_nfeatures_performance_table.txt",
            row.names = TRUE,col.names = NA, sep = "\t")


performance_summary <- model_performance %>%
  group_by(i) %>%
  summarise(
    mean_AUC = mean(AUC,na.rm = TRUE),
    F1 = mean(F1,na.rm = TRUE)
  ) %>%
  ungroup()

max_AUC_i <- filter(performance_summary,mean_AUC == max(mean_AUC))$i
max_AUC_i



model_tuned_best = Modeling_result[["models"]][[max_AUC_i]]
tuned_feature = colnames(model_tuned_best[["ptype"]]) 
write.table(tuned_feature,"glm_tuned_features.txt",row.names = FALSE,col.names = "features")

trainData = Modeling_result$trainData
testData = Modeling_result$testData

valiData = Modeling_result$valiData
validation_group = valiData

source("D:/PTMBIO/XA04703/20240918 final data/models for CJ/ROC_CM_train_validation.R")
source("D:/PTMBIO/XA04703/20240918 final data/models for CJ/plot_roc_avg_train.R")
source("D:/PTMBIO/XA04703/20240918 final data/models for CJ/plot_roc_avg.R")

trainData = Modeling_result$trainData

train_ROC_avg <- plot_roc_with_auc_train(trainData, model_tuned_best, group_column = colnames(trainData)[1])
vali_ROC_avg <- plot_roc_with_auc(validation_group, model_tuned_best, group_column = colnames(trainData)[1])

combined_plot_val <- grid.arrange(train_ROC, train_ROC_avg$plot, train_CM_plot,
                                  #test_ROC, test_ROC_avg$plot, test_CM_plot,
                                  vali_ROC,vali_ROC_avg$plot, vali_CM_plot,ncol =3)

ggsave("glm_nfeatures_performance.pdf",plot = combined_plot_val, width = 9.5, height = 6)





####### 3-0 modeling ########
discovery_final = rbind(trainData, testData)
validation_final = valiData

source("D:/PTMBIO/XA04703/20240918 final data/models for CJ/svm_modeling_evaluation_update.R")
unique_combinations <- paste(sort(tuned_feature), collapse = "-")  # 组合并排序为字符串

Modeling_result = Modeling_evaluation(discovery = discovery_final,
                                      valiData = validation_final,
                                      smote = FALSE,
                                      unique_combinations = unique_combinations,
                                      p = 1,
                                      RFE = FALSE,
                                      metric = "AUC",
                                      rds = "svm_nfeatures.rds")

model_performance = Modeling_result$results_df 

ggsave("svm_RFE_result.pdf", Modeling_result$RFE_result, width = 4, height = 3)

write.table(model_performance,"svm_nfeatures_performance_table.txt",
            row.names = TRUE,col.names = NA, sep = "\t")


performance_summary <- model_performance %>%
  group_by(i) %>%
  summarise(
    mean_AUC = mean(AUC,na.rm = TRUE),
    F1 = mean(F1,na.rm = TRUE)
  ) %>%
  ungroup()

max_AUC_i <- filter(performance_summary,mean_AUC == max(mean_AUC))$i
max_AUC_i



model_tuned_best = Modeling_result[["models"]][[max_AUC_i]]
tuned_feature = colnames(model_tuned_best[["ptype"]]) 
write.table(tuned_feature,"svm_tuned_features.txt",row.names = FALSE,col.names = "features")

trainData = Modeling_result$trainData
testData = Modeling_result$testData

valiData = Modeling_result$valiData
validation_group = valiData

source("D:/PTMBIO/XA04703/20240918 final data/models for CJ/ROC_CM_train_validation.R")
source("D:/PTMBIO/XA04703/20240918 final data/models for CJ/plot_roc_avg_train.R")
source("D:/PTMBIO/XA04703/20240918 final data/models for CJ/plot_roc_avg.R")

trainData = Modeling_result$trainData

train_ROC_avg <- plot_roc_with_auc_train(trainData, model_tuned_best, group_column = colnames(trainData)[1])
vali_ROC_avg <- plot_roc_with_auc(validation_group, model_tuned_best, group_column = colnames(trainData)[1])

combined_plot_val <- grid.arrange(train_ROC, train_ROC_avg$plot, train_CM_plot,
                                  #test_ROC, test_ROC_avg$plot, test_CM_plot,
                                  vali_ROC,vali_ROC_avg$plot, vali_CM_plot,ncol =3)

ggsave("svm_nfeatures_performance.pdf",plot = combined_plot_val, width = 9.5, height = 6)




####### 4-0 modeling ########
discovery_final = rbind(trainData, testData)
validation_final = valiData

source("D:/PTMBIO/XA04703/20240918 final data/models for CJ/xgb_modeling_evaluation_update.R")
unique_combinations <- paste(sort(tuned_feature), collapse = "-")  # 组合并排序为字符串

Modeling_result = Modeling_evaluation(discovery = discovery_final,
                                      valiData = validation_final,
                                      smote = FALSE,
                                      unique_combinations = unique_combinations,
                                      p = 1,
                                      RFE = FALSE,
                                      metric = "AUC",
                                      rds = "xgb_nfeatures.rds")

model_performance = Modeling_result$results_df 

ggsave("xgb_RFE_result.pdf", Modeling_result$RFE_result, width = 4, height = 3)

write.table(model_performance,"xgb_nfeatures_performance_table.txt",
            row.names = TRUE,col.names = NA, sep = "\t")


performance_summary <- model_performance %>%
  group_by(i) %>%
  summarise(
    mean_AUC = mean(AUC,na.rm = TRUE),
    F1 = mean(F1,na.rm = TRUE)
  ) %>%
  ungroup()

max_AUC_i <- filter(performance_summary,mean_AUC == max(mean_AUC))$i
max_AUC_i



model_tuned_best = Modeling_result[["models"]][[max_AUC_i]]
tuned_feature = colnames(model_tuned_best[["ptype"]]) 
write.table(tuned_feature,"xgb_tuned_features.txt",row.names = FALSE,col.names = "features")

trainData = Modeling_result$trainData
testData = Modeling_result$testData

valiData = Modeling_result$valiData
validation_group = valiData

source("D:/PTMBIO/XA04703/20240918 final data/models for CJ/ROC_CM_train_validation.R")
source("D:/PTMBIO/XA04703/20240918 final data/models for CJ/plot_roc_avg_train.R")
source("D:/PTMBIO/XA04703/20240918 final data/models for CJ/plot_roc_avg.R")

trainData = Modeling_result$trainData

train_ROC_avg <- plot_roc_with_auc_train(trainData, model_tuned_best, group_column = colnames(trainData)[1])
vali_ROC_avg <- plot_roc_with_auc(validation_group, model_tuned_best, group_column = colnames(trainData)[1])

combined_plot_val <- grid.arrange(train_ROC, train_ROC_avg$plot, train_CM_plot,
                                  #test_ROC, test_ROC_avg$plot, test_CM_plot,
                                  vali_ROC,vali_ROC_avg$plot, vali_CM_plot,ncol =3)

ggsave("xgb_nfeatures_performance.pdf",plot = combined_plot_val, width = 9.5, height = 6)



