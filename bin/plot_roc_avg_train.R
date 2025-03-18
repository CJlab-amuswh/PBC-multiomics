library(pROC)
library(ggplot2)
library(RColorBrewer)
library(caret)

mycolor = RColorBrewer::brewer.pal(5, 'Set1')
mycolor2 = colorRampPalette(mycolor)(10)


plot_roc_with_auc_train <- function(IHC_Data, svm_model, group_column) {
  # 确保 group 列为因子类型并获取类别名称
  colnames(IHC_Data)[1] <- "group"
  IHC_Data$group <- factor(IHC_Data$group)
  
  class_names <- levels(IHC_Data$group)
  
  # 使用模型的预测结果
  train_predictions <- svm_model[["pred"]]
  train_predictions = train_predictions[order(train_predictions$rowIndex),]
  
  
  # 将真实标签进行独热编码
  dummy_vars <- dummyVars(~obs, data = train_predictions)
  true_label <- predict(dummy_vars, newdata = train_predictions)
  true_label <- data.frame(true_label)
  
  # 重命名真实标签列
  colnames(true_label) <- gsub("obs\\.", "", colnames(true_label))
  colnames(true_label) <- paste(colnames(true_label), "_true", sep = "")
  
  # 预测结果，确保列名顺序与 class_names 一致
  pred <- train_predictions[, class_names, drop = FALSE]
  colnames(pred) <- paste(class_names, "_pred_model", sep = "")
  
  # 合并真实标签和预测结果
  final_df <- cbind(true_label, pred)
  rownames(final_df) = rownames(IHC_Data)
  # 计算多类别ROC曲线
  roc_res <- multi_roc(final_df, force_diag = FALSE)
  
  # 提取绘图数据
  plot_roc_df <- plot_roc_data(roc_res)
  
  # 提取AUC值
  auc_values <- as.numeric(roc_res$AUC$model)
  
  # 创建AUC标签
  auc_labels <- paste0(c(class_names, "Macro", "Micro"), " (AUC: ", round(auc_values, 3), ")")
  
  # 设置 Group 列的因子水平，确保顺序一致
  plot_roc_df$Group <- factor(plot_roc_df$Group, levels = c(class_names, "Macro", "Micro"))
  
  # 确保颜色向量长度足够
  mycolor2 <- colorRampPalette(mycolor)(length(class_names) + 2)
  
  # 绘制ROC曲线并添加AUC标注
  p <- ggplot(plot_roc_df, aes(x = 1 - Specificity, y = Sensitivity, color = Group)) +
    geom_line(size = 1) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +
    scale_color_manual(values = mycolor2, labels = auc_labels) +
    theme_bw() +
    theme(
      legend.justification = c(1, 0), 
      legend.position = c(1, 0), 
      legend.text = element_text(size = 8), 
      legend.title = element_text(size = 8),
      legend.key.size = unit(0.3, "cm"), 
      legend.spacing.y = unit(0.1, "cm"),
      legend.background = element_rect(fill = NA)
    )
  
  auc_df <- data.frame(Group = c(class_names, "Macro", "Micro"), AUC = auc_values)
  
  return(list(plot = p, auc_data = auc_df, prob = final_df))
}


# 示例调用
# IHC_Data <- trainData  # 确保 trainData 已定义
# svm_model <- model_tuned_best # 确保 svm_tuned 已定义
# group_column <- colnames(trainData)[1]
# p1 <- plot_roc_with_auc_train(IHC_Data, svm_model, group_column = group_column)
# ggsave("train_ROC_score.pdf", plot = p1, width = 4, height = 4)
