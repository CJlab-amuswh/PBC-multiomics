# require(devtools)
# install_github("WandeRum/multiROC")
# require(multiROC)
library(multiROC)

mycolor = RColorBrewer::brewer.pal(5, 'Set1')
mycolor2 = colorRampPalette(mycolor)(10)



plot_roc_with_auc <- function(IHC_Data, svm_model, group_column) {
  # 确保 subtype 列为因子类型并获取类别名称
  # IHC_Data = testData[sample_M2_neg_test,]
  # svm_model = model_tuned_best
  # group_column = colnames(trainData)[1]
  # IHC_Data = testData
  # svm_model = model_tuned_best
  # group_column = colnames(trainData)[1]
  
  
  colnames(IHC_Data)[1] <- "subtype"
  IHC_Data$subtype <- factor(IHC_Data$subtype)
  class_names <- levels(IHC_Data$subtype)
  
  # 使用模型进行预测
  IHC_pred_prob <- predict(svm_model, IHC_Data[, tuned_feature,drop = FALSE], type = "prob")
  IHC_pred_prob <- data.frame(IHC_pred_prob)
  
  # 重命名预测概率列为 xxx_pred_model，按照 class_names 的顺序
  IHC_pred_prob <- IHC_pred_prob[, class_names]
  colnames(IHC_pred_prob) <- paste(class_names, "_pred_model", sep = "")
  
  # 将真实标签进行独热编码
  dummy_vars <- dummyVars(~subtype, data = IHC_Data)
  true_label <- predict(dummy_vars, newdata = IHC_Data)
  true_label <- data.frame(true_label)
  
  # 重命名真实标签列
  colnames(true_label) <- gsub("subtype\\.", "", colnames(true_label))
  colnames(true_label) <- paste(colnames(true_label), "_true", sep = "")
  
  # 合并真实标签和预测结果
  final_df <- cbind(true_label, IHC_pred_prob)
  rownames(final_df) = rownames(IHC_Data)
  # 计算多类别ROC曲线和PR曲线
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
    geom_line(size = 1)+
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




