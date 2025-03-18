library(viridis)
library(gridExtra)

### I. 预测在训练集上的概率值#####
train_predictions <- model_tuned_best[["pred"]]
multi_roc_train <- multiclass.roc(train_predictions$obs, train_predictions)

AUROC_train = multi_roc_train[["auc"]]
AUROC_train
# 遍历每个类别的 ROC 曲线，并将数据逐个添加到 roc_data 中
roc_data_train <- data.frame()

for (i in c(1:length(multi_roc_train[["rocs"]]))) {
  roc <- multi_roc_train[["rocs"]][[i]][[1]]
  roc_df <- data.frame(Specificity = roc$specificities, 
                       Sensitivity = roc$sensitivities,
                       Group = names(multi_roc_train[["rocs"]][i])
  )  # 使用 names() 获取类别的名称
  roc_data_train <- rbind(roc_data_train, roc_df)
}

# 排序
roc_data_train <- roc_data_train[order(roc_data_train$Sensitivity,roc_data_train$Group, decreasing = FALSE),]
#####绘制训练集AUROC 曲线####
train_ROC = ggplot(roc_data_train, aes(x = 1-Specificity, y = Sensitivity)) +
  geom_line(aes(color = Group), size = 1) +
  geom_segment(aes(x = 0, y = 0, xend = 1, yend = 1), 
               linetype = "dashed", color = "black") +
  theme_bw()+
  annotate("text", x = 0.8, y = 0.2, label = paste("AUROC =", round(AUROC_train, 3)))+
  scale_color_viridis_d()+
  theme(legend.position="none")
train_ROC
#ggsave("train ROC.pdf", width = 4, height = 4)
#ggsave("train ROC.pdf", width = 4, height = 4)

##绘制混线矩阵
confusion_matrix_train <-confusionMatrix(model_tuned_best, "none")
confusion_matrix_train

CM_table_train = confusion_matrix_train$table
CM_table_train = as.data.frame(CM_table_train)

# 绘制热图
CM_table_train$Reference = factor(CM_table_train$Reference, 
                                  #levels = c("Good", "Poor")
                                  )

train_CM_plot = ggplot(CM_table_train, aes(x = Reference, y = Prediction, fill = Freq))+
  geom_tile(color = "black", size = 0.5)+###设置边界线的颜色和大小
  scale_fill_gradient(low = "white", high = "skyblue")+
  theme_update()+
  ggtitle("Train set")+
  geom_text(aes(label = Freq), color = "black", vjust = 0.5, size = 5) +
  theme(panel.background = element_rect(fill = "transparent", color = NA))
train_CM_plot
#ggsave("confusion_matrix_train.pdf",width = 4.5, height = 3.5)


### II. 预测在测试集上的概率值#####
test_pred_prob <- predict(model_tuned_best, testData[,tuned_feature,drop = FALSE], type = "prob")
test_pred_prob

# # 计算每个类别相对于其他类别的 ROC 曲线
# roc_curves <- lapply(1:nlevels(testData$group), function(i) {
#   multiclass.roc(testData$group == levels(testData$group)[i], test_pred_prob[, i])
# })

# 计算平均 ROC 曲线
multi_roc_test <- multiclass.roc(response = testData$group, predictor = test_pred_prob)
AUROC_test = multi_roc_test[["auc"]]
AUROC_test
# 遍历每个类别的 ROC 曲线，并将数据逐个添加到 roc_data 中
roc_data_test <- data.frame()

for (i in c(1:length(multi_roc_test[["rocs"]]))) {
  roc <- multi_roc_test[["rocs"]][[i]][[1]]
  roc_df <- data.frame(Specificity = roc$specificities, 
                       Sensitivity = roc$sensitivities,
                       Group = names(multi_roc_test[["rocs"]][i])
  )
  roc_data_test <- rbind(roc_data_test, roc_df)
}

# 排序
roc_data_test <- roc_data_test[order(roc_data_test$Sensitivity,roc_data_test$Group, decreasing = FALSE),]


####绘制测试集AUROC 曲线#####
test_ROC = ggplot(roc_data_test, aes(x = 1-Specificity, y = Sensitivity)) +
  geom_line(aes(color = Group), size = 1) +
  geom_segment(aes(x = 0, y = 0, xend = 1, yend = 1), 
               linetype = "dashed", color = "black") +
  theme_bw()+
  scale_color_viridis_d()+
  annotate("text", x = 0.8, y = 0.2, label = paste("AUROC =", round(AUROC_test, 3)))+
  theme(legend.position="none")
test_ROC
#ggsave("test ROC.pdf", width = 4, height = 4)


##绘制混线矩阵
test_pred <- factor(colnames(test_pred_prob)[apply(test_pred_prob, 1, which.max)]
                    #,levels = c("Good", "Poor")
                    )
#testData$group <- factor(testData$group, levels(trainData_optimal$group))
confusion_matrix_test <- confusionMatrix(test_pred, reference = testData$group)
confusion_matrix_test

CM_table_test = confusion_matrix_test$table
CM_table_test = as.data.frame(CM_table_test)

# 绘制热图
CM_table_test$Reference = factor(CM_table_test$Reference, 
                                 #levels = c("Good", "Poor")
                                 )

test_CM_plot = ggplot(CM_table_test, aes(x = Reference, y = Prediction, fill = Freq))+
  geom_tile(color = "black", size = 0.5)+###设置边界线的颜色和大小
  scale_fill_gradient(low = "white", high = "skyblue")+
  theme_update()+
  ggtitle("Test set")+
  geom_text(aes(label = Freq), color = "black", vjust = 0.5, size = 5) +
  theme(panel.background = element_rect(fill = "transparent", color = NA))

#ggsave("confusion_matrix_test.pdf",width = 4.5, height = 3.5)





### III. 预测在验证集上的概率值#####


validation_prep = valiData[c("group",intersect(tuned_feature,colnames(valiData)))]


pred_vali <- predict(model_tuned_best, newdata = validation_prep[tuned_feature],type = "raw")
pred_vali_prob <- predict(model_tuned_best, newdata = validation_prep[tuned_feature], type = "prob")

pred_vali_result = cbind(pred_vali,pred_vali_prob)
row.names(pred_vali_result) = row.names(valiData)
table(pred_vali_result$pred_vali)


pred_vali_result = cbind(pred_vali_result,valiData[1])
#write.table(pred_vali_result,file = "prediction 36 geo.txt",row.names=TRUE,sep = "\t", col.names = NA)  




##### 绘制验证集AUROC 曲线#####
# 计算平均 ROC 曲线
multi_roc_vali <- multiclass.roc(response = validation_prep[,1], predictor = pred_vali_prob)
AUROC_vali = multi_roc_vali[["auc"]]
AUROC_vali
# 遍历每个类别的 ROC 曲线，并将数据逐个添加到 roc_data 中
roc_data_vali <- data.frame()
for (i in c(1:length(multi_roc_vali[["rocs"]]))) {
  roc <- multi_roc_vali[["rocs"]][[i]][[1]]
  roc_df <- data.frame(Specificity = roc$specificities, 
                       Sensitivity = roc$sensitivities,
                       Group = names(multi_roc_vali[["rocs"]][i])
  )
  roc_data_vali <- rbind(roc_data_vali, roc_df)
}

# 排序
roc_data_vali <- roc_data_vali[order(roc_data_vali$Sensitivity,roc_data_vali$Group, decreasing = FALSE),]

# 绘图
vali_ROC = ggplot(roc_data_vali, aes(x = 1-Specificity, y = Sensitivity)) +
  geom_line(aes(color = Group), size = 1) +
  geom_segment(aes(x = 0, y = 0, xend = 1, yend = 1),
               linetype = "dashed", color = "black") +
  theme_bw()+
  scale_color_viridis_d()+
  annotate("text", x = 0.8, y = 0.2, label = paste("AUROC =", round(AUROC_vali, 3)))+
  theme(legend.position="none")


vali_ROC

# ggsave("./validation_ROC.pdf", width = 4, height = 4)

##绘制混淆矩阵
vali_pred <- factor(colnames(pred_vali_prob)[apply(pred_vali_prob, 1, which.max)]
                    #,levels = c("Good", "Poor")
)
#validation_prep$group <- factor(validation_prep$group, levels(trainData_optimal$group))
validation_prep$group <- factor(validation_prep$group)

confusion_matrix_vali <- confusionMatrix(vali_pred, reference = validation_prep$group)
confusion_matrix_vali

CM_table_vali = confusion_matrix_vali$table
CM_table_vali = as.data.frame(CM_table_vali)

# 绘制热图
CM_table_vali$Reference = factor(CM_table_vali$Reference, 
                                 #levels = c("Good", "Poor")
)

vali_CM_plot = ggplot(CM_table_vali, aes(x = Reference, y = Prediction, fill = Freq))+
  geom_tile(color = "black", size = 0.5)+###设置边界线的颜色和大小
  scale_fill_gradient(low = "white", high = "skyblue")+
  theme_update()+
  ggtitle("Validation set")+
  geom_text(aes(label = Freq), color = "black", vjust = 0.5, size = 5) +
  theme(panel.background = element_rect(fill = "transparent", color = NA))

# ggsave("confusion_matrix_vali.pdf",width = 4.5, height = 3.5)


combined_plot_val <- grid.arrange(train_ROC, train_CM_plot, test_ROC, test_CM_plot,vali_ROC,vali_CM_plot,ncol =2)
combined_plot_val


