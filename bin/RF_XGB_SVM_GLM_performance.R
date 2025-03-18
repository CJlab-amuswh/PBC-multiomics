rm(list=ls())

####### 1-0 modeling ########
Modeling_result = readRDS("RF_nfeatures.rds")


model_performance = read.table("RF_nfeatures_performance_table.txt", header = 1)

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
tuned_feature = names(model_tuned_best$ptype)


trainData = Modeling_result$trainData
testData = Modeling_result$testData
valiData = Modeling_result$valiData
validation_group = valiData


source("C:/Users/WXH-PC/Desktop/20240918 final data/models for CJ/ROC_CM_train_test_validation.R")
source("C:/Users/WXH-PC/Desktop/20240918 final data/models for CJ/plot_roc_avg_train.R")
source("C:/Users/WXH-PC/Desktop/20240918 final data/models for CJ/plot_roc_avg.R")


####### 1-1 AMA-M2 ########
annotation = read.xlsx("C:/Users/WXH-PC/Desktop/20240918 final data/Clinical laboratory tests ZD-2024-10-29.xlsx",
                       rowNames = TRUE)


sample_M2_pos = c(rownames(subset(annotation,annotation$Group == "CTR")),
                  rownames(subset(annotation,annotation$`AMA-M2` == 1)))

sample_M2_neg = c(rownames(subset(annotation,annotation$Group == "CTR")),
                  rownames(subset(annotation,annotation$`AMA-M2` == 0)))

sample_M2_pos_train = intersect(sample_M2_pos, rownames(trainData))
sample_M2_neg_train = intersect(sample_M2_neg, rownames(trainData))

sample_M2_pos_test = intersect(sample_M2_pos, rownames(testData))
sample_M2_neg_test = intersect(sample_M2_neg, rownames(testData))

sample_M2_pos_test = intersect(sample_M2_pos, rownames(testData))
sample_M2_neg_test = intersect(sample_M2_neg, rownames(testData))

sample_M2_pos_vali = intersect(sample_M2_pos, rownames(valiData))
sample_M2_neg_vali = intersect(sample_M2_neg, rownames(valiData))


test_ROC_avg <- plot_roc_with_auc(testData[sample_M2_pos_test,], model_tuned_best, group_column = colnames(trainData)[1])
vali_ROC_avg <- plot_roc_with_auc(valiData[sample_M2_pos_vali,], model_tuned_best, group_column = colnames(trainData)[1])

combined_plot_val <- grid.arrange(test_ROC, test_ROC_avg$plot, test_CM_plot,
                                  vali_ROC,vali_ROC_avg$plot, vali_CM_plot,ncol =3)
ggsave("RF_nfeatures_performance M2_pos.pdf",plot = combined_plot_val, width = 9.5, height = 6)


test_ROC_avg <- plot_roc_with_auc(testData[sample_M2_neg_test,], model_tuned_best, group_column = colnames(trainData)[1])
vali_ROC_avg <- plot_roc_with_auc(valiData[sample_M2_neg_vali,], model_tuned_best, group_column = colnames(trainData)[1])

combined_plot_val <- grid.arrange(test_ROC, test_ROC_avg$plot, test_CM_plot,
                                  vali_ROC,vali_ROC_avg$plot, vali_CM_plot,ncol =3)

ggsave("RF_nfeatures_performance M2_neg.pdf",plot = combined_plot_val, width = 9.5, height = 6)

####### 1-2 Luwdwig ########
annotation = read.xlsx("C:/Users/WXH-PC/Desktop/20240918 final data/Clinical laboratory tests ZD-2024-10-29.xlsx",
                       rowNames = FALSE)
row.names(annotation) = annotation$sample
annotation <- annotation %>% dplyr::rename(`AMA_M2` = `AMA-M2`)
annotation_Lud = annotation[c("Group","Ludw_ALD")]

sample_BLI = c(rownames(subset(annotation_Lud,annotation_Lud$Group %in% c("CTR","A","AB"))),
               rownames(subset(annotation_Lud,annotation_Lud$Ludw_ALD == "B-LI")))

sample_BLII = c(rownames(subset(annotation_Lud,annotation_Lud$Group %in% c("CTR","A","AB"))),
                rownames(subset(annotation_Lud,annotation_Lud$Ludw_ALD == "B-LII")))

sample_BLI_train = intersect(sample_BLI, rownames(trainData))
sample_BLII_train = intersect(sample_BLII, rownames(trainData))

sample_BLI_test = intersect(sample_BLI, rownames(testData))
sample_BLII_test = intersect(sample_BLII, rownames(testData))

sample_BLI_test = intersect(sample_BLI, rownames(testData))
sample_BLII_test = intersect(sample_BLII, rownames(testData))

sample_BLI_vali = intersect(sample_BLI, rownames(valiData))
sample_BLII_vali = intersect(sample_BLII, rownames(valiData))


test_ROC_avg <- plot_roc_with_auc(testData[sample_BLI_test,], model_tuned_best, group_column = colnames(trainData)[1])
vali_ROC_avg <- plot_roc_with_auc(valiData[sample_BLI_vali,], model_tuned_best, group_column = colnames(trainData)[1])

combined_plot_val <- grid.arrange(test_ROC, test_ROC_avg$plot, test_CM_plot,
                                  vali_ROC,vali_ROC_avg$plot, vali_CM_plot,ncol =3)
ggsave("RF_nfeatures_performance BLI.pdf",plot = combined_plot_val, width = 9.5, height = 6)


test_ROC_avg <- plot_roc_with_auc(testData[sample_BLII_test,], model_tuned_best, group_column = colnames(trainData)[1])
vali_ROC_avg <- plot_roc_with_auc(valiData[sample_BLII_vali,], model_tuned_best, group_column = colnames(trainData)[1])

combined_plot_val <- grid.arrange(test_ROC, test_ROC_avg$plot, test_CM_plot,
                                  vali_ROC,vali_ROC_avg$plot, vali_CM_plot,ncol =3)

ggsave("RF_nfeatures_performance BLII.pdf",plot = combined_plot_val, width = 9.5, height = 6)


####### 1-3 subgroup ########

annotation = read.xlsx("C:/Users/WXH-PC/Desktop/20240918 final data/Clinical laboratory tests ZD-2024-10-29.xlsx",
                       rowNames = FALSE)
row.names(annotation) = annotation$sample

source("C:/Users/WXH-PC/Desktop/20240918 final data/models for CJ/ROC_CM_train_test_validation.R")
source("C:/Users/WXH-PC/Desktop/20240918 final data/models for CJ/plot_roc_avg_train.R")
source("C:/Users/WXH-PC/Desktop/20240918 final data/models for CJ/plot_roc_avg.R")

sample_subgroup_BP = unique(c(rownames(subset(annotation,annotation$Group %in% c("CTR","A","AB"))),
                              rownames(subset(annotation,!(annotation$Subgroup %in% c("BN"))))))

sample_subgroup_BN = unique(c(rownames(subset(annotation,annotation$Group %in% c("CTR","A","AB"))),
                              rownames(subset(annotation,!(annotation$Subgroup %in% c("BP"))))))


sample_subgroup_BP_train = intersect(sample_subgroup_BP, rownames(trainData))
sample_subgroup_BN_train = intersect(sample_subgroup_BN, rownames(trainData))

sample_subgroup_BP_test = intersect(sample_subgroup_BP, rownames(testData))
sample_subgroup_BN_test = intersect(sample_subgroup_BN, rownames(testData))

sample_subgroup_BP_vali = intersect(sample_subgroup_BP, rownames(valiData))
sample_subgroup_BN_vali = intersect(sample_subgroup_BN, rownames(valiData))



test_ROC_avg <- plot_roc_with_auc(testData[sample_subgroup_BP_test,], model_tuned_best, group_column = colnames(trainData)[1])
vali_ROC_avg <- plot_roc_with_auc(valiData[sample_subgroup_BP_vali,], model_tuned_best, group_column = colnames(trainData)[1])

combined_plot_val <- grid.arrange(test_ROC, test_ROC_avg$plot, test_CM_plot,
                                  vali_ROC,vali_ROC_avg$plot, vali_CM_plot,ncol =3)
ggsave("RF_nfeatures_performance subgroup_BP.pdf",plot = combined_plot_val, width = 9.5, height = 6)


test_ROC_avg <- plot_roc_with_auc(testData[sample_subgroup_BN_test,], model_tuned_best, group_column = colnames(trainData)[1])
vali_ROC_avg <- plot_roc_with_auc(valiData[sample_subgroup_BN_vali,], model_tuned_best, group_column = colnames(trainData)[1])

combined_plot_val <- grid.arrange(test_ROC, test_ROC_avg$plot, test_CM_plot,
                                  vali_ROC,vali_ROC_avg$plot, vali_CM_plot,ncol =3)

ggsave("RF_nfeatures_performance subgroup_BN.pdf",plot = combined_plot_val, width = 9.5, height = 6)

####### 1-4   UDCA response ########

annotation = read.xlsx("C:/Users/WXH-PC/Desktop/20240918 final data/Clinical laboratory tests ZD-2024-10-29.xlsx",
                       rowNames = FALSE)
row.names(annotation) = annotation$sample

source("C:/Users/WXH-PC/Desktop/20240918 final data/models for CJ/ROC_CM_train_test_validation.R")
source("C:/Users/WXH-PC/Desktop/20240918 final data/models for CJ/plot_roc_avg_train.R")
source("C:/Users/WXH-PC/Desktop/20240918 final data/models for CJ/plot_roc_avg.R")

sample_UDCA_BR = unique(c(rownames(subset(annotation,annotation$Group %in% c("CTR","A","AB"))),
                              rownames(subset(annotation,!(annotation$`UDCA-response_ALD` %in% c("BR"))))))

sample_UDCA_BNR = unique(c(rownames(subset(annotation,annotation$Group %in% c("CTR","A","AB"))),
                              rownames(subset(annotation,!(annotation$`UDCA-response_ALD` %in% c("BNR"))))))


sample_UDCA_BR_train = intersect(sample_UDCA_BR, rownames(trainData))
sample_UDCA_BNR_train = intersect(sample_UDCA_BNR, rownames(trainData))

sample_UDCA_BR_test = intersect(sample_UDCA_BR, rownames(testData))
sample_UDCA_BNR_test = intersect(sample_UDCA_BNR, rownames(testData))

sample_UDCA_BR_vali = intersect(sample_UDCA_BR, rownames(valiData))
sample_UDCA_BNR_vali = intersect(sample_UDCA_BNR, rownames(valiData))



test_ROC_avg <- plot_roc_with_auc(testData[sample_UDCA_BR_test,], model_tuned_best, group_column = colnames(trainData)[1])
vali_ROC_avg <- plot_roc_with_auc(valiData[sample_UDCA_BR_vali,], model_tuned_best, group_column = colnames(trainData)[1])

combined_plot_val <- grid.arrange(test_ROC, test_ROC_avg$plot, test_CM_plot,
                                  vali_ROC,vali_ROC_avg$plot, vali_CM_plot,ncol =3)
ggsave("RF_nfeatures_performance UDCA_BR.pdf",plot = combined_plot_val, width = 9.5, height = 6)


test_ROC_avg <- plot_roc_with_auc(testData[sample_UDCA_BNR_test,], model_tuned_best, group_column = colnames(trainData)[1])
vali_ROC_avg <- plot_roc_with_auc(valiData[sample_UDCA_BNR_vali,], model_tuned_best, group_column = colnames(trainData)[1])

combined_plot_val <- grid.arrange(test_ROC, test_ROC_avg$plot, test_CM_plot,
                                  vali_ROC,vali_ROC_avg$plot, vali_CM_plot,ncol =3)

ggsave("RF_nfeatures_performance UDCA_BNR.pdf",plot = combined_plot_val, width = 9.5, height = 6)

####### 2-0 modeling ########
discovery_final = rbind(trainData, testData)
validation_final = valiData

source("C:/Users/WXH-PC/Desktop/20240918 final data/models for CJ/multi_glm_modeling_evaluation_update.R")
unique_combinations <- paste(sort(tuned_feature), collapse = "-")  # 组合并排序为字符串

Modeling_result = Modeling_evaluation(discovery = discovery_final,
                                      valiData = validation_final,
                                      smote = FALSE,
                                      unique_combinations = unique_combinations,
                                      p = 0.7,
                                      RFE = FALSE,
                                      metric = "AUC",
                                      rds = "glm_nfeatures.rds")

model_performance = Modeling_result$results_df 

# RFE_plot = ggplot(Modeling_result$RFE_result) + theme_bw()
# ggsave("glm_RFE_result.pdf", RFE_plot, width = 4, height = 3)

write.table(model_performance,"glm_nfeatures_performance_table.txt",
            row.names = TRUE,col.names = NA, sep = "\t")


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
write.table(tuned_feature,"glm_tuned_features.txt",row.names = FALSE,col.names = "features")

trainData = Modeling_result$trainData
testData = Modeling_result$testData

valiData = Modeling_result$valiData
validation_group = valiData

source("C:/Users/WXH-PC/Desktop/20240918 final data/models for CJ/ROC_CM_train_test_validation.R")
source("C:/Users/WXH-PC/Desktop/20240918 final data/models for CJ/plot_roc_avg_train.R")
source("C:/Users/WXH-PC/Desktop/20240918 final data/models for CJ/plot_roc_avg.R")

trainData = Modeling_result$trainData

train_ROC_avg <- plot_roc_with_auc_train(trainData, model_tuned_best, group_column = colnames(trainData)[1])
test_ROC_avg <- plot_roc_with_auc(testData, model_tuned_best, group_column = colnames(trainData)[1])
vali_ROC_avg <- plot_roc_with_auc(validation_group, model_tuned_best, group_column = colnames(trainData)[1])

combined_plot_val <- grid.arrange(train_ROC, train_ROC_avg$plot, train_CM_plot,
                                  test_ROC, test_ROC_avg$plot, test_CM_plot,
                                  vali_ROC,vali_ROC_avg$plot, vali_CM_plot,ncol =3)

ggsave("glm_nfeatures_performance.pdf",plot = combined_plot_val, width = 9.5, height = 9)

glm_prob_test = test_ROC_avg$prob
glm_prob_vali = vali_ROC_avg$prob
glm_prob = rbind(glm_prob_test,glm_prob_vali)

write.table(glm_prob, "glm_test_vali_prob.txt",
            row.names = TRUE, col.names = NA)

####### 2-1 AMA-M2 ########
annotation = read.xlsx("C:/Users/WXH-PC/Desktop/20240918 final data/Clinical laboratory tests ZD-2024-10-29.xlsx",
                       rowNames = TRUE)


sample_M2_pos = c(rownames(subset(annotation,annotation$Group == "CTR")),
                  rownames(subset(annotation,annotation$`AMA-M2` == 1)))

sample_M2_neg = c(rownames(subset(annotation,annotation$Group == "CTR")),
                  rownames(subset(annotation,annotation$`AMA-M2` == 0)))

sample_M2_pos_train = intersect(sample_M2_pos, rownames(trainData))
sample_M2_neg_train = intersect(sample_M2_neg, rownames(trainData))

sample_M2_pos_test = intersect(sample_M2_pos, rownames(testData))
sample_M2_neg_test = intersect(sample_M2_neg, rownames(testData))

sample_M2_pos_test = intersect(sample_M2_pos, rownames(testData))
sample_M2_neg_test = intersect(sample_M2_neg, rownames(testData))

sample_M2_pos_vali = intersect(sample_M2_pos, rownames(valiData))
sample_M2_neg_vali = intersect(sample_M2_neg, rownames(valiData))


test_ROC_avg <- plot_roc_with_auc(testData[sample_M2_pos_test,], model_tuned_best, group_column = colnames(trainData)[1])
vali_ROC_avg <- plot_roc_with_auc(valiData[sample_M2_pos_vali,], model_tuned_best, group_column = colnames(trainData)[1])

combined_plot_val <- grid.arrange(test_ROC, test_ROC_avg$plot, test_CM_plot,
                                  vali_ROC,vali_ROC_avg$plot, vali_CM_plot,ncol =3)
ggsave("GLM_nfeatures_performance M2_pos.pdf",plot = combined_plot_val, width = 9.5, height = 6)


test_ROC_avg <- plot_roc_with_auc(testData[sample_M2_neg_test,], model_tuned_best, group_column = colnames(trainData)[1])
vali_ROC_avg <- plot_roc_with_auc(valiData[sample_M2_neg_vali,], model_tuned_best, group_column = colnames(trainData)[1])

combined_plot_val <- grid.arrange(test_ROC, test_ROC_avg$plot, test_CM_plot,
                                  vali_ROC,vali_ROC_avg$plot, vali_CM_plot,ncol =3)

ggsave("GLM_nfeatures_performance M2_neg.pdf",plot = combined_plot_val, width = 9.5, height = 6)

####### 2-2 Luwdwig ########
annotation = read.xlsx("C:/Users/WXH-PC/Desktop/20240918 final data/Clinical laboratory tests ZD-2024-10-29.xlsx",
                       rowNames = FALSE)
row.names(annotation) = annotation$sample
annotation <- annotation %>% dplyr::rename(`AMA_M2` = `AMA-M2`)
annotation_Lud = annotation[c("Group","Ludw_ALD")]

sample_BLI = c(rownames(subset(annotation_Lud,annotation_Lud$Group %in% c("CTR","A","AB"))),
               rownames(subset(annotation_Lud,annotation_Lud$Ludw_ALD == "B-LI")))

sample_BLII = c(rownames(subset(annotation_Lud,annotation_Lud$Group %in% c("CTR","A","AB"))),
                rownames(subset(annotation_Lud,annotation_Lud$Ludw_ALD == "B-LII")))

sample_BLI_train = intersect(sample_BLI, rownames(trainData))
sample_BLII_train = intersect(sample_BLII, rownames(trainData))

sample_BLI_test = intersect(sample_BLI, rownames(testData))
sample_BLII_test = intersect(sample_BLII, rownames(testData))

sample_BLI_test = intersect(sample_BLI, rownames(testData))
sample_BLII_test = intersect(sample_BLII, rownames(testData))

sample_BLI_vali = intersect(sample_BLI, rownames(valiData))
sample_BLII_vali = intersect(sample_BLII, rownames(valiData))


test_ROC_avg <- plot_roc_with_auc(testData[sample_BLI_test,], model_tuned_best, group_column = colnames(trainData)[1])
vali_ROC_avg <- plot_roc_with_auc(valiData[sample_BLI_vali,], model_tuned_best, group_column = colnames(trainData)[1])

combined_plot_val <- grid.arrange(test_ROC, test_ROC_avg$plot, test_CM_plot,
                                  vali_ROC,vali_ROC_avg$plot, vali_CM_plot,ncol =3)
ggsave("GLM_nfeatures_performance BLI.pdf",plot = combined_plot_val, width = 9.5, height = 6)


test_ROC_avg <- plot_roc_with_auc(testData[sample_BLII_test,], model_tuned_best, group_column = colnames(trainData)[1])
vali_ROC_avg <- plot_roc_with_auc(valiData[sample_BLII_vali,], model_tuned_best, group_column = colnames(trainData)[1])

combined_plot_val <- grid.arrange(test_ROC, test_ROC_avg$plot, test_CM_plot,
                                  vali_ROC,vali_ROC_avg$plot, vali_CM_plot,ncol =3)

ggsave("GLM_nfeatures_performance BLII.pdf",plot = combined_plot_val, width = 9.5, height = 6)


####### 2-3 subgroup ########

annotation = read.xlsx("C:/Users/WXH-PC/Desktop/20240918 final data/Clinical laboratory tests ZD-2024-10-29.xlsx",
                       rowNames = FALSE)
row.names(annotation) = annotation$sample

source("C:/Users/WXH-PC/Desktop/20240918 final data/models for CJ/ROC_CM_train_test_validation.R")
source("C:/Users/WXH-PC/Desktop/20240918 final data/models for CJ/plot_roc_avg_train.R")
source("C:/Users/WXH-PC/Desktop/20240918 final data/models for CJ/plot_roc_avg.R")

sample_subgroup_BP = unique(c(rownames(subset(annotation,annotation$Group %in% c("CTR","A","AB"))),
                              rownames(subset(annotation,!(annotation$Subgroup %in% c("BN"))))))

sample_subgroup_BN = unique(c(rownames(subset(annotation,annotation$Group %in% c("CTR","A","AB"))),
                              rownames(subset(annotation,!(annotation$Subgroup %in% c("BP"))))))


sample_subgroup_BP_train = intersect(sample_subgroup_BP, rownames(trainData))
sample_subgroup_BN_train = intersect(sample_subgroup_BN, rownames(trainData))

sample_subgroup_BP_test = intersect(sample_subgroup_BP, rownames(testData))
sample_subgroup_BN_test = intersect(sample_subgroup_BN, rownames(testData))

sample_subgroup_BP_vali = intersect(sample_subgroup_BP, rownames(valiData))
sample_subgroup_BN_vali = intersect(sample_subgroup_BN, rownames(valiData))



test_ROC_avg <- plot_roc_with_auc(testData[sample_subgroup_BP_test,], model_tuned_best, group_column = colnames(trainData)[1])
vali_ROC_avg <- plot_roc_with_auc(valiData[sample_subgroup_BP_vali,], model_tuned_best, group_column = colnames(trainData)[1])

combined_plot_val <- grid.arrange(test_ROC, test_ROC_avg$plot, test_CM_plot,
                                  vali_ROC,vali_ROC_avg$plot, vali_CM_plot,ncol =3)
ggsave("GLM_nfeatures_performance subgroup_BP.pdf",plot = combined_plot_val, width = 9.5, height = 6)


test_ROC_avg <- plot_roc_with_auc(testData[sample_subgroup_BN_test,], model_tuned_best, group_column = colnames(trainData)[1])
vali_ROC_avg <- plot_roc_with_auc(valiData[sample_subgroup_BN_vali,], model_tuned_best, group_column = colnames(trainData)[1])

combined_plot_val <- grid.arrange(test_ROC, test_ROC_avg$plot, test_CM_plot,
                                  vali_ROC,vali_ROC_avg$plot, vali_CM_plot,ncol =3)

ggsave("GLM_nfeatures_performance subgroup_BN.pdf",plot = combined_plot_val, width = 9.5, height = 6)

####### 2-4   UDCA response ########

annotation = read.xlsx("C:/Users/WXH-PC/Desktop/20240918 final data/Clinical laboratory tests ZD-2024-10-29.xlsx",
                       rowNames = FALSE)
row.names(annotation) = annotation$sample

source("C:/Users/WXH-PC/Desktop/20240918 final data/models for CJ/ROC_CM_train_test_validation.R")
source("C:/Users/WXH-PC/Desktop/20240918 final data/models for CJ/plot_roc_avg_train.R")
source("C:/Users/WXH-PC/Desktop/20240918 final data/models for CJ/plot_roc_avg.R")

sample_UDCA_BR = unique(c(rownames(subset(annotation,annotation$Group %in% c("CTR","A","AB"))),
                          rownames(subset(annotation,!(annotation$`UDCA-response_ALD` %in% c("BR"))))))

sample_UDCA_BNR = unique(c(rownames(subset(annotation,annotation$Group %in% c("CTR","A","AB"))),
                           rownames(subset(annotation,!(annotation$`UDCA-response_ALD` %in% c("BNR"))))))


sample_UDCA_BR_train = intersect(sample_UDCA_BR, rownames(trainData))
sample_UDCA_BNR_train = intersect(sample_UDCA_BNR, rownames(trainData))

sample_UDCA_BR_test = intersect(sample_UDCA_BR, rownames(testData))
sample_UDCA_BNR_test = intersect(sample_UDCA_BNR, rownames(testData))

sample_UDCA_BR_vali = intersect(sample_UDCA_BR, rownames(valiData))
sample_UDCA_BNR_vali = intersect(sample_UDCA_BNR, rownames(valiData))



test_ROC_avg <- plot_roc_with_auc(testData[sample_UDCA_BR_test,], model_tuned_best, group_column = colnames(trainData)[1])
vali_ROC_avg <- plot_roc_with_auc(valiData[sample_UDCA_BR_vali,], model_tuned_best, group_column = colnames(trainData)[1])

combined_plot_val <- grid.arrange(test_ROC, test_ROC_avg$plot, test_CM_plot,
                                  vali_ROC,vali_ROC_avg$plot, vali_CM_plot,ncol =3)
ggsave("GLM_nfeatures_performance UDCA_BR.pdf",plot = combined_plot_val, width = 9.5, height = 6)


test_ROC_avg <- plot_roc_with_auc(testData[sample_UDCA_BNR_test,], model_tuned_best, group_column = colnames(trainData)[1])
vali_ROC_avg <- plot_roc_with_auc(valiData[sample_UDCA_BNR_vali,], model_tuned_best, group_column = colnames(trainData)[1])

combined_plot_val <- grid.arrange(test_ROC, test_ROC_avg$plot, test_CM_plot,
                                  vali_ROC,vali_ROC_avg$plot, vali_CM_plot,ncol =3)

ggsave("GLM_nfeatures_performance UDCA_BNR.pdf",plot = combined_plot_val, width = 9.5, height = 6)


####### 3-0 modeling ########
discovery_final = rbind(trainData, testData)
validation_final = valiData

source("C:/Users/WXH-PC/Desktop/20240918 final data/models for CJ/multi_svm_modeling_evaluation_update.R")
unique_combinations <- paste(sort(tuned_feature), collapse = "-")  # 组合并排序为字符串

Modeling_result = Modeling_evaluation(discovery = discovery_final,
                                      valiData = validation_final,
                                      smote = FALSE,
                                      unique_combinations = unique_combinations,
                                      p = 0.7,
                                      RFE = FALSE,
                                      metric = "AUC",
                                      rds = "svm_nfeatures.rds")

model_performance = Modeling_result$results_df 

# RFE_plot = ggplot(Modeling_result$RFE_result) + theme_bw()
# ggsave("svm_RFE_result.pdf", RFE_plot, width = 4, height = 3)

write.table(model_performance,"svm_nfeatures_performance_table.txt",
            row.names = TRUE,col.names = NA, sep = "\t")


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
write.table(tuned_feature,"svm_tuned_features.txt",row.names = FALSE,col.names = "features")

trainData = Modeling_result$trainData
testData = Modeling_result$testData

valiData = Modeling_result$valiData
validation_group = valiData

source("C:/Users/WXH-PC/Desktop/20240918 final data/models for CJ/ROC_CM_train_test_validation.R")
source("C:/Users/WXH-PC/Desktop/20240918 final data/models for CJ/plot_roc_avg_train.R")
source("C:/Users/WXH-PC/Desktop/20240918 final data/models for CJ/plot_roc_avg.R")

trainData = Modeling_result$trainData

train_ROC_avg <- plot_roc_with_auc_train(trainData, model_tuned_best, group_column = colnames(trainData)[1])
test_ROC_avg <- plot_roc_with_auc(testData, model_tuned_best, group_column = colnames(trainData)[1])
vali_ROC_avg <- plot_roc_with_auc(validation_group, model_tuned_best, group_column = colnames(trainData)[1])

combined_plot_val <- grid.arrange(train_ROC, train_ROC_avg$plot, train_CM_plot,
                                  test_ROC, test_ROC_avg$plot, test_CM_plot,
                                  vali_ROC,vali_ROC_avg$plot, vali_CM_plot,ncol =3)

ggsave("svm_nfeatures_performance.pdf",plot = combined_plot_val, width = 9.5, height = 9)

svm_prob_test = test_ROC_avg$prob
svm_prob_vali = vali_ROC_avg$prob
svm_prob = rbind(svm_prob_test,svm_prob_vali)

write.table(svm_prob,  "svm_test_vali_prob.txt",
            row.names = TRUE, col.names = NA)

####### 3-1 AMA-M2 ########
annotation = read.xlsx("C:/Users/WXH-PC/Desktop/20240918 final data/Clinical laboratory tests ZD-2024-10-29.xlsx",
                       rowNames = TRUE)


sample_M2_pos = c(rownames(subset(annotation,annotation$Group == "CTR")),
                  rownames(subset(annotation,annotation$`AMA-M2` == 1)))

sample_M2_neg = c(rownames(subset(annotation,annotation$Group == "CTR")),
                  rownames(subset(annotation,annotation$`AMA-M2` == 0)))

sample_M2_pos_train = intersect(sample_M2_pos, rownames(trainData))
sample_M2_neg_train = intersect(sample_M2_neg, rownames(trainData))

sample_M2_pos_test = intersect(sample_M2_pos, rownames(testData))
sample_M2_neg_test = intersect(sample_M2_neg, rownames(testData))

sample_M2_pos_test = intersect(sample_M2_pos, rownames(testData))
sample_M2_neg_test = intersect(sample_M2_neg, rownames(testData))

sample_M2_pos_vali = intersect(sample_M2_pos, rownames(valiData))
sample_M2_neg_vali = intersect(sample_M2_neg, rownames(valiData))


test_ROC_avg <- plot_roc_with_auc(testData[sample_M2_pos_test,], model_tuned_best, group_column = colnames(trainData)[1])
vali_ROC_avg <- plot_roc_with_auc(valiData[sample_M2_pos_vali,], model_tuned_best, group_column = colnames(trainData)[1])

combined_plot_val <- grid.arrange(test_ROC, test_ROC_avg$plot, test_CM_plot,
                                  vali_ROC,vali_ROC_avg$plot, vali_CM_plot,ncol =3)
ggsave("svm_nfeatures_performance M2_pos.pdf",plot = combined_plot_val, width = 9.5, height = 6)


test_ROC_avg <- plot_roc_with_auc(testData[sample_M2_neg_test,], model_tuned_best, group_column = colnames(trainData)[1])
vali_ROC_avg <- plot_roc_with_auc(valiData[sample_M2_neg_vali,], model_tuned_best, group_column = colnames(trainData)[1])

combined_plot_val <- grid.arrange(test_ROC, test_ROC_avg$plot, test_CM_plot,
                                  vali_ROC,vali_ROC_avg$plot, vali_CM_plot,ncol =3)

ggsave("svm_nfeatures_performance M2_neg.pdf",plot = combined_plot_val, width = 9.5, height = 6)

####### 3-2 Luwdwig ########
annotation = read.xlsx("C:/Users/WXH-PC/Desktop/20240918 final data/Clinical laboratory tests ZD-2024-10-29.xlsx",
                       rowNames = FALSE)
row.names(annotation) = annotation$sample
annotation <- annotation %>% dplyr::rename(`AMA_M2` = `AMA-M2`)
annotation_Lud = annotation[c("Group","Ludw_ALD")]

sample_BLI = c(rownames(subset(annotation_Lud,annotation_Lud$Group %in% c("CTR","A","AB"))),
               rownames(subset(annotation_Lud,annotation_Lud$Ludw_ALD == "B-LI")))

sample_BLII = c(rownames(subset(annotation_Lud,annotation_Lud$Group %in% c("CTR","A","AB"))),
                rownames(subset(annotation_Lud,annotation_Lud$Ludw_ALD == "B-LII")))

sample_BLI_train = intersect(sample_BLI, rownames(trainData))
sample_BLII_train = intersect(sample_BLII, rownames(trainData))

sample_BLI_test = intersect(sample_BLI, rownames(testData))
sample_BLII_test = intersect(sample_BLII, rownames(testData))

sample_BLI_test = intersect(sample_BLI, rownames(testData))
sample_BLII_test = intersect(sample_BLII, rownames(testData))

sample_BLI_vali = intersect(sample_BLI, rownames(valiData))
sample_BLII_vali = intersect(sample_BLII, rownames(valiData))


test_ROC_avg <- plot_roc_with_auc(testData[sample_BLI_test,], model_tuned_best, group_column = colnames(trainData)[1])
vali_ROC_avg <- plot_roc_with_auc(valiData[sample_BLI_vali,], model_tuned_best, group_column = colnames(trainData)[1])

combined_plot_val <- grid.arrange(test_ROC, test_ROC_avg$plot, test_CM_plot,
                                  vali_ROC,vali_ROC_avg$plot, vali_CM_plot,ncol =3)
ggsave("svm_nfeatures_performance BLI.pdf",plot = combined_plot_val, width = 9.5, height = 6)


test_ROC_avg <- plot_roc_with_auc(testData[sample_BLII_test,], model_tuned_best, group_column = colnames(trainData)[1])
vali_ROC_avg <- plot_roc_with_auc(valiData[sample_BLII_vali,], model_tuned_best, group_column = colnames(trainData)[1])

combined_plot_val <- grid.arrange(test_ROC, test_ROC_avg$plot, test_CM_plot,
                                  vali_ROC,vali_ROC_avg$plot, vali_CM_plot,ncol =3)

ggsave("svm_nfeatures_performance BLII.pdf",plot = combined_plot_val, width = 9.5, height = 6)


####### 3-3 subgroup ########

annotation = read.xlsx("C:/Users/WXH-PC/Desktop/20240918 final data/Clinical laboratory tests ZD-2024-10-29.xlsx",
                       rowNames = FALSE)
row.names(annotation) = annotation$sample

source("C:/Users/WXH-PC/Desktop/20240918 final data/models for CJ/ROC_CM_train_test_validation.R")
source("C:/Users/WXH-PC/Desktop/20240918 final data/models for CJ/plot_roc_avg_train.R")
source("C:/Users/WXH-PC/Desktop/20240918 final data/models for CJ/plot_roc_avg.R")

sample_subgroup_BP = unique(c(rownames(subset(annotation,annotation$Group %in% c("CTR","A","AB"))),
                              rownames(subset(annotation,!(annotation$Subgroup %in% c("BN"))))))

sample_subgroup_BN = unique(c(rownames(subset(annotation,annotation$Group %in% c("CTR","A","AB"))),
                              rownames(subset(annotation,!(annotation$Subgroup %in% c("BP"))))))


sample_subgroup_BP_train = intersect(sample_subgroup_BP, rownames(trainData))
sample_subgroup_BN_train = intersect(sample_subgroup_BN, rownames(trainData))

sample_subgroup_BP_test = intersect(sample_subgroup_BP, rownames(testData))
sample_subgroup_BN_test = intersect(sample_subgroup_BN, rownames(testData))

sample_subgroup_BP_vali = intersect(sample_subgroup_BP, rownames(valiData))
sample_subgroup_BN_vali = intersect(sample_subgroup_BN, rownames(valiData))



test_ROC_avg <- plot_roc_with_auc(testData[sample_subgroup_BP_test,], model_tuned_best, group_column = colnames(trainData)[1])
vali_ROC_avg <- plot_roc_with_auc(valiData[sample_subgroup_BP_vali,], model_tuned_best, group_column = colnames(trainData)[1])

combined_plot_val <- grid.arrange(test_ROC, test_ROC_avg$plot, test_CM_plot,
                                  vali_ROC,vali_ROC_avg$plot, vali_CM_plot,ncol =3)
ggsave("svm_nfeatures_performance subgroup_BP.pdf",plot = combined_plot_val, width = 9.5, height = 6)


test_ROC_avg <- plot_roc_with_auc(testData[sample_subgroup_BN_test,], model_tuned_best, group_column = colnames(trainData)[1])
vali_ROC_avg <- plot_roc_with_auc(valiData[sample_subgroup_BN_vali,], model_tuned_best, group_column = colnames(trainData)[1])

combined_plot_val <- grid.arrange(test_ROC, test_ROC_avg$plot, test_CM_plot,
                                  vali_ROC,vali_ROC_avg$plot, vali_CM_plot,ncol =3)

ggsave("svm_nfeatures_performance subgroup_BN.pdf",plot = combined_plot_val, width = 9.5, height = 6)

####### 3-4   UDCA response ########

annotation = read.xlsx("C:/Users/WXH-PC/Desktop/20240918 final data/Clinical laboratory tests ZD-2024-10-29.xlsx",
                       rowNames = FALSE)
row.names(annotation) = annotation$sample

source("C:/Users/WXH-PC/Desktop/20240918 final data/models for CJ/ROC_CM_train_test_validation.R")
source("C:/Users/WXH-PC/Desktop/20240918 final data/models for CJ/plot_roc_avg_train.R")
source("C:/Users/WXH-PC/Desktop/20240918 final data/models for CJ/plot_roc_avg.R")

sample_UDCA_BR = unique(c(rownames(subset(annotation,annotation$Group %in% c("CTR","A","AB"))),
                          rownames(subset(annotation,!(annotation$`UDCA-response_ALD` %in% c("BR"))))))

sample_UDCA_BNR = unique(c(rownames(subset(annotation,annotation$Group %in% c("CTR","A","AB"))),
                           rownames(subset(annotation,!(annotation$`UDCA-response_ALD` %in% c("BNR"))))))


sample_UDCA_BR_train = intersect(sample_UDCA_BR, rownames(trainData))
sample_UDCA_BNR_train = intersect(sample_UDCA_BNR, rownames(trainData))

sample_UDCA_BR_test = intersect(sample_UDCA_BR, rownames(testData))
sample_UDCA_BNR_test = intersect(sample_UDCA_BNR, rownames(testData))

sample_UDCA_BR_vali = intersect(sample_UDCA_BR, rownames(valiData))
sample_UDCA_BNR_vali = intersect(sample_UDCA_BNR, rownames(valiData))



test_ROC_avg <- plot_roc_with_auc(testData[sample_UDCA_BR_test,], model_tuned_best, group_column = colnames(trainData)[1])
vali_ROC_avg <- plot_roc_with_auc(valiData[sample_UDCA_BR_vali,], model_tuned_best, group_column = colnames(trainData)[1])

combined_plot_val <- grid.arrange(test_ROC, test_ROC_avg$plot, test_CM_plot,
                                  vali_ROC,vali_ROC_avg$plot, vali_CM_plot,ncol =3)
ggsave("svm_nfeatures_performance UDCA_BR.pdf",plot = combined_plot_val, width = 9.5, height = 6)


test_ROC_avg <- plot_roc_with_auc(testData[sample_UDCA_BNR_test,], model_tuned_best, group_column = colnames(trainData)[1])
vali_ROC_avg <- plot_roc_with_auc(valiData[sample_UDCA_BNR_vali,], model_tuned_best, group_column = colnames(trainData)[1])

combined_plot_val <- grid.arrange(test_ROC, test_ROC_avg$plot, test_CM_plot,
                                  vali_ROC,vali_ROC_avg$plot, vali_CM_plot,ncol =3)

ggsave("svm_nfeatures_performance UDCA_BNR.pdf",plot = combined_plot_val, width = 9.5, height = 6)


####### 4-0 modeling ########
discovery_final = rbind(trainData, testData)
validation_final = valiData

source("C:/Users/WXH-PC/Desktop/20240918 final data/models for CJ/multi_xgb_modeling_evaluation_update.R")
unique_combinations <- paste(sort(tuned_feature), collapse = "-")  # 组合并排序为字符串

Modeling_result = Modeling_evaluation(discovery = discovery_final,
                                      valiData = validation_final,
                                      smote = FALSE,
                                      unique_combinations = unique_combinations,
                                      p = 0.7,
                                      RFE = FALSE,
                                      metric = "AUC",
                                      rds = "xgb_nfeatures.rds")

model_performance = Modeling_result$results_df 

# RFE_plot = ggplot(Modeling_result$RFE_result) + theme_bw()
# ggsave("xgb_RFE_result.pdf", RFE_plot, width = 4, height = 3)

write.table(model_performance,"xgb_nfeatures_performance_table.txt",
            row.names = TRUE,col.names = NA, sep = "\t")


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
write.table(tuned_feature,"xgb_tuned_features.txt",row.names = FALSE,col.names = "features")

trainData = Modeling_result$trainData
testData = Modeling_result$testData

valiData = Modeling_result$valiData
validation_group = valiData

source("C:/Users/WXH-PC/Desktop/20240918 final data/models for CJ/ROC_CM_train_test_validation.R")
source("C:/Users/WXH-PC/Desktop/20240918 final data/models for CJ/plot_roc_avg_train.R")
source("C:/Users/WXH-PC/Desktop/20240918 final data/models for CJ/plot_roc_avg.R")

trainData = Modeling_result$trainData

train_ROC_avg <- plot_roc_with_auc_train(trainData, model_tuned_best, group_column = colnames(trainData)[1])
test_ROC_avg <- plot_roc_with_auc(testData, model_tuned_best, group_column = colnames(trainData)[1])
vali_ROC_avg <- plot_roc_with_auc(validation_group, model_tuned_best, group_column = colnames(trainData)[1])

combined_plot_val <- grid.arrange(train_ROC, train_ROC_avg$plot, train_CM_plot,
                                  test_ROC, test_ROC_avg$plot, test_CM_plot,
                                  vali_ROC,vali_ROC_avg$plot, vali_CM_plot,ncol =3)

ggsave("xgb_nfeatures_performance.pdf",plot = combined_plot_val, width = 9.5, height = 9)

xgb_prob_test = test_ROC_avg$prob
xgb_prob_vali = vali_ROC_avg$prob
xgb_prob = rbind(xgb_prob_test,xgb_prob_vali)

write.table(xgb_prob,  "xgb_test_vali_prob.txt",
            row.names = TRUE, col.names = NA)

####### 4-1 AMA-M2 ########
annotation = read.xlsx("C:/Users/WXH-PC/Desktop/20240918 final data/Clinical laboratory tests ZD-2024-10-29.xlsx",
                       rowNames = TRUE)


sample_M2_pos = c(rownames(subset(annotation,annotation$Group == "CTR")),
                  rownames(subset(annotation,annotation$`AMA-M2` == 1)))

sample_M2_neg = c(rownames(subset(annotation,annotation$Group == "CTR")),
                  rownames(subset(annotation,annotation$`AMA-M2` == 0)))

sample_M2_pos_train = intersect(sample_M2_pos, rownames(trainData))
sample_M2_neg_train = intersect(sample_M2_neg, rownames(trainData))

sample_M2_pos_test = intersect(sample_M2_pos, rownames(testData))
sample_M2_neg_test = intersect(sample_M2_neg, rownames(testData))

sample_M2_pos_test = intersect(sample_M2_pos, rownames(testData))
sample_M2_neg_test = intersect(sample_M2_neg, rownames(testData))

sample_M2_pos_vali = intersect(sample_M2_pos, rownames(valiData))
sample_M2_neg_vali = intersect(sample_M2_neg, rownames(valiData))


test_ROC_avg <- plot_roc_with_auc(testData[sample_M2_pos_test,], model_tuned_best, group_column = colnames(trainData)[1])
vali_ROC_avg <- plot_roc_with_auc(valiData[sample_M2_pos_vali,], model_tuned_best, group_column = colnames(trainData)[1])

combined_plot_val <- grid.arrange(test_ROC, test_ROC_avg$plot, test_CM_plot,
                                  vali_ROC,vali_ROC_avg$plot, vali_CM_plot,ncol =3)
ggsave("xgb_nfeatures_performance M2_pos.pdf",plot = combined_plot_val, width = 9.5, height = 6)


test_ROC_avg <- plot_roc_with_auc(testData[sample_M2_neg_test,], model_tuned_best, group_column = colnames(trainData)[1])
vali_ROC_avg <- plot_roc_with_auc(valiData[sample_M2_neg_vali,], model_tuned_best, group_column = colnames(trainData)[1])

combined_plot_val <- grid.arrange(test_ROC, test_ROC_avg$plot, test_CM_plot,
                                  vali_ROC,vali_ROC_avg$plot, vali_CM_plot,ncol =3)

ggsave("xgb_nfeatures_performance M2_neg.pdf",plot = combined_plot_val, width = 9.5, height = 6)

####### 4-2 Luwdwig ########
annotation = read.xlsx("C:/Users/WXH-PC/Desktop/20240918 final data/Clinical laboratory tests ZD-2024-10-29.xlsx",
                       rowNames = FALSE)
row.names(annotation) = annotation$sample
annotation <- annotation %>% dplyr::rename(`AMA_M2` = `AMA-M2`)
annotation_Lud = annotation[c("Group","Ludw_ALD")]

sample_BLI = c(rownames(subset(annotation_Lud,annotation_Lud$Group %in% c("CTR","A","AB"))),
               rownames(subset(annotation_Lud,annotation_Lud$Ludw_ALD == "B-LI")))

sample_BLII = c(rownames(subset(annotation_Lud,annotation_Lud$Group %in% c("CTR","A","AB"))),
                rownames(subset(annotation_Lud,annotation_Lud$Ludw_ALD == "B-LII")))

sample_BLI_train = intersect(sample_BLI, rownames(trainData))
sample_BLII_train = intersect(sample_BLII, rownames(trainData))

sample_BLI_test = intersect(sample_BLI, rownames(testData))
sample_BLII_test = intersect(sample_BLII, rownames(testData))

sample_BLI_test = intersect(sample_BLI, rownames(testData))
sample_BLII_test = intersect(sample_BLII, rownames(testData))

sample_BLI_vali = intersect(sample_BLI, rownames(valiData))
sample_BLII_vali = intersect(sample_BLII, rownames(valiData))


test_ROC_avg <- plot_roc_with_auc(testData[sample_BLI_test,], model_tuned_best, group_column = colnames(trainData)[1])
vali_ROC_avg <- plot_roc_with_auc(valiData[sample_BLI_vali,], model_tuned_best, group_column = colnames(trainData)[1])

combined_plot_val <- grid.arrange(test_ROC, test_ROC_avg$plot, test_CM_plot,
                                  vali_ROC,vali_ROC_avg$plot, vali_CM_plot,ncol =3)
ggsave("xgb_nfeatures_performance BLI.pdf",plot = combined_plot_val, width = 9.5, height = 6)


test_ROC_avg <- plot_roc_with_auc(testData[sample_BLII_test,], model_tuned_best, group_column = colnames(trainData)[1])
vali_ROC_avg <- plot_roc_with_auc(valiData[sample_BLII_vali,], model_tuned_best, group_column = colnames(trainData)[1])

combined_plot_val <- grid.arrange(test_ROC, test_ROC_avg$plot, test_CM_plot,
                                  vali_ROC,vali_ROC_avg$plot, vali_CM_plot,ncol =3)

ggsave("xgb_nfeatures_performance BLII.pdf",plot = combined_plot_val, width = 9.5, height = 6)


####### 4-3 subgroup ########

annotation = read.xlsx("C:/Users/WXH-PC/Desktop/20240918 final data/Clinical laboratory tests ZD-2024-10-29.xlsx",
                       rowNames = FALSE)
row.names(annotation) = annotation$sample

source("C:/Users/WXH-PC/Desktop/20240918 final data/models for CJ/ROC_CM_train_test_validation.R")
source("C:/Users/WXH-PC/Desktop/20240918 final data/models for CJ/plot_roc_avg_train.R")
source("C:/Users/WXH-PC/Desktop/20240918 final data/models for CJ/plot_roc_avg.R")

sample_subgroup_BP = unique(c(rownames(subset(annotation,annotation$Group %in% c("CTR","A","AB"))),
                              rownames(subset(annotation,!(annotation$Subgroup %in% c("BN"))))))

sample_subgroup_BN = unique(c(rownames(subset(annotation,annotation$Group %in% c("CTR","A","AB"))),
                              rownames(subset(annotation,!(annotation$Subgroup %in% c("BP"))))))


sample_subgroup_BP_train = intersect(sample_subgroup_BP, rownames(trainData))
sample_subgroup_BN_train = intersect(sample_subgroup_BN, rownames(trainData))

sample_subgroup_BP_test = intersect(sample_subgroup_BP, rownames(testData))
sample_subgroup_BN_test = intersect(sample_subgroup_BN, rownames(testData))

sample_subgroup_BP_vali = intersect(sample_subgroup_BP, rownames(valiData))
sample_subgroup_BN_vali = intersect(sample_subgroup_BN, rownames(valiData))



test_ROC_avg <- plot_roc_with_auc(testData[sample_subgroup_BP_test,], model_tuned_best, group_column = colnames(trainData)[1])
vali_ROC_avg <- plot_roc_with_auc(valiData[sample_subgroup_BP_vali,], model_tuned_best, group_column = colnames(trainData)[1])

combined_plot_val <- grid.arrange(test_ROC, test_ROC_avg$plot, test_CM_plot,
                                  vali_ROC,vali_ROC_avg$plot, vali_CM_plot,ncol =3)
ggsave("xgb_nfeatures_performance subgroup_BP.pdf",plot = combined_plot_val, width = 9.5, height = 6)


test_ROC_avg <- plot_roc_with_auc(testData[sample_subgroup_BN_test,], model_tuned_best, group_column = colnames(trainData)[1])
vali_ROC_avg <- plot_roc_with_auc(valiData[sample_subgroup_BN_vali,], model_tuned_best, group_column = colnames(trainData)[1])

combined_plot_val <- grid.arrange(test_ROC, test_ROC_avg$plot, test_CM_plot,
                                  vali_ROC,vali_ROC_avg$plot, vali_CM_plot,ncol =3)

ggsave("xgb_nfeatures_performance subgroup_BN.pdf",plot = combined_plot_val, width = 9.5, height = 6)

####### 4-4   UDCA response ########

annotation = read.xlsx("C:/Users/WXH-PC/Desktop/20240918 final data/Clinical laboratory tests ZD-2024-10-29.xlsx",
                       rowNames = FALSE)
row.names(annotation) = annotation$sample

source("C:/Users/WXH-PC/Desktop/20240918 final data/models for CJ/ROC_CM_train_test_validation.R")
source("C:/Users/WXH-PC/Desktop/20240918 final data/models for CJ/plot_roc_avg_train.R")
source("C:/Users/WXH-PC/Desktop/20240918 final data/models for CJ/plot_roc_avg.R")

sample_UDCA_BR = unique(c(rownames(subset(annotation,annotation$Group %in% c("CTR","A","AB"))),
                          rownames(subset(annotation,!(annotation$`UDCA-response_ALD` %in% c("BR"))))))

sample_UDCA_BNR = unique(c(rownames(subset(annotation,annotation$Group %in% c("CTR","A","AB"))),
                           rownames(subset(annotation,!(annotation$`UDCA-response_ALD` %in% c("BNR"))))))


sample_UDCA_BR_train = intersect(sample_UDCA_BR, rownames(trainData))
sample_UDCA_BNR_train = intersect(sample_UDCA_BNR, rownames(trainData))

sample_UDCA_BR_test = intersect(sample_UDCA_BR, rownames(testData))
sample_UDCA_BNR_test = intersect(sample_UDCA_BNR, rownames(testData))

sample_UDCA_BR_vali = intersect(sample_UDCA_BR, rownames(valiData))
sample_UDCA_BNR_vali = intersect(sample_UDCA_BNR, rownames(valiData))
 


test_ROC_avg <- plot_roc_with_auc(testData[sample_UDCA_BR_test,], model_tuned_best, group_column = colnames(trainData)[1])
vali_ROC_avg <- plot_roc_with_auc(valiData[sample_UDCA_BR_vali,], model_tuned_best, group_column = colnames(trainData)[1])

combined_plot_val <- grid.arrange(test_ROC, test_ROC_avg$plot, test_CM_plot,
                                  vali_ROC,vali_ROC_avg$plot, vali_CM_plot,ncol =3)
ggsave("xgb_nfeatures_performance UDCA_BR.pdf",plot = combined_plot_val, width = 9.5, height = 6)


test_ROC_avg <- plot_roc_with_auc(testData[sample_UDCA_BNR_test,], model_tuned_best, group_column = colnames(trainData)[1])
vali_ROC_avg <- plot_roc_with_auc(valiData[sample_UDCA_BNR_vali,], model_tuned_best, group_column = colnames(trainData)[1])

combined_plot_val <- grid.arrange(test_ROC, test_ROC_avg$plot, test_CM_plot,
                                  vali_ROC,vali_ROC_avg$plot, vali_CM_plot,ncol =3)

ggsave("xgb_nfeatures_performance UDCA_BNR.pdf",plot = combined_plot_val, width = 9.5, height = 6)






