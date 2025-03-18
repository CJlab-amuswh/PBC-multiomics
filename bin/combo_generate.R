combo_generate <- function(features, nfeatures = 5, ncombo = 100, seed = 12345678) {
  set.seed(seed)  # 设置种子以确保可重复性
  unique_combinations <- character(0)  # 初始化为空字符向量
  
  # 循环直到获得所需数量的唯一组合
  while(length(unique_combinations) < ncombo) {
    combo <- sample(features, nfeatures)  # 随机采样
    combo_str <- paste(sort(combo), collapse = "-")  # 组合并排序为字符串
    if(!combo_str %in% unique_combinations) {
      unique_combinations <- c(unique_combinations, combo_str)  # 添加到唯一组合中
    }
  }
  
  # 处理单个 feature 的情况
  if (nfeatures == 1) {
    combo_df <- data.frame(Protein1 = unique_combinations, stringsAsFactors = FALSE)
  } else {
    # 将组合字符串转换为数据框
    combo_df <- t(sapply(unique_combinations, function(x) unlist(strsplit(x, "-", fixed = TRUE))))
    combo_df <- as.data.frame(combo_df, stringsAsFactors = FALSE)
    colnames(combo_df) <- paste0("Protein", 1:nfeatures)  # 给列名赋予合适的名称
  }
  
  return(list(unique_combinations = unique_combinations, combo_df = combo_df))  # 返回数据框
}

# 使用示例
# features <- c("A", "B", "C", "D", "E", "F", "G")
# result <- combo_generate(features = features, nfeatures = 1, ncombo = 5)
# print(result$combo_df)
