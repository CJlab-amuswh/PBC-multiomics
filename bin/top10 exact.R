# 定义一个函数来筛选每个比较组中的up和down的前10个行名
extract_top10 <- function(type_col, p_col) {
  # 筛选up的前10个
  up <- DEP_results %>%
    filter(!!sym(type_col) == "up") %>%
    arrange(!!sym(p_col)) %>%
    slice(1:10) %>%
    rownames()
  
  # 筛选down的前10个
  down <- DEP_results %>%
    filter(!!sym(type_col) == "down") %>%
    arrange(!!sym(p_col)) %>%
    slice(1:10) %>%
    rownames()
  
  # 返回up和down的并集
  union(up, down)
}

# 针对每个比较组提取top10行名并取并集
A_top10 <- extract_top10("A.CTR_Type", "A.CTR_p_value")
B_top10 <- extract_top10("B.CTR_Type", "B.CTR_p_value")
AB_top10 <- extract_top10("AB.CTR_Type", "AB.CTR_p_value")
BA_top10 <- extract_top10("B.A_Type", "B.A_p_value")
BAB_top10 <- extract_top10("B.AB_Type", "B.AB_p_value")
AAB_top10 <- extract_top10("A.AB_Type", "A.AB_p_value")

# 所有比较组的并集
all_top10_union <- Reduce(union, list(A_top10, B_top10, AB_top10, BA_top10, BAB_top10, AAB_top10))

# 显示结果
all_top10_union


