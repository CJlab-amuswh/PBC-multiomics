# README.md

This repository contains code and data to reproduce the results of the study:  
**"Identification of Novel Non-invasive Biomarkers for Diagnosis and UDCA Response Prediction in Primary Biliary Cholangitis Through Human Serum Proteomic and N-glycoproteomic Analyses: A Multicenter Study"**  


## 1. Project Overview  
This repository provides complete, reproducible workflows for the key models developed in the study, all built using the **Random Forest (RF) classifier** in R (version 4.2.1). The models include:  
- Diagnosis models (clinical biomarker model and CPG-AILD model)  
- Ludwig stage classification model (PBC-stage model)  
- UDCA response prediction model (UDCA-treat model)  

All code and data are provided to replicate the figures and supplementary figures reported in the study.  
## 2. Data Preprocessing
Quantitative proteomic and glycoproteomic data were normalized to the median of each sample to correct for potential sample loading differences. The data were multiplied by the global median of each omics dataset for ease of presentation, followed by log2 transformation. The proteins, and intact glycopeptides (IGPs) with missing values <50% in the CTR, PBC, AIH, or PBC+AIH were retained. Missing values were estimated using the K-nearest neighbor (KNN) imputation method via the R package DreamAI (https://github.com/WangLab-MSSM/DreamAI). Because our samples were collected from multiple centers, the latent batch effect was removed using the combat function in the sva package.

## 3. Data Availability  
Processed biomarker data and sample information are stored in the `Data/` directory. These files are ready for direct use in model training (no additional preprocessing required unless specified in scripts).  

| File Name | Description |  
|-----------|-------------|  
| `Cohort data.xlsx` | Demographic information, clinical labels (e.g., diagnosis, Ludwig stage, UDCA response), and cohort grouping for all samples. |  
| `XA04703B2_DA_discovery cohort.xlsx` | Final protein biomarker data from the discovery cohort. |  
| `XA04703B2_DA_validation cohort.xlsx` | Final protein biomarker data from the validation cohort. |  
| `XA04703B2_LPIg_discovery cohort.xlsx` | Final glycoprotein biomarker data from the discovery cohort. |  
| `XA04703B2_LPIg_validation cohort.xlsx` | Final glycoprotein biomarker data from the validation cohort. |  

 (e.g., normalization, missing value imputation, outlier removal, feature selection) are automatically executed within the scripts and align with the methods described in the study. No manual preprocessing of the provided Data/ files is required.


## 4. Code Structure  
The repository is organized into three main folders, each corresponding to a model type. Each folder contains executable scripts and a `Read me` file with detailed workflow descriptions.  

### 4.1 Diagnosis Models (`Diagnosis/` folder)  
Scripts for building the diagnosis-related models:  
- `Read me`: Step-by-step description of training/testing processes for both the clinical biomarker model and CPG-AILD model.  
- `Diagnosis modeling 1st.R` & `Diagnosis modeling 2nd.R`: Code to construct the CPG-AILD model (replicates Figure 3a-3b and Supplementary Fig. 9).  
- `Clinical_nfeature_RFE_RF_classifier_multi.R`: Code to construct the clinical biomarker model (replicates Supplementary Fig. 8).  


### 4.2 Ludwig Stage Model (`Ludwig stage/` folder)  
Scripts for the PBC-stage classification model:  
- `Read me`: Workflow details for training and testing the Ludwig stage model.  
- `Ludwig stage classifier.R`: Code to build the model (replicates Figure 5f).  


### 4.3 UDCA Response Model (`UDCA response/` folder)  
Scripts for the UDCA response prediction model:  
- `Read me`: Workflow details for training and testing the UDCA-treat model.  
- `UDCA response director.R`: Code to build the model (replicates Figure 6e).  


## 5. Installation Guide  
> sessionInfo()
R version 4.4.1 (2024-06-14 ucrt)
Platform: x86_64-w64-mingw32/x64
Running under: Windows 11 x64 (build 22621)

Matrix products: default


locale:
[1] LC_COLLATE=Chinese (Simplified)_China.utf8  LC_CTYPE=Chinese (Simplified)_China.utf8   
[3] LC_MONETARY=Chinese (Simplified)_China.utf8 LC_NUMERIC=C                               
[5] LC_TIME=Chinese (Simplified)_China.utf8    

time zone: Asia/Shanghai
tzcode source: internal

attached base packages:
[1] stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
 [1] UBL_0.0.9            sp_2.1-4             automap_1.1-9        gstat_2.1-1          MBA_0.1-0            RColorBrewer_1.1-3  
 [7] gridExtra_2.3        viridis_0.6.5        viridisLite_0.4.2    multiROC_1.1.1       randomForest_4.7-1.1 future_1.67.0       
[13] lubridate_1.9.3      forcats_1.0.0        stringr_1.5.1        purrr_1.0.2          readr_2.1.5          tibble_3.2.1        
[19] tidyverse_2.0.0      openxlsx_4.2.6.1     pROC_1.18.5          e1071_1.7-14         caret_6.0-94         lattice_0.22-6      
[25] ggplot2_4.0.0        glmnet_4.1-8         Matrix_1.7-0         tidyr_1.3.1          dplyr_1.1.4         

loaded via a namespace (and not attached):
 [1] tidyselect_1.2.1     timeDate_4041.110    farver_2.1.2         S7_0.2.0             reshape_0.8.10       digest_0.6.36       
 [7] rpart_4.1.23         timechange_0.3.0     lifecycle_1.0.4      sf_1.0-16            survival_3.7-0       magrittr_2.0.3      
[13] compiler_4.4.1       rlang_1.1.4          tools_4.4.1          data.table_1.15.4    FNN_1.1.4            classInt_0.4-10     
[19] plyr_1.8.9           abind_1.4-8          KernSmooth_2.23-24   withr_3.0.2          nnet_7.3-19          grid_4.4.1          
[25] stats4_4.4.1         xts_0.14.0           colorspace_2.1-1     globals_0.18.0       scales_1.4.0         iterators_1.0.14    
[31] MASS_7.3-61          cli_3.6.2            intervals_0.15.4     generics_0.1.4       rstudioapi_0.17.1    future.apply_1.20.0 
[37] reshape2_1.4.4       tzdb_0.4.0           DBI_1.2.3            proxy_0.4-27         stars_0.6-8          splines_4.4.1       
[43] parallel_4.4.1       vctrs_0.6.5          hardhat_1.4.2        boot_1.3-32          hms_1.1.3            listenv_0.9.1       
[49] foreach_1.5.2        gower_1.0.1          units_0.8-5          recipes_1.3.1        glue_1.7.0           parallelly_1.45.1   
[55] codetools_0.2-20     stringi_1.8.4        shape_1.4.6.1        gtable_0.3.6         pillar_1.11.1        ipred_0.9-15        
[61] lava_1.8.1           R6_2.6.1             class_7.3-22         Rcpp_1.0.13          zip_2.3.1            spacetime_1.3-3     
[67] nlme_3.1-165         prodlim_2024.06.25   zoo_1.8-12           ModelMetrics_1.2.2.2 pkgconfig_2.0.3     
