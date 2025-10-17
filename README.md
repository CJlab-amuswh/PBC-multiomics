# PBC-multiomics

This repository of codes can reproduce the results of study:
"Identification of Novel Non-invasive Biomarkers for Diagnosis and UDCA Response Prediction in Primary Biliary Cholangitis Through Human Serum Proteomic and N-glycoproteomic Analyses: A Multicenter Study"

All the analyses were performed by random forest (RF) classifier in R (4.2.1).

Diagnosis file: 
1.Read me: describes the training and test processes for clinical biomarker and CPG-AILD model. 
2.Diagnosis modeling 1st.R and Diagnosis modeling 2nd.R for constructing CPG-AILD corresponding to Figure 3a-3b, and Supplementary Fig. 9.
3.Clinical_nfeature_RFE_RF_classifier_multi.R for constructing clinical biomarker corresponding to Supplementary Fig. 8.

Ludwig stage file:
1.Read me: describes the training and test processes for the PBC-stage model. 
2.Ludwig stage classifier.R for constructing PBC-stage corresponding to Figure 5f.

UDCA response file:
1.Read me: describes the training and test processes for the UDCA-treat model. 
2.UDCA response director.R for constructing UDCA-treat corresponding to Figure 6e.

The final biomarker data can be found in the Data file:
1)Sample information: 
Cohort data.xlsx
2)The final protein biomarker data in the discovery cohort: 
XA04703B2_DA_discovery cohort.xlsx
3)The final protein biomarker data in the validation cohort: 
XA04703B2_DA_validation cohort.xlsx
4)The final glycoprotein biomarker data in the discovery cohort: 
XA04703B2_LPIg_discovery cohort.xlsx
5)The final glycoprotein biomarker data in the validation cohort: 
XA04703B2_LPIg_validation cohort.xlsx
