# get and process data
library(car); library(dplyr)
cancer <- read.csv("C:/Users/kauls15/Desktop/github/genie/data/derived/data_clinical_patient.txt", stringsAsFactors = FALSE, sep = "\t")
cancer <- cancer[-c(1,2,3),]
colnames(cancer) = cancer[1, ] # the first row will be the header
cancer <- cancer[-1, -5]  
head(cancer)

sample <- read.csv("C:/Users/kauls15/Desktop/github/genie/data/derived/data_clinical_sample.txt", stringsAsFactors = FALSE, sep = "\t")
sample$AGE_AT_SEQ_REPORT[sample$AGE_AT_SEQ_REPORT == '<18'] <- 17
sample$AGE_AT_SEQ_REPORT[sample$AGE_AT_SEQ_REPORT == '>89'] <- 90
head(sample)

c <- cancer
# sort(unique(df$SEX))
c$SEX <- recode(c$SEX, "Female" = 0, "Male" = 1, "Unknown" = 2);
c$PRIMARY_RACE <- recode(c$PRIMARY_RACE, Asian = 0, Black = 1, "Native American" = 2, Other = 3, Undefined = 4, Unknown = 5, White = 6)
c$ETHNICITY <- recode(c$ETHNICITY, "Non-Spanish/non-Hispanic" = 0, "Spanish/Hispanic" = 1, Unknown = 2)
head(c)

s <- sample
s <- s[, c(-3,-6)]
s <- cbind(s, s$SAMPLE_TYPE)
s <- s[, -3]
names(s) <- c("PATIENT_ID", "age", "cancer_type", "metastasis")
s$metastasis <- recode(s$metastasis, Metastasis = 0, Other = 2, Primary = 1, Unspecified = 3)
s$cancer_type <- recode(s$cancer_type, "Adenocarcinoma In Situ" = 0, 
                        "Adrenocortical Carcinoma" = 1, "Ampullary Carcinoma" = 2,
                        "Anal Cancer" = 3, "Appendiceal Cancer" = 4,
                        "Bladder Cancer" = 5, "Bladder/Urinary Tract Cancer, NOS" = 6,
                        "Blastic Plasmacytoid Dendritic Cell Neoplasm" = 7, "Blood Cancer, NOS" = 8,
                        "Bone Cancer" = 9, "Bone Cancer, NOS" = 10, 
                        "Bowel Cancer, NOS" = 11, "Breast Cancer" = 12, 
                        "Breast Sarcoma"= 13, "Cancer of Unknown Primary" = 14, 
                        "Cervical Cancer" = 15, "Choroid Plexus Tumor" = 16, 
                        "CNS Cancer" = 17, "CNS/Brain Cancer, NOS" = 18, 
                        "Colorectal Cancer" = 19, "Embryonal Tumor" = 20, 
                        "Endometrial Cancer" = 21, "Esophageal/Stomach Cancer, NOS" = 22, 
                        "Esophagogastric Cancer" = 23, "Gastrointestinal Neuroendocrine Tumor" = 24,
                        "Gastrointestinal Stromal Tumor" = 25, "Germ Cell Tumor" = 26, 
                        "Gestational Trophoblastic Disease" = 27, "Glioma" = 28, 
                        "Head and Neck Cancer" = 29, "Head and Neck Cancer, NOS" = 30, 
                        "Hepatobiliary Cancer" = 31, "Histiocytic Disorder" = 32, 
                        "Histiocytosis" = 33, "Hodgkin Lymphoma" = 34,
                        "Leukemia" = 35, "Lung Cancer, NOS" = 36, 
                        "Mastocytosis" = 37, "Melanoma" = 38,
                        "Mesothelioma" = 39, "Miscellaneous Brain Tumor" = 40,
                        "Miscellaneous Neuroepithelial Tumor" = 41, "Multiple Myeloma" = 42,
                        "Myelodysplasia" = 43, "Myeloproliferative Neoplasm" = 44,
                        "Nerve Sheath Tumor" = 45, "Non-Hodgkin Lymphoma" = 46, 
                        "Non-Small Cell Lung Cancer" = 47, "Other Cancer, NOS"= 48,
                        "Ovarian Cancer" = 49, "Ovarian/Fallopian Tube Cancer, NOS" = 50, 
                        "Pancreatic Cancer" = 51, "Pancreatic Cancer, NOS" = 52, 
                        "Penile Cancer" = 53, "Pheochromocytoma" = 54, 
                        "Pineal Tumor" = 55, "Prostate Cancer" = 56,
                        "Renal Cell Carcinoma" = 57, "Retinoblastoma" = 58,
                        "Salivary Gland Cancer" = 59, "Sellar Tumor" = 60,
                        "Sex Cord Stromal Tumor" = 61, "Skin Cancer, Non-Melanoma" = 62, 
                        "Skin Cancer, NOS" = 63, "Small Bowel Cancer" = 64, 
                        "Small Cell Lung Cancer" = 65, "Soft Tissue Sarcoma" = 66,
                        "Testicular Cancer, NOS" = 67, "Thymic Tumor" = 68,
                        "Thyroid Cancer" = 69, "Thyroid Cancer, NOS" = 70,
                        "Uterine Cancer, NOS" = 71, "Uterine Sarcoma" = 72, 
                        "Vaginal Cancer" = 73, "Vulvar/Vaginal Cancer, NOS" = 74, "Wilms Tumor" = 75)
head(s)
temp <- merge(c, s, by="PATIENT_ID")
names(temp) <- c("patient_id", "sex", "primary_race", "ethnicity","age", "cancer_type", "metastasis")
# write.table(temp, "C:/Users/kauls15/Desktop/github/genie/data/derived/data_clinical_patient_and_sample.txt", sep="\t")
head(temp)

d <- temp
d$age <- as.numeric(d$age)
head(d)
t <- subset(d, metastasis != 2)
t <- subset(t, metastasis != 3)
t <- subset(t, sex != 2)
head(t)
d <- t
write.table(d, "C:/Users/kauls15/Desktop/github/genie/data/derived/d.txt", sep="\t")
write.csv(d, "C:/Users/kauls15/Desktop/github/genie/data/derived/d.csv")

