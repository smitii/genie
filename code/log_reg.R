# -----------
# import data
# -----------

library(readr)
df <- read_csv("C:/Users/kauls15/Desktop/github/genie/data/derived/d2.csv", col_names = FALSE)
head(df)

### give column names

colnames(df) = c("sex", "primary_race", "ethnicity", "age", "cancer_type", "metastasis")
colnames(df)

### check for NAs

any(is.na(df))

### check structure of df

str(df)

# --------------
# visualize data
# --------------

library(ggplot2); library(ggthemes); library(dplyr)

### numeric only

num_cols <- sapply(df, is.numeric)

### filter

cor_data <- cor(df[, num_cols])
cor_data

### add libraries

library(corrplot); library(corrgram)

### plot

corrplot(cor_data, method = 'color')

# ----------------------------------
# prepare data for linear regression
# ----------------------------------

### create training and test sets

library(caret)
set.seed(2018)
trainIndex <- createDataPartition(y = df$metastasis, p = .70, list = FALSE)
train <- df[trainIndex, ]
test  <- df[-trainIndex, ]

### balance training and test sets by duplicating rows with no metastasis

train_0 <- subset(train, metastasis == 0)
train_1 <- subset(train, metastasis == 1)
num_0 <- dim(subset(train, metastasis == 0))[1]
num_1 <- dim(subset(train, metastasis == 1))[1]
pick_0 <- train_0[sample(nrow(train_0), num_1 - num_0), ]
train <- rbind(train, pick_0)
table(train$metastasis)

### feature plot -- not a good idea with this data

featurePlot(x = train[, c("sex", "primary_race", "ethnicity", "age", "cancer_type")], y = train$metastasis, plot = "pairs")

### quantile plot -- not a good idea with this data

qplot(age, metastasis, data = train)

# -------------------
# logistic regression
# -------------------

### build the first model

m1 <- glm(metastasis ~ sex + primary_race + ethnicity + age + cancer_type, data = train, family = binomial)
summary(m1)

### remove primary_race and cancer_type

m2 <- glm(metastasis ~ sex + ethnicity + age, data = train, family = binomial)
summary(m2)

### predict for training data

p1 <- predict(m2, train, type = 'response')
head(p1); head(train)

### misclassification error for training data

pred1 <- ifelse(p1 > 0.5, 1, 0)
tab1 <- table(Predicted = pred1, Actual = train$metastasis)
tab1
misclass_error <- 1 - sum(diag(tab1)) / sum(tab1)
misclass_error # 47.98 %

### ROC curve and AUC train

library(ROCR)
pred1 <- prediction(pred1, train$metastasis)
roc <- performance(pred1, 'tpr', 'fpr')
plot(roc,
     colorize = T,
     main = 'ROC Curve',
     xlab = 'False Positive Rate / 1 - Specificity',
     ylab = 'True Positive Rate / Specificity')
abline(a = 0, b = 1)

auc <- performance(pred1, 'auc')
auc <- unlist(slot(auc, 'y.values'))
auc <- round(auc, 4)
legend(0.6, 0.2, auc, title = "AUC", cex = 1.2)

### predict for test data

p2 <- predict(m2, test, type = 'response')
head(p2); head(test)

### misclassification error for test data

pred2 <- ifelse(p2 > 0.5, 1, 0)
tab2 <- table(Predicted = pred2, Actual = test$metastasis)
tab2
misclass_error <- 1 - sum(diag(tab2)) / sum(tab2)
misclass_error # 48.12 %

### ROC curve and AUC test

pred2 <- prediction(pred2, test$metastasis)
roc <- performance(pred2, 'tpr', 'fpr')
plot(roc,
     colorize = T,
     main = 'ROC Curve',
     xlab = 'False Positive Rate / 1 - Specificity',
     ylab = 'True Positive Rate / Specificity')
abline(a = 0, b = 1)

auc <- performance(pred2, 'auc')
auc <- unlist(slot(auc, 'y.values'))
auc <- round(auc, 4)
legend(0.6, 0.2, auc, title = "AUC", cex = 1.2)

### goodness-of-fit test

with(m2, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = F)) # p-value = 2.724977e-14
