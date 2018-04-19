set.seed(125)
cv_errors <- rep(0, 10)

df <- d[sample(nrow(d)),]
folds <- cut(seq(1, nrow(df)), breaks = 10, labels = FALSE)

for (i in 1:10) {
  # segment data
  test_indexes <- which(folds == i, arr.ind = TRUE)
  test <- df[test_indexes, ]
  train <- df[-test_indexes, ]
  
  m1 = glm(train$metastasis ~ train$sex + train$primary_race + train$ethnicity + train$age + train$cancer_type, family = binomial)
  
  fold_errors <- rep(0, nrow(test))
  
  for (j in 1:nrow(test)) {
    fold_errors[j] <- predict.glm(m1, data.frame(metastasis=test$metastasis[j]),type="resp") - test$metastasis[j]
    
  }
  
  cv_errors[i] <- mean(fold_errors ^ 2)
}

mse_m1 <- mean(cv_errors)
cat("test mse: ", mse_m1) # 0.02253641

##
trainIndex <- createDataPartition(d$metastasis, p = .75, list = FALSE, times = 1)
train <- d[trainIndex, ]
test  <- d[-trainIndex, ]

m1 = glm(train$metastasis ~ train$sex + train$primary_race + train$ethnicity + train$age + train$cancer_type, family = binomial)

fold_errors <- rep(0, nrow(test))
predicted <- predict.glm(m1, data.frame(metastasis=test$metastasis[j]),type="resp")
predicted <- data.frame(predicted)
for (j in 1:nrow(test)) {
  fold_errors[j] <- predicted[j] - test$metastasis[j]
}
x <- predict(m1, test$metastasis)
x <- as.vector(x)

for (j in 1:nrow(test)) {
  fold_errors[j] <- (x[j] - test$metastasis[j])
}
for (k in 1:length(x)) {
  
}
error <- mean(fold_errors ^ 2)
mse_m1 <- mean(error)
cat("test mse: ", mse_m1) # 0.02379561
confusion_matrix <- as.data.frame(table(predicted, test$metastasis))

ggplot(data = confusion_matrix,
       mapping = aes(x = predicted,
                     y = Var2)) +
  geom_tile(aes(fill = Freq)) +
  geom_text(aes(label = sprintf("%1.0f", Freq)), vjust = 1) +
  scale_fill_gradient(low = "blue",
                      high = "red",
                      trans = "log") 

TClass <- factor(c(0, 0, 1, 1))
PClass <- factor(c(0, 1, 0, 1))
Y      <- c(2816, 248, 34, 235)
df <- data.frame(TClass, PClass, Y)

library(ggplot2)
ggplot(data =  df, mapping = aes(x = TClass, y = PClass)) +
  geom_tile(aes(fill = Y), colour = "white") +
  geom_text(aes(label = sprintf("%1.0f", Y)), vjust = 1) +
  scale_fill_gradient(low = "blue", high = "red") +
  theme_bw() + theme(legend.position = "none")