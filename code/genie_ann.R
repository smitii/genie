library(neuralnet); library(caret)
data <- d[, -1]
head(data)

trainIndex <- createDataPartition(data$metastasis, p = .75, list = FALSE, times = 1)
train <- data[trainIndex, ]
test  <- data[-trainIndex, ]

# max-min Normalization
normalize <- function(x) {
  return ((x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)))
}

scaled <- as.data.frame(lapply(data, normalize))
head(scaled)
row_has_na <- apply(scaled, 1, function(x){any(is.na(x))})
sum(row_has_na)
scaled <- scaled[!row_has_na,]

train_nn <- scaled[trainIndex, ]
test_nn  <- scaled[-trainIndex, ]

nn = neuralnet(metastasis ~ sex + primary_race + ethnicity + age + cancer_type, data = train_nn, 
               hidden = 2, act.fct = "logistic", err.fct = "sse", linear.output = F)
plot(nn)
predict_train_nn <- compute(nn, train_nn[, -6])
head(predict_train_nn$net.result)
p1 <- predict_train_nn$net.result
pred1 <- ifelse(p1 > 0.5, 1, 0)
tab1 <- table(pred1, train_nn$metastasis)
tab1