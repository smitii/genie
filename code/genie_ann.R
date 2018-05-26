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
train_nn_0 <- subset(train_nn, metastasis == 0)
train_nn_1 <- subset(train_nn, metastasis == 1)
pick <- train_nn_1[sample(nrow(train_nn_1), 10772), ]
pick_0 <- train_nn_0[sample(nrow(train_nn_0), 6871), ]

train_nn <- rbind(train_nn, pick_0)
table(train_nn$metastasis)

nn = neuralnet(metastasis ~ sex + primary_race + ethnicity + age + cancer_type, data = train_nn, 
               hidden = 2, act.fct = "logistic", err.fct = "sse", linear.output = F, stepmax = 1000000)
plot(nn)
predict_train_nn <- compute(nn, train_nn[, -6])
head(predict_train_nn$net.result)
p1 <- predict_train_nn$net.result
pred1 <- ifelse(p1 > 0.5, 1, 0)
tab1 <- table(pred1, train_nn$metastasis)
tab1

pred_class <- factor(c(0, 1, 0, 1))
true_class <- factor(c(1,1,0,0))
Y      <- c(8620 , 9405, 2152, 1367)
df <- data.frame(pred_class, true_class, Y)

library(ggplot2)
ggplot(data =  df, mapping = aes(x = pred_class, y = true_class)) +
  geom_tile(aes(fill = Y), colour = "white") +
  geom_text(aes(label = sprintf("%1.0f", Y)), vjust = 1) +
  scale_fill_gradient(low = "blue", high = "red") +
  theme_bw() + theme(legend.position = "none")
