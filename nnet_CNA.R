# import clinical data and CNA data
library(readr); library(rnn); library(MLmetrics); library(Metrics)
with_cna <- read_csv("C:/Users/kauls15/Desktop/ml_now/with_cna/with_cna_1.01.csv");
with_cna <- with_cna[-6117:-7917, -1] # remove samples with no CNA data

# remove first 3 columns
with_cna <- with_cna[, -1:-3] # remove samples with no CNA data

# move sample_type response variable to the last, 11th, column
with_cna[12] = with_cna[6]
with_cna <- with_cna[, -6] # remove samples with no CNA data
colnames(with_cna)[11] <- "sample_type"

# columns:
#  1: age, 2: ethnicity, 3: primary_cancer, 4: primary_race, 5: sex,
#  6: deep_loss, 7: sc_loss, 8: diploid, 9: lowl_gain, 10: highl_gain
# 11: sample_type

# partition into training (50%), testing (25%), and validation (25%) sets
temp <- with_cna
train <- temp[1:ceiling(dim(temp)[1] * .5),]
temp <- temp[(1 + ceiling(dim(temp)[1] * .5)):(dim(temp)[1]),]
valid <- temp[1:ceiling(dim(temp)[1] * .5),]
test <- temp[(1 + ceiling(dim(temp)[1] * .5)):(dim(temp)[1]),]

# convert to binary
X1 <- int2bin(train$age)
X2 <- int2bin(train$ethnicity)
X3 <- int2bin(train$primary_cancer)
X4 <- int2bin(train$primary_race)
X5 <- int2bin(train$sex)
X6 <- int2bin(train$deep_loss)
X7 <- int2bin(train$sc_loss)
X8 <- int2bin(train$diploid)
X9 <- int2bin(train$lowl_gain)
X10 <- int2bin(train$lowl_gain)

Y <- int2bin(train$sample_type)

# Create 3d array: dim 1: samples; dim 2: time; dim 3: variables.
X <- array( c(X1,X2,X3,X4,X5,X6,X7,X8,X9,X10), dim=c(dim(X1),10) )

# train the model
model <- trainr( Y=Y[,dim(Y)[2]:1],
                 X=X[,dim(X)[2]:1,],
                 learningrate = 0.1,
                 hidden_dim = 10,
                 numepochs = 10
)

plot( colMeans(model$error), type='l',
      xlab='epoch',
      ylab='errors'                  
)

# convert test inputs to binary
A1 <- int2bin(test$age)
A2 <- int2bin(test$ethnicity)
A3 <- int2bin(test$primary_cancer)
A4 <- int2bin(test$primary_race)
A5 <- int2bin(test$sex)
A6 <- int2bin(test$deep_loss)
A7 <- int2bin(test$sc_loss)
A8 <- int2bin(test$diploid)
A9 <- int2bin(test$lowl_gain)
A10 <- int2bin(test$lowl_gain)

# create 3d array: dim 1: samples; dim 2: time; dim 3: variables
A <- array( c(A1,A2,A3,A4,A5,A6,A7,A8,A9,A10), dim=c(dim(A1), 10) )

# predict
B <- predictr( model, A[, dim(A)[2]:1, ] )
B = B[, dim(B)[2]:1]

# convert back to integers
A1 <- bin2int(A1)
A2 <- bin2int(A2)
A3 <- bin2int(A3)
A4 <- bin2int(A4)
A5 <- bin2int(A5)
A6 <- bin2int(A6)
A7 <- bin2int(A7)
A8 <- bin2int(A8)
A9 <- bin2int(A9)
A10 <- bin2int(A10)

B <- bin2int(B)

# inspect the differences
table(B - (test$sample_type) )

# plot the difference
hist( B-(test$sample_type) ) 