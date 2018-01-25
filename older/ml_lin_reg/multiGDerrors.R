# multivariate gradient descent
# training and testing errors

# in R, output data to file "fname.txt" with commands:
#
# sink("fname.txt", append = FALSE, split = FALSE)
# source("multiGDerrors.R")
# sink()
#

# load packages
library(caret); library(ggplot2); library(jpeg);

##############################################################################################################
# save .dat files to R variables
dist = read.table("dist.dat", colClasses = c("V1" = "NULL"))
dihe = read.table("dihe.dat", colClasses = c("V1" = "NULL"))
ext = read.table("extension.dat", colClasses = c("V1" = "NULL"))
rmsd = read.table("rmsdCorrect.dat", colClasses = c("V1" = "NULL"))
gyr = read.table("rgyr.dat", colClasses = c("V1" = "NULL"))

# also combine data into one frame
all = cbind(ext, rmsd, gyr, dist, dihe)
colnames(all) [1:14] = c("e", "rmsd1", "rmsd2", "rmsd3", "g", "d1", "d2", "d3", "d4", "d5", "d6", "a1", "a2", "a3")

attach(all); 
for (i in 1:3) {
	if (i == 1) {x <- a1}
	else if (i == 2) {x <- a2}
	else if (i == 3) {x <- a3}

	for (j in 1:19000) {
		if(is.na(x[j]) == FALSE) {
			if (x[j] < 0) {
				x[j] <- abs(x[j])
			}
		}
	}

	if (i == 1) {a1 <- x}
	else if (i == 2) {a2 <- x}
	else if (i == 3) {a3 <- x}
}

set.seed(10)
inTrain <- createDataPartition(y = all$e, p = 0.75, list = FALSE)
train <- all[inTrain,]
test <- all[-inTrain,]

# Do
##############################################################################################################
##############################################################################################################
for (k in 1:5) {
	if (k == 1) { y <- train$rmsd1; y.test <- test$rmsd1; ylab <- "rmsd1" }
	if (k == 2) { y <- train$rmsd2; y.test <- test$rmsd2; ylab <- "rmsd2" }
	if (k == 3) { y <- train$rmsd3; y.test <- test$rmsd3; ylab <- "rmsd3" }
	if (k == 4) { y <- train$e    ; y.test <- test$e    ; ylab <- "e"     }
	if (k == 5) { y <- train$g    ; y.test <- test$g    ; ylab <- "gyr"   }
	cat(ylab, "\n")

	x <- cbind(train$d1, train$d2, train$d3, train$d4, train$d5, train$d6);
	x.test <- cbind(test$d1, test$d2, test$d3, test$d4, test$d5, test$d6)
	X <- cbind(1, x)
	X.test <- cbind(1, x.test)
	m <- nrow(X)
	m.test <- nrow(X.test)
	alpha <- 0.00001
	theta <- c(0, 0, 0, 0, 0, 0, 0)
	threshold <- .00032

	# define cost function
	cost <- function(theta_temp) {
		return (sum(((X %*% theta_temp) - y) ^ 2) / (2 * m))
	}

	theta1.history <- c(0)
	theta2.history <- c(0)
	theta3.history <- c(0)
	theta4.history <- c(0)
	theta5.history <- c(0)
	theta6.history <- c(0)
	theta7.history <- c(0)
	cost.history <- c(0)

	i = 0;
	repeat {
		theta[1] <- theta[1] - alpha * (1/m) * sum(((X %*% theta) - y))
		theta[2] <- theta[2] - alpha * (1/m) * sum(((X %*% theta) - y) * X[,2])
		theta[3] <- theta[3] - alpha * (1/m) * sum(((X %*% theta) - y) * X[,3])
		theta[4] <- theta[4] - alpha * (1/m) * sum(((X %*% theta) - y) * X[,4])
		theta[5] <- theta[5] - alpha * (1/m) * sum(((X %*% theta) - y) * X[,5])
		theta[6] <- theta[6] - alpha * (1/m) * sum(((X %*% theta) - y) * X[,6])
		theta[7] <- theta[7] - alpha * (1/m) * sum(((X %*% theta) - y) * X[,7])

		theta1.history[i] <- theta[1]
		theta2.history[i] <- theta[2]
		theta3.history[i] <- theta[3]
		theta4.history[i] <- theta[4]
		theta5.history[i] <- theta[5]
		theta6.history[i] <- theta[6]
		theta7.history[i] <- theta[7]
		cost.history[i] <- cost(theta) 
	
		if (i > 1) {
			diff <- abs(cost.history[i] - cost.history[i- 1])
			if (diff < threshold) {
				break
			}
		}

		i <- i + 1
	}

	# training error	
	predict <- c(0)
	for (h in 1:m) {
		predict[h] <- c(1, X[h, 2], X[h, 3], X[h, 4], X[h, 5], X[h, 6], X[h, 7]) %*% theta
	}

	sum <- 0
	for (h in 1:m) {
		sum <- sum + ((y[h] - predict[h]) ^ 2)
	}

	rmse <- sqrt(sum/m)
	
	# testing error
	predict.test <- c(0)
	for (h in 1:m.test) {
		predict.test[h] <- c(1, X.test[h, 2], X.test[h, 3], X.test[h, 4], X.test[h, 5], X.test[h, 6], X.test[h, 7]) %*% theta
	}

	sum.test<- 0
	for (h in 1:m.test) {
		sum.test <- sum.test + ((y.test[h] - predict.test[h]) ^ 2)
	}

	rmse.test <- sqrt(sum.test/m.test)

	# output
	cat("		learning rate            : ", alpha,    "| # iters: ", i - 1, "\n")
	cat("		res sum of squares       : ", sum, "\n")
	cat("		res sum of squares, test : ", sum.test, "\n")
	cat("		rmse                     : ", rmse, "\n")
	cat("		rmse, test               : ", rmse.test, "\n")

	#cat("		actual y[1] value        : ", y[1], "\n")
	#cat("		predicted [1] value      : ", predict[1], "\n")
	#cat("		actual y[19000] value    : ", y[19000], "\n")
	#cat("		predicted [19000] value  : ", predict[19000], "\n")

	# generate plots
	# cost plots
	#jpeg(paste("costPlot_", ylab, "_on_multi.jpg"))
	#plot(1:(i), cost.history, type = "line", lwd = 2, main = paste(ylab, " on multi: cost function"), xlab = "iters", ylab = "cost")
	#dev.off();

	cat("\n")
}

