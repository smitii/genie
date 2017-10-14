# I run this code in R with
# 

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
library(caret); library(ggplot2); library(jpeg);

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

	for (j in 1:6) {

		if (j == 7 || j == 8 || j == 9) {
			threshold <- .001
			if (k == 4) {threshold <- .00281}
			if (j == 7) { x <- train$a1; x <- train$a1; xlab <- "a1" }
			if (j == 8) { x <- train$a2; x <- train$a2; xlab <- "a2" }
			if (j == 9) { x <- train$a3; x <- train$a3; xlab <- "a3" }
		}
		else {
			threshold <- .00032
			if (j == 1) { x <- train$d1; x.test <- test$d1; xlab <- "d1" }
			if (j == 2) { x <- train$d2; x.test <- test$d2; xlab <- "d2" }
			if (j == 3) { x <- train$d3; x.test <- test$d3; xlab <- "d3" }
			if (j == 4) { x <- train$d4; x.test <- test$d4; xlab <- "d4" }
			if (j == 5) { x <- train$d5; x.test <- test$d5; xlab <- "d5" }
			if (j == 6) { x <- train$d6; x.test <- test$d6; xlab <- "d6" }
		}
		cat("	", xlab, "\n")

		X <- cbind(1, x)
		X.test <- cbind(1, x.test)
		m <- nrow(X)
		m.test <- nrow(X.test)
		alpha <- 0.00001
		theta <- c(0, 0)

		# define cost function
		cost <- function(theta_temp) {
			return (sum(((X %*% theta_temp) - y) ^ 2) / (2 * m))
		}

		theta1.history <- c(0)
		theta2.history <- c(0)
		cost.history <- c(0)

		i = 0;
		repeat {
			theta[1] <- theta[1] - alpha * (1/m) * sum(((X %*% theta) - y))
			theta[2] <- theta[2] - alpha * (1/m) * sum(((X %*% theta) - y) * X[,2])
			if (i == 0) {
				cat("		starting theta values   : ", theta[1], ", ", theta[2], "\n")
			}


			theta1.history[i] <- theta[1]
			theta2.history[i] <- theta[2]
			cost.history[i] <- cost(theta) 
	
			if (i > 1) {
				diff <- abs(cost.history[i] - cost.history[i- 1])
				if (diff < threshold) {
					break
				}
			}

			i <- i + 1
		}

		line <- theta[0] + theta[1] * d1

		predict <- c(0)
		for (h in 1:m.test) {
			predict[h] <- c(1, X.test[h, 2]) %*% theta
		}

		sum <- 0
		for (h in 1:m.test) {
			sum <- sum + ((y.test[h] - predict[h]) ^ 2)
		}

		rmse <- sqrt(sum/m.test)

		# output
		
		cat("		final theta values      : ", theta[1], ", ", theta[2], "\n")
		cat("		learning rate           : ", alpha,    "| # iters: ", i - 1, "\n")
		#cat("		actual y[1] value       : ", y[1], "\n")
		#cat("		predicted [1] value     : ", predict[1], "\n")
		#cat("		actual y[19000] value   : ", y[19000], "\n")
		#cat("		predicted [19000] value : ", predict[19000], "\n")
		cat("		res sum of squares      : ", sum, "\n")
		cat("		rmse                    : ", rmse, "\n")
		cat("\n")

		# generate plots
		# cost plots
		jpeg(paste("costPlot_", ylab, "_on_", xlab, ".jpg"))
		plot(1:(i), cost.history, type = "line", lwd = 2, main = paste(ylab, " on ", xlab, ": cost function", xlab = "iters", ylab = "cost"))
		dev.off();

		# scatter plots with best fit line using our theta vals
		jpeg(paste("fit_", ylab, "_on_", xlab, ".jpg"))
		plot(x, y, main = paste(ylab, " on ", xlab)); abline(lm(predict~x.test))
		dev.off();
	}
	cat("\n")
}

