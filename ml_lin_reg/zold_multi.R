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

# Do
##############################################################################################################
##############################################################################################################
for (k in 1:5) {
	if (k == 1) { y <- rmsd1; ylab <- "rmsd1" }
	if (k == 2) { y <- rmsd2; ylab <- "rmsd2" }
	if (k == 3) { y <- rmsd3; ylab <- "rmsd3" }
	if (k == 4) { y <- e; ylab <- "e"         }
	if (k == 5) { y <- g; ylab <- "gyr"       }
	cat(ylab, "\n")

	x <- cbind(d1, d2, d3, d4, d5, d6);
	X <- cbind(1, x)
	m <- nrow(X)
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

	#line <- theta[0] + theta[1] * d1

	predict <- c(0)
	for (h in 1:m) {
		predict[h] <- c(1, X[h, 2], X[h, 3], X[h, 4], X[h, 5], X[h, 6], X[h, 7]) %*% theta
	}

	sum <- 0
	for (h in 1:m) {
		sum <- sum + ((y[h] - predict[h]) ^ 2)
	}

	rmse <- sqrt(sum/m)

	# output
	cat("		learning rate           : ", alpha,    "| # iters: ", i - 1, "\n")
	cat("		actual y[1] value       : ", y[1], "\n")
	cat("		predicted [1] value     : ", predict[1], "\n")
	cat("		actual y[19000] value   : ", y[19000], "\n")
	cat("		predicted [19000] value : ", predict[19000], "\n")
	cat("		res sum of squares      : ", sum, "\n")
	cat("		rmse                    : ", rmse, "\n")

	# generate plots
	# cost plots
	jpeg(paste("costPlot_", ylab, "_on_multi.jpg"))
	plot(1:(i), cost.history, type = "line", lwd = 2, main = paste(ylab, " on ", xlab, ": cost function", xlab = "iters", ylab = "cost"))
	dev.off();

	# some plots, idk
	# jpeg(paste("fit_", ylab, "_on_multi.jpg"))
	# fit <- lm(predict ~ x)
	# plot(fit, main = paste(ylab, "_on_multi"))
	# plot(x, y, main = paste(ylab, " on ", xlab)); abline(lm(predict~x))
	# dev.off();
 
	# scatter plots with best fit line using our theta vals
	#jpeg(paste(ylab, "_on_multi.jpg"))
	#par(mfrow = c(3, 3))
	#plot(d1, y, col = 'blue', pch = 10, main = paste(ylab, " on X with multi")); 
	#points(d2, y, col = 'red', pch = 10)
	#points(d3, y, col = 'orange', pch = 10)
	#points(d4, y, col = 'green', pch = 10)
	#points(d5, y, col = 'pink', pch = 10)
	#points(d6, y, col = 'black', pch = 10)
	#abline(lm(predict~d1+d2+d3+d4+d5+d6))
	#abline(lm(predict~X))
	#abline(predict)
	#abline(coef=theta, col="black")
	#legend(10,85,legend = c("d1","d2","d3","d4","d5","d6"), col = c("blue", "red", "orange", "green", "pink", "black"),pch = 16)
	#dev.off();


	#plot(d2, y, main = paste(ylab, " on d2 with multi")); abline(lm(predict~d2))
	#plot(d3, y, main = paste(ylab, " on d3 with multi")); abline(lm(predict~d3))
	#plot(d4, y, main = paste(ylab, " on d4 with multi")); abline(lm(predict~d4))
	#plot(d5, y, main = paste(ylab, " on d5 with multi")); abline(lm(predict~d5))
	#plot(d6, y, main = paste(ylab, " on d6 with multi")); abline(lm(predict~d6))


	#jpeg(paste("multi_", ylab, ".jpg"))
	#plot(x, y, main = paste(ylab, " on X with multi")); abline(lm(predict~X))
	#dev.off();

	jpeg(paste(ylab, "_on_multi.jpg"))
		plot(d1, y, col = 'blue', main = paste(ylab, " on X with multi")); 
		points(d2, y, col = 'red')
		points(d3, y, col = 'orange')
		points(d4, y, col = 'green')
		points(d5, y, col = 'pink')
		points(d6, y, col = 'purple')
		lines(1:19000,predict);
		legend(55,30,legend = c("d1","d2","d3","d4","d5","d6"), col = c("blue", "red", "orange", "green", "pink", "purple"),pch = 16)
	dev.off()

	cat("\n")
}

