# import clinical data and CNA data
library(readr)
main_old <- read_csv("C:/Users/kauls15/Desktop/ml_now/with_cna/our_data_1.05.csv");
cna_old <- read_csv("C:/Users/kauls15/Desktop/ml_now/with_cna/data_CNA.csv", col_names = FALSE);

# add CNA categorical columns to main dataset
main_old$deep_loss <- NA
main_old$sc_loss <- NA
main_old$diploid <- NA
main_old$lowl_gain <- NA
main_old$highl_gain <- NA

# figure out CNA data
main <- as.data.frame(main_old)
cna <- t(cna_old)
rownames(cna) <- 1:nrow(cna)
colnames(cna) <- cna[1, ]
cna = cna[-1, ]

for (i in 2:dim(cna)[1]) { # loop through each column of CNA data, aka each GENIE sample
  j <- 1;
  while ((cna[i, 1] != main[j, 1]) && (j <= dim(main)[1])) { # loop through each row of MAIN data, aka each GENIE sample in CNA data matches sample in MAIN data 
    j = j + 1;
  }    
  
  # cat("---- i =", i, ", j =", j - 1, ", outside the while loop\n");
  if (j <= dim(main)[1]) {
    if (cna[i, 1] == main[j, 1]) {
      # cat(cna[i, 1], " matches ", main[j, 1], "\n");
      
      deep_loss_sum <- 0
      sc_loss_sum <- 0
      diploid_sum <- 0
      lowl_gain_sum <- 0
      highl_gain_sum <- 0
      
      for (k in 2:dim(cna)[2]) { # loop through entire row of the CNA sample data under inspection
        if (cna[i, k] == -2) {
          deep_loss_sum = deep_loss_sum + 1
        }
        else if (cna[i, k] == -1) {
          sc_loss_sum = sc_loss_sum + 1
        }
        else if (cna[i, k] == 0) {
          diploid_sum = diploid_sum + 1
        }
        else if (cna[i, k] == 1) {
          lowl_gain_sum = lowl_gain_sum + 1
        }
        else if (cna[i, k] == 2) {
          highl_gain_sum = highl_gain_sum + 1
        }
        else {
           # cna[i, k]
        }
      }
      
      main$deep_loss[j] <- deep_loss_sum
      main$sc_loss[j] <- sc_loss_sum
      main$diploid[j] <- diploid_sum
      main$lowl_gain[j] <- lowl_gain_sum
      main$highl_gain[j] <- highl_gain_sum
      
    }
  }
}

write.csv(main, "C:/Users/kauls15/Desktop/ml_now/with_cna/with_cna_1.01.csv")