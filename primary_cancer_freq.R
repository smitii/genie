# import clinical data 
library(readr)
main_freq <- read_csv("C:/Users/kauls15/Desktop/ml_now/cancer_freq/our_data_1.05.csv")
main_freq <- main_freq[, -1:-5]
main_freq <- main_freq[, -2:-4]
main_freq <- as.data.frame(main_freq)

count <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)

for (i in 1:dim(main_freq)[1]) { 
  ctype <- main_freq[i,1]
  count[ctype] <- count[ctype] + 1
}

max(count)
which(count == max(count))

# frequency 1259 at index 7  => bowel => most frequent
# frequency 1244 at index 18 => lung  => second-most frequent
