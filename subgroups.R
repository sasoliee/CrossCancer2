library(stats)
library(Rtsne)

# 
cancerOrder.df <- read.csv("C:/Users/solis/Desktop/Research/CC/CO2.csv")
typeof(cancerOrder.df)
#transposeCancerOrder <- t(cancerOrder.df) might need this ?
# generate cancerOrder list for subgroup 1
subgroup1 <- list() 

for(cancer in cancerOrder.df$X0){
  if (cancer == 'Breast' | cancer == 'Lung'){
    append(subgroup1, cancer)
  }
}

