
library(stats)

# reading data
#seriesMatrix.df <- read.csv("C:/Users/solis/Desktop/Research/CC/SM/SM2-06.csv")
cancerOrder.df <- read.csv("C:/Users/solis/Desktop/Research/CC/CO2.csv")
geneAverage.df <- read.csv("C:/Users/solis/Desktop/Research/CC/GM-SM.csv")
geneName = geneAverage.df$Gene
#seriesMatrix.df$X = NULL
cancerOrder.df[c("X")] <- NULL
geneAverage.df$X <- NULL
geneAverage.df$Gene <- NULL
#cancerOrder.df2 <- cancerOrder.df[, c(5)]


#df_new2 <- seriesMatrix.df[ , unlist(lapply(seriesMatrix.df, is.numeric))]
df_new3 <- geneAverage.df[, unlist(lapply(geneAverage.df, is.numeric))]

# Define Min-Max Normalization Function
min_max_norm <- function(x) {
  if (is.numeric(x)) {  # Check if column is numeric
    return((x - min(x)) / (max(x) - min(x)))
  } else {
    return(x)  # Return unchanged if not numeric
  }
}

# Apply the function only to numeric columns
#sm_numeric <- seriesMatrix.df[, sapply(seriesMatrix.df, is.numeric)]  # Select only numeric columns
#sm_normalized <- as.data.frame(lapply(df_new2, min_max_norm))  # Normalize them
ga_normalized <- as.data.frame(lapply(df_new3, min_max_norm))  # Normalize them

# compute PCs on 2-D
#CCpcs1 <- prcomp(data.frame(sm_normalized))
#summary(CCpcs1)
#scores1 <- CCpcs1$x
#head(scores,5)

# compute PCs on 2-D
CCpcs <- prcomp(data.frame(ga_normalized))
summary(CCpcs)
scores <- CCpcs$x
head(scores,5)

# define color for each of the  6 cancers
colors <- c("#0096FF","#FF7F00","#008B00","#FF0000","#7D26CD","#8B4513")
colors2 <- colors[as.factor(cancerOrder.df$x)]




# plot for PC1 v. PC2
plot(scores[,1], scores[,2], xlab = "PC1 (1.1965%)", ylab = "PC2 (0.01875%)",col=colors)
title(main="PC1 vs. PC2")
#legend(2, 4, legend=c("Breast", "Colon", "Kidney","Ovary","Uterus","Lung"),  
 #      fill = c("#0096FF","#FF7F00","#008B00","#FF0000","#7D26CD","#8B4513") 
#)
# plot for PC1 v. PC3
plot(scores[,1], scores[,3], xlab = "PC1", ylab = "PC3", col = colors)
title(main="PC1 vs. PC3")
# plot for PC2 v. PC3
plot(scores[,2], scores[,3], xlab = "PC2", ylab = "PC3", col = colors)
title(main="PC2 vs. PC3")
#plot for PC1 v. PC4
plot(scores[,1], scores[,4], xlab = "PC1", ylab = "PC4", col = colors)
title(main="PC1 vs. PC4")
#plot for PC2 v. PC4
plot(scores[,2], scores[,4], xlab = "PC2", ylab = "PC4", col = colors)
title(main = "PC2 vs. PC4")
#plot for PC3 v. PC4
plot(scores[,3], scores[,4], xlab = "PC3", ylab = "PC4", col = colors)
title(main = "PC3 vs. PC4")

#---------------------------------------------------------------
#umap plot

library(umap)
library(ggplot2)

set.seed(123)  # For reproducibility

# Perform UMAP
umap_result <- umap(ga_normalized)

# Extract UMAP coordinates
umap_df <- as.data.frame(umap_result$layout)
colnames(umap_df) <- c("UMAP1", "UMAP2")

# Add Cancer Type Labels
umap_df$Cancer_Type <- cancerOrder.df$X0  # Replace with actual column name

sm.umap <- umap(ga_normalized)
head(sm.umap$layout, 6)
sm.labels <- colors

# Define Colors for Cancer Types
cancer_colors <- c("#0096FF", "#FF7F00", "#008B00", "#FF0000", "#7D26CD", "#8B4513")
names(cancer_colors) <- unique(umap_df$Cancer_Type)

# UMAP Plot
ggplot(umap_df, aes(x = UMAP1, y = UMAP2, color = Cancer_Type)) +
  geom_point(size = 1, alpha = 0.8) +  # Scatter plot points
  scale_color_manual(values = cancer_colors) +  # Apply custom colors
  labs(title = "UMAP Projection of Cancer Data", x = "UMAP1", y = "UMAP2", color = "Cancer Type") +
  theme_minimal()

#-----------------------------------------------------
#tsne plot 
library(M3C)

tsne(pollen$data,labels=as.factor(pollen$celltypes))

