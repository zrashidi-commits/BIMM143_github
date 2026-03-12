# Class17
Zahra Rashidi (A18561538)

## 

``` r
library(tximport)

# setup the folder and filenames to read
folders <- dir(pattern="SRR21568*")
samples <- sub("_quant", "", folders)
files <- file.path( folders, "abundance.h5" )
names(files) <- samples

txi.kallisto <- tximport(files, type = "kallisto", txOut = TRUE)
```

    1 2 3 4 

``` r
head(txi.kallisto$counts)
```

                    SRR2156848 SRR2156849 SRR2156850 SRR2156851
    ENST00000539570          0          0    0.00000          0
    ENST00000576455          0          0    2.62037          0
    ENST00000510508          0          0    0.00000          0
    ENST00000474471          0          1    1.00000          0
    ENST00000381700          0          0    0.00000          0
    ENST00000445946          0          0    0.00000          0

``` r
colSums(txi.kallisto$counts)
```

    SRR2156848 SRR2156849 SRR2156850 SRR2156851 
       2563611    2600800    2372309    2111474 

``` r
sum(rowSums(txi.kallisto$counts)>0)
```

    [1] 94561

``` r
to.keep <- rowSums(txi.kallisto$counts) > 0
kset.nonzero <- txi.kallisto$counts[to.keep,]
```

``` r
keep2 <- apply(kset.nonzero,1,sd)>0
x <- kset.nonzero[keep2,]
```

## Principal Component Analysis

``` r
pca <- prcomp(t(x), scale=TRUE)
```

``` r
summary(pca)
```

    Importance of components:
                                PC1      PC2      PC3   PC4
    Standard deviation     183.6379 177.3605 171.3020 1e+00
    Proportion of Variance   0.3568   0.3328   0.3104 1e-05
    Cumulative Proportion    0.3568   0.6895   1.0000 1e+00

``` r
plot(pca$x[,1], pca$x[,2],
     col=c("blue","blue","red","red"),
     xlab="PC1", ylab="PC2", pch=16)
```

![](Class16.markdown_strict_files/figure-markdown_strict/unnamed-chunk-9-1.png)

> Q. Use ggplot to make a similar figure of PC1 vs PC2 and a seperate
> figure PC1 vs PC3 and PC2 vs PC3.

``` r
library(ggplot2)

pc_df <- as.data.frame(pca$x)
pc_df$sample <- rownames(pc_df)

# Assign groups based on sample names
pc_df$group <- ifelse(grepl("SRR2156848|SRR2156849", pc_df$sample),
                      "Control",
                      "CRISPR")
```

``` r
ggplot(pc_df, aes(x = PC1, y = PC3, color = group, label = sample)) +
  geom_point(size = 4) +
  geom_text(vjust = -0.7, size = 3) +
  scale_color_manual(values = c("Control"="blue", "CRISPR"="red")) +
  theme_bw() +
  labs(title = "PC1 vs PC3",
      x = "PC1",
       y = "PC3",
       color = "Group")
```

![](Class16.markdown_strict_files/figure-markdown_strict/unnamed-chunk-11-1.png)

``` r
ggplot(pc_df, aes(x = PC2, y = PC3, color = group, label = sample)) +
  geom_point(size = 4) +
  geom_text(vjust = -0.7, size = 3) +
  scale_color_manual(values = c("Control"="blue", "CRISPR"="red")) +
  theme_bw() +
  labs(title = "PC2 vs PC3",
       x = "PC2",
       y = "PC3",
       color = "Group")
```

![](Class16.markdown_strict_files/figure-markdown_strict/unnamed-chunk-12-1.png)
