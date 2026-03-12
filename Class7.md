# Class 7: Machine Learning 1
Zahra Rashidi (A18561538)

## Background

Today we will begin our explortaion of importnat machine learning
methods with a focus on **clustering** and **dimensionally reduction**.

To start testing these methods let;s make up some sample data to
clusterwhere we know what the answer should be.

``` r
hist( rnorm(3000, mean=10) )
```

![](Class7.markdown_strict_files/figure-markdown_strict/unnamed-chunk-1-1.png)

> Q. Can you generate 30 numbers centered at + and 30 numbers at -3
> taken from at random from a normal distribution?

``` r
tmp <- c (rnorm(n=30, mean=3) ,
          rnorm(30,mean=-3) )

x <- cbind(x=tmp, y=rev(tmp))
```

``` r
plot(x)
```

![](Class7.markdown_strict_files/figure-markdown_strict/unnamed-chunk-3-1.png)

## K-means clustering

The main function in “base R” for K-means clustering is called
`kmeans()`, let’s try it out

``` r
k<- kmeans(x, center=2)
k
```

    K-means clustering with 2 clusters of sizes 30, 30

    Cluster means:
              x         y
    1  2.943219 -3.023726
    2 -3.023726  2.943219

    Clustering vector:
     [1] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 2 2 2 2 2 2 2 2
    [39] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2

    Within cluster sum of squares by cluster:
    [1] 51.28577 51.28577
     (between_SS / total_SS =  91.2 %)

    Available components:

    [1] "cluster"      "centers"      "totss"        "withinss"     "tot.withinss"
    [6] "betweenss"    "size"         "iter"         "ifault"      

> Q. What component of your k means result object has the cluster
> centers?

``` r
k$centers
```

              x         y
    1  2.943219 -3.023726
    2 -3.023726  2.943219

> Q. What component of your kmeans result object has the cluster
> membership vector (i.e. the main clustering result: which points are
> in which cluster)?

``` r
k$cluster
```

     [1] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 2 2 2 2 2 2 2 2
    [39] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2

> Q. Plot the results of clustering (i.e. our data colored by the
> clusteirng result) alone with the cluster centers

``` r
plot(x, col=k$cluster)
points(k$centers, col="red", pch=15, cex=2)
```

![](Class7.markdown_strict_files/figure-markdown_strict/unnamed-chunk-7-1.png)

> Q. can you run `kmeans()` again and cluster `x` into 4 clusters and
> plot the results just like we did above with coloring by cluster and
> the cluster centers shown in blue?

``` r
k<- kmeans(x, center=4)
k$centers
```

              x         y
    1  2.350357 -3.044093
    2  4.326565 -2.976202
    3 -3.044093  2.350357
    4 -2.976202  4.326565

``` r
k$cluster
```

     [1] 1 1 1 2 1 1 1 1 1 2 1 2 2 1 1 2 1 1 2 2 2 1 1 2 1 1 1 1 1 1 3 3 3 3 3 3 4 3
    [39] 3 4 4 4 3 3 4 3 3 4 4 3 4 3 3 3 3 3 4 3 3 3

``` r
plot(x, col=k$cluster)
points(k$centers, col="red", pch=15, cex=2)
```

![](Class7.markdown_strict_files/figure-markdown_strict/unnamed-chunk-8-1.png)

> Q. **Key-point** Kmeans will always return the clustering that we ask
> for (this the “k” or “centers” in K-means)

``` r
k$tot.withinss
```

    [1] 53.30545

## Hierachical clustering

The main function for hierarchical clustering in base R is called
`hclust()`. One of the main differences with respect to the `kmeans()`
function is that you can not just pass your input data directly to
`hclust()` - it needs a “distance matrix” as input. We can get this from
lots of places including the `dist()` function.

``` r
d <- dist(x)
hc <- hclust(d)

plot(hc)
```

![](Class7.markdown_strict_files/figure-markdown_strict/unnamed-chunk-10-1.png)

``` r
grps <- cutree(hc, h=10)
```

We can “cut” the dendrogram or “tree” at a given height to yeild our
“clsuters:. For thsi we use the function `cutree()`

``` r
plot(hc)
abline (h=10, col="red")
```

![](Class7.markdown_strict_files/figure-markdown_strict/unnamed-chunk-11-1.png)

``` r
cutree(hc,h =10)
```

     [1] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 2 2 2 2 2 2 2 2
    [39] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2

``` r
 grps
```

     [1] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 2 2 2 2 2 2 2 2
    [39] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2

> Q. PLot our data `x` colored by the cluster in result from `hclust()`
> and `cutree()`

``` r
plot(x,col=grps)
```

![](Class7.markdown_strict_files/figure-markdown_strict/unnamed-chunk-13-1.png)

``` r
plot(hc)
abline(h=4.9, col="red")
```

![](Class7.markdown_strict_files/figure-markdown_strict/unnamed-chunk-14-1.png)

## Principal Component Analysis (PCA)

PCA is a popua;r dimensopna;oty reduction technique that is widely used
in bioformatics

### PCS of UK food data

``` r
url <- "https://tinyurl.com/UK-foods"
x <- read.csv(url)
```

> Q1. How many rows and columns are in your new data frame named x? What
> R functions could you use to answer this questions?

``` r
ncol(x)
```

    [1] 5

``` r
nrow(x)
```

    [1] 17

> Q2. Which approach to solving the ‘row-names problem’ mentioned above
> do you prefer and why? Is one approach more robust than another under
> certain circumstances?

``` r
x <- read.csv(url, row.names = 1)
```

> Q3. Changing what optional argument in the above barplot() function
> results in the following plot?

``` r
barplot(as.matrix(x), col=rainbow(nrow(x)))
```

![](Class7.markdown_strict_files/figure-markdown_strict/unnamed-chunk-19-1.png)

``` r
library(tidyr)

# Convert data to long format for ggplot with `pivot_longer()`
x_long <- x |> 
          tibble::rownames_to_column("Food") |> 
          pivot_longer(cols = -Food, 
                       names_to = "Country", 
                       values_to = "Consumption")

dim(x_long)
```

    [1] 68  3

``` r
# Create grouped bar plot
library(ggplot2)

ggplot(x_long) +
  aes(x = Country, y = Consumption, fill = Food) +
  geom_col(position = "dodge") +
  theme_bw()
```

![](Class7.markdown_strict_files/figure-markdown_strict/unnamed-chunk-21-1.png)

> Q4. Changing what optional argument in the above ggplot() code results
> in a stacked barplot figure?

``` r
library(ggplot2)

ggplot(x_long) +
  aes(x = Country, y = Consumption, fill = Food) +
  geom_col(position = "stack") +
  theme_bw()
```

![](Class7.markdown_strict_files/figure-markdown_strict/unnamed-chunk-22-1.png)

> Q5. We can use the pairs() function to generate all pairwise plots for
> our countries. Can you make sense of the following code and resulting
> figure? What does it mean if a given point lies on the diagonal for a
> given plot?

``` r
pairs(x, col=rainbow(nrow(x)), pch=16)
```

![](Class7.markdown_strict_files/figure-markdown_strict/unnamed-chunk-23-1.png)

Each point in a scatterplot represents one food time. Its position shows
how much taht is consumed in the two countries being compared. If a
given point lies on the diagonal for a given plot, it represents
agreemnet between two fountries for that food item.

## Heatmap

``` r
library(pheatmap)

pheatmap( as.matrix(x) )
```

![](Class7.markdown_strict_files/figure-markdown_strict/unnamed-chunk-24-1.png)

Of all these plot really only the `pairs()` plot was useful. This
however took a bit of work to interpret and will not scale when I am
lookin at much bigger datasets

> Q6. Based on the pairs and heatmap figures, which countries cluster
> together and what does this suggest about their food consumption
> patterns? Can you easily tell what the main differences between N.
> Ireland and the other countries of the UK in terms of this data-set?

England, Wales, and Scotland cluster together, this means their
consumption pattersn across the 17 foods are very similar. N. Ireland
tends to appear as the odd on in both visualizations. In the heatmap, it
shows different intensities for several foods and in the dendrogram, it
has its own bronch

> Q7.

``` r
# Use the prcomp() PCA function 
pca <- prcomp( t(x) )
summary(pca)
```

    Importance of components:
                                PC1      PC2      PC3     PC4
    Standard deviation     324.1502 212.7478 73.87622 2.7e-14
    Proportion of Variance   0.6744   0.2905  0.03503 0.0e+00
    Cumulative Proportion    0.6744   0.9650  1.00000 1.0e+00

``` r
# Create a data frame for plotting
df <- as.data.frame(pca$x)
df$Country <- rownames(df)

# Plot PC1 vs PC2 with ggplot
ggplot(pca$x) +
  aes(x = PC1, y = PC2, label = rownames(pca$x)) +
  geom_point(size = 3) +
  geom_text(vjust = -0.5) +
  xlim(-270, 500) +
  xlab("PC1") +
  ylab("PC2") +
  theme_bw()
```

![](Class7.markdown_strict_files/figure-markdown_strict/unnamed-chunk-26-1.png)

> Q8. Customize your plot so that the colors of the country names match
> the colors in our UK and Ireland map and table at start of this
> document.

ˆ

``` r
country_cols <- c( "England" = "red", "Wales" = "green", "Scotland" = "blue", "N.Ireland" = "orange" ) 
ggplot(pca$x) + aes(x = PC1, y = PC2, label = rownames(pca$x), color = rownames(pca$x)) + geom_point(size = 3) + geom_text(vjust = -0.5) + scale_color_manual(values = country_cols) + xlim(-270, 500) + xlab("PC1") + ylab("PC2") + theme_bw() + theme(legend.position = "none")
```

![](Class7.markdown_strict_files/figure-markdown_strict/unnamed-chunk-27-1.png)

> Q9: Generate a similar ‘loadings plot’ for PC2. What two food groups
> feature prominantely and what does PC2 maninly tell us about?

``` r
ggplot(pca$rotation) +
  aes(x = PC2,
      y = reorder(rownames(pca$rotation), PC2)) +
  geom_col(fill = "steelblue") +
  xlab("PC2 Loading Score") +
  ylab("") +
  theme_bw() +
  theme(axis.text.y = element_text(size = 9))
```

![](Class7.markdown_strict_files/figure-markdown_strict/unnamed-chunk-28-1.png)

> Q10: How many genes and samples are in this data set? How many PCs do
> you think it will take to have a useful overview of this data set (see
> below)?

``` r
url2 <- "https://tinyurl.com/expression-CSV"
rna.data <- read.csv(url2, row.names=1)
head(rna.data)
```

           wt1 wt2  wt3  wt4 wt5 ko1 ko2 ko3 ko4 ko5
    gene1  439 458  408  429 420  90  88  86  90  93
    gene2  219 200  204  210 187 427 423 434 433 426
    gene3 1006 989 1030 1017 973 252 237 238 226 210
    gene4  783 792  829  856 760 849 856 835 885 894
    gene5  181 249  204  244 225 277 305 272 270 279
    gene6  460 502  491  491 493 612 594 577 618 638

There are 10 samples and 6 genes. I think it will take 2-3 PCs to have a
useful overview of this data set.

## PCA the rescue

The main function in “base R” for PCA is called `prcomp()`.

``` r
pca <- prcomp( t(x) )
summary(pca)
```

    Importance of components:
                                PC1      PC2      PC3     PC4
    Standard deviation     324.1502 212.7478 73.87622 2.7e-14
    Proportion of Variance   0.6744   0.2905  0.03503 0.0e+00
    Cumulative Proportion    0.6744   0.9650  1.00000 1.0e+00

> Q. How much variance is captured in the first PC?

67.4%

> Q. How many PCs do I need to capture at leats 90% of the total varance
> in the dataset?

2 PCs capture 96.5% of the total varance

> Q. Plot our main PCA result. Folds can call this different things
> depending on thier field of study e.g. “PC plot”, “ordientation plot”
> “Score plot,”PC1 vs PC2 plot”…

``` r
attributes(pca)
```

    $names
    [1] "sdev"     "rotation" "center"   "scale"    "x"       

    $class
    [1] "prcomp"

To generate our PCA score plot we want the `pca$x` component of the
result object

``` r
pca$x
```

                     PC1         PC2        PC3           PC4
    England   -144.99315   -2.532999 105.768945  1.612425e-14
    Wales     -240.52915 -224.646925 -56.475555  4.751043e-13
    Scotland   -91.86934  286.081786 -44.415495 -6.044349e-13
    N.Ireland  477.39164  -58.901862  -4.877895  1.145386e-13

``` r
my_cols <- c("orange", "red", "blue", "darkgreen")
plot(pca$x[,1],pca$x[,2], col=my_cols, pch=16)
```

![](Class7.markdown_strict_files/figure-markdown_strict/unnamed-chunk-33-1.png)

``` r
library (ggplot2)
ggplot(pca$x)+
  aes(PC1,PC2) +
  geom_point(col=my_cols)
```

![](Class7.markdown_strict_files/figure-markdown_strict/unnamed-chunk-34-1.png)
