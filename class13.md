# class13
Zahra Rashidi

## Background

Today we will perform an RNASeq analysis of the effects of a common
steroid on airway cells.

In particular, dexamethasone (hereafter just called “dex”) on different
airway smooyh muscle cell lines (ASM cells).

## Data Import

We need two different inputs:

-   **countData**: with genes in rows and experiments in columns
-   **colData**: meta data that describes the columns in countData

``` r
counts <- read.csv("airway_scaledcounts.csv", row.names=1)
metadata <- read.csv("airway_metadata.csv")
```

``` r
head(counts)
```

                    SRR1039508 SRR1039509 SRR1039512 SRR1039513 SRR1039516
    ENSG00000000003        723        486        904        445       1170
    ENSG00000000005          0          0          0          0          0
    ENSG00000000419        467        523        616        371        582
    ENSG00000000457        347        258        364        237        318
    ENSG00000000460         96         81         73         66        118
    ENSG00000000938          0          0          1          0          2
                    SRR1039517 SRR1039520 SRR1039521
    ENSG00000000003       1097        806        604
    ENSG00000000005          0          0          0
    ENSG00000000419        781        417        509
    ENSG00000000457        447        330        324
    ENSG00000000460         94        102         74
    ENSG00000000938          0          0          0

``` r
metadata
```

              id     dex celltype     geo_id
    1 SRR1039508 control   N61311 GSM1275862
    2 SRR1039509 treated   N61311 GSM1275863
    3 SRR1039512 control  N052611 GSM1275866
    4 SRR1039513 treated  N052611 GSM1275867
    5 SRR1039516 control  N080611 GSM1275870
    6 SRR1039517 treated  N080611 GSM1275871
    7 SRR1039520 control  N061011 GSM1275874
    8 SRR1039521 treated  N061011 GSM1275875

> Q1. How many genes are in this dataset?

``` r
nrow(counts)
```

    [1] 38694

> Q2. How many ‘control’ cell lines do we have?

``` r
table(metadata$dex)
```


    control treated 
          4       4 

##Differential gene expression

We have 4 replicate drug treated and control (no drug) clumns?experments
in our ‘counts’ object.

We want one “mean” value for each gene (rows) in “treated” (drug) and
one mean value for each gene in “control” cols.

Step 1. find all “control” colums Step 2. Extract these colums to a new
object called ‘control.counts’ Step 3. Then caculate the mean value fo
each gene

``` r
contol.inds <- metadata$dex=="control"
```

Step 2.

``` r
control.counts <- counts[ ,contol.inds]
```

Step 3.

``` r
control.mean <-rowMeans(control.counts)
```

``` r
metadata
```

              id     dex celltype     geo_id
    1 SRR1039508 control   N61311 GSM1275862
    2 SRR1039509 treated   N61311 GSM1275863
    3 SRR1039512 control  N052611 GSM1275866
    4 SRR1039513 treated  N052611 GSM1275867
    5 SRR1039516 control  N080611 GSM1275870
    6 SRR1039517 treated  N080611 GSM1275871
    7 SRR1039520 control  N061011 GSM1275874
    8 SRR1039521 treated  N061011 GSM1275875

Now do the same thing for the “treated” colums/experiments

``` r
treated.mean <- rowMeans(counts[ ,metadata$dex=="treated"])
```

``` r
meancounts<- data.frame(control.mean, treated.mean)
```

``` r
plot(meancounts)
```

![](class13.markdown_strict_files/figure-markdown_strict/unnamed-chunk-12-1.png)

Let’s log transform this count data:

``` r
plot(meancounts, log="xy")
```

    Warning in xy.coords(x, y, xlabel, ylabel, log): 15032 x values <= 0 omitted
    from logarithmic plot

    Warning in xy.coords(x, y, xlabel, ylabel, log): 15281 y values <= 0 omitted
    from logarithmic plot

![](class13.markdown_strict_files/figure-markdown_strict/unnamed-chunk-13-1.png)

**N.B.** We most often use log2 for this type of data as it makes the
interpretation much more straightforward.

Treated/Control is often called “fold-change”.

if there was no change we would have a log2-fc of zero:

``` r
log2(10/10)
```

    [1] 0

if we had double the amount of trancript around we would have a log2-fc
of 1:

``` r
log2(20/10)
```

    [1] 1

if we had half as much trancript around we would have a log2-fc of -1

``` r
log2(5/10)
```

    [1] -1

> Q. Caculate a log2 fold change value for all our genes and add it as a
> new column to our ‘meanscounts’ object.

``` r
meancounts$log2f<- log2 (meancounts$treated.mean / 
                        meancounts$control.mean)
```

``` r
head(meancounts)
```

                    control.mean treated.mean       log2f
    ENSG00000000003       900.75       658.00 -0.45303916
    ENSG00000000005         0.00         0.00         NaN
    ENSG00000000419       520.50       546.00  0.06900279
    ENSG00000000457       339.75       316.50 -0.10226805
    ENSG00000000460        97.25        78.75 -0.30441833
    ENSG00000000938         0.75         0.00        -Inf

There are some “funky” values that come about when ever we have 0 mean
count values. Typically remove these genes from any further analysis- as
we can’t say anything about them if we have no data for them.

``` r
log2(40/10)
```

    [1] 2

##DESeq anlaysis

let’s do this anlaysis with an estimate of statiscal significance using
the **DESeq2** package.

``` r
library(DESeq2)
```

    Warning: package 'IRanges' was built under R version 4.4.2

    Warning: package 'GenomeInfoDb' was built under R version 4.4.2

    Warning: package 'MatrixGenerics' was built under R version 4.4.2

DESeq (like bioconductor packages) want it’s input data in a very
specific way.

``` r
dds <-DESeqDataSetFromMatrix(countData= counts, 
                       colData = metadata, 
                       design = ~dex)
```

    converting counts to integer mode

    Warning in DESeqDataSet(se, design = design, ignoreRank): some variables in
    design formula are characters, converting to factors

### Run the DESeq anlaysis pipline

The main function ‘DESeq()’

``` r
dds <- DESeq(dds) 
```

    estimating size factors

    estimating dispersions

    gene-wise dispersion estimates

    mean-dispersion relationship

    final dispersion estimates

    fitting model and testing

``` r
res<- results(dds)  
head(res)
```

    log2 fold change (MLE): dex treated vs control 
    Wald test p-value: dex treated vs control 
    DataFrame with 6 rows and 6 columns
                      baseMean log2FoldChange     lfcSE      stat    pvalue
                     <numeric>      <numeric> <numeric> <numeric> <numeric>
    ENSG00000000003 747.194195     -0.3507030  0.168246 -2.084470 0.0371175
    ENSG00000000005   0.000000             NA        NA        NA        NA
    ENSG00000000419 520.134160      0.2061078  0.101059  2.039475 0.0414026
    ENSG00000000457 322.664844      0.0245269  0.145145  0.168982 0.8658106
    ENSG00000000460  87.682625     -0.1471420  0.257007 -0.572521 0.5669691
    ENSG00000000938   0.319167     -1.7322890  3.493601 -0.495846 0.6200029
                         padj
                    <numeric>
    ENSG00000000003  0.163035
    ENSG00000000005        NA
    ENSG00000000419  0.176032
    ENSG00000000457  0.961694
    ENSG00000000460  0.815849
    ENSG00000000938        NA

``` r
36000*0.05
```

    [1] 1800

##Volcano plot

This is a main summary results figure from these kinds of studies. it is
a plot of log2-foldchnage vs of (Adjusted) P-value.

``` r
plot(res$log2FoldChange, 
     res$padj)
```

![](class13.markdown_strict_files/figure-markdown_strict/unnamed-chunk-25-1.png)

Again this y-axis is a highly needs log transforming and we can flip the
y-axis with a minus sign so it looks like every other volcano plot.

``` r
plot(res$log2FoldChange, 
     -log(res$padj)) 
abline(v=-2, col="red")  
       abline(v=+2, col="red") 
       abline(h=-log(0.05), col="red")
```

![](class13.markdown_strict_files/figure-markdown_strict/unnamed-chunk-26-1.png)

###Adding some colors annotation

Start with default base color “gray”

``` r
# Start with a default base color "gray"
mycols <- rep("gray", nrow(res))
mycols[ res$log2FoldChange > 2 ] <- "blue"
mycols[ res$log2FoldChange < -2 ] <- "darkgreen"

mycols[res$padj >=0.05]<-"pink"

plot(res$log2FoldChange,
     -log(res$padj),
     col = mycols)


#Volcano 
plot(res$log2FoldChange, 
     -log(res$padj),
     col=mycols)
abline(v=c(-2,+2), lty=2)
abline(h=-log(0.05), lty=2)
```

![](class13.markdown_strict_files/figure-markdown_strict/unnamed-chunk-27-1.png)

> Q.Make a presentation quailty ggplot version quality of this plot.
> Include clear axis labels, a clean theme , your custom colors, cut-off
> lines and a plot title.

``` r
library(ggplot2)
```

    Warning: package 'ggplot2' was built under R version 4.4.3

``` r
df <- as.data.frame(res)
df <- df[!is.na(df$log2FoldChange) & !is.na(df$padj), ]

# same color logic as your base plot
df$color <- "gray"
df$color[df$log2FoldChange > 2]  <- "blue"
df$color[df$log2FoldChange < -2] <- "darkgreen"
df$color[df$padj >= 0.05]        <- "pink"

ggplot(df, aes(x = log2FoldChange, y = -log10(padj))) +
  geom_point(aes(color = color), size = 1.5) +
  geom_vline(xintercept = c(-2, 2), linetype = "dashed") +
  geom_hline(yintercept = -log10(0.05), linetype = "dashed") +
  scale_color_identity() +
  labs(
    title = "Volcano Plot",
    x = "Log2 Fold Change",
    y = "-Log10 Adjusted P-value"
  ) +
  theme_classic()
```

![](class13.markdown_strict_files/figure-markdown_strict/unnamed-chunk-28-1.png)

## Save our results

Write a CVS file

``` r
write.csv(res, file="results.csv")
```
