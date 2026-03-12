# class 09
Zahra Rashidi (A18561538)

## Background

In this mini-project, you will explore FiveThirtyEight’s Halloween Candy
dataset.

We will use lots of ggplot, some basic stats, correlation analysis and
PCA to make sense of the landscape of US candy – something hopefully
more relatable than the proteomics and transcriptomics that we will use
these methods on throughout the rest of the course.

## Data Import

Our dataset is a CSV file so we use read.csv()

``` r
candy <- read.csv(
  "https://raw.githubusercontent.com/fivethirtyeight/data/master/candy-power-ranking/candy-data.csv",
  row.names = 1
)
```

> Q1. How many different candy types are in this dataset?

There are r nrow(candy) rows in this dataset.b

``` r
nrow(candy)
```

    [1] 85

> Q2. How many fruity candy types are in the dataset?

``` r
sum(candy$fruity)
```

    [1] 38

> Q3. What is your favorite candy (other than Twix) in the dataset and
> what is its winpercent value?

``` r
library(dplyr)
```

    Warning: package 'dplyr' was built under R version 4.4.3


    Attaching package: 'dplyr'

    The following objects are masked from 'package:stats':

        filter, lag

    The following objects are masked from 'package:base':

        intersect, setdiff, setequal, union

Attaching package: ‘dplyr’ The following objects are masked from
‘package:stats’: filter, lag The following objects are masked from
‘package:base’: intersect, setdiff, setequal, union

``` r
candy |>
  filter(row.names(candy) == "Air Heads") |>
  select(winpercent)
```

              winpercent
    Air Heads   52.34146

Q4. What is the winpercent value for “Kit Kat”?

``` r
candy |>
  filter(row.names(candy)=="Kit Kat") |>
  select(winpercent)
```

            winpercent
    Kit Kat    76.7686

Q5. What is the winpercent value for “Tootsie Roll Snack Bars”?

``` r
candy |>
  filter(row.names(candy)=="Tootsie Roll Snack Bars") |>
  select(winpercent)
```

                            winpercent
    Tootsie Roll Snack Bars    49.6535

Q6. Is there any variable/column that looks to be on a different scale
to the majority of the other columns in the dataset?

``` r
 library(skimr)
```

    Warning: package 'skimr' was built under R version 4.4.3

``` r
skim(candy)
```

<table>
<caption>Data summary</caption>
<tbody>
<tr class="odd">
<td style="text-align: left;">Name</td>
<td style="text-align: left;">candy</td>
</tr>
<tr class="even">
<td style="text-align: left;">Number of rows</td>
<td style="text-align: left;">85</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Number of columns</td>
<td style="text-align: left;">12</td>
</tr>
<tr class="even">
<td style="text-align: left;">_______________________</td>
<td style="text-align: left;"></td>
</tr>
<tr class="odd">
<td style="text-align: left;">Column type frequency:</td>
<td style="text-align: left;"></td>
</tr>
<tr class="even">
<td style="text-align: left;">numeric</td>
<td style="text-align: left;">12</td>
</tr>
<tr class="odd">
<td style="text-align: left;">________________________</td>
<td style="text-align: left;"></td>
</tr>
<tr class="even">
<td style="text-align: left;">Group variables</td>
<td style="text-align: left;">None</td>
</tr>
</tbody>
</table>

Data summary

**Variable type: numeric**

<table style="width:100%;">
<colgroup>
<col style="width: 19%" />
<col style="width: 11%" />
<col style="width: 15%" />
<col style="width: 6%" />
<col style="width: 6%" />
<col style="width: 6%" />
<col style="width: 6%" />
<col style="width: 6%" />
<col style="width: 6%" />
<col style="width: 6%" />
<col style="width: 6%" />
</colgroup>
<thead>
<tr class="header">
<th style="text-align: left;">skim_variable</th>
<th style="text-align: right;">n_missing</th>
<th style="text-align: right;">complete_rate</th>
<th style="text-align: right;">mean</th>
<th style="text-align: right;">sd</th>
<th style="text-align: right;">p0</th>
<th style="text-align: right;">p25</th>
<th style="text-align: right;">p50</th>
<th style="text-align: right;">p75</th>
<th style="text-align: right;">p100</th>
<th style="text-align: left;">hist</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">chocolate</td>
<td style="text-align: right;">0</td>
<td style="text-align: right;">1</td>
<td style="text-align: right;">0.44</td>
<td style="text-align: right;">0.50</td>
<td style="text-align: right;">0.00</td>
<td style="text-align: right;">0.00</td>
<td style="text-align: right;">0.00</td>
<td style="text-align: right;">1.00</td>
<td style="text-align: right;">1.00</td>
<td style="text-align: left;">▇▁▁▁▆</td>
</tr>
<tr class="even">
<td style="text-align: left;">fruity</td>
<td style="text-align: right;">0</td>
<td style="text-align: right;">1</td>
<td style="text-align: right;">0.45</td>
<td style="text-align: right;">0.50</td>
<td style="text-align: right;">0.00</td>
<td style="text-align: right;">0.00</td>
<td style="text-align: right;">0.00</td>
<td style="text-align: right;">1.00</td>
<td style="text-align: right;">1.00</td>
<td style="text-align: left;">▇▁▁▁▆</td>
</tr>
<tr class="odd">
<td style="text-align: left;">caramel</td>
<td style="text-align: right;">0</td>
<td style="text-align: right;">1</td>
<td style="text-align: right;">0.16</td>
<td style="text-align: right;">0.37</td>
<td style="text-align: right;">0.00</td>
<td style="text-align: right;">0.00</td>
<td style="text-align: right;">0.00</td>
<td style="text-align: right;">0.00</td>
<td style="text-align: right;">1.00</td>
<td style="text-align: left;">▇▁▁▁▂</td>
</tr>
<tr class="even">
<td style="text-align: left;">peanutyalmondy</td>
<td style="text-align: right;">0</td>
<td style="text-align: right;">1</td>
<td style="text-align: right;">0.16</td>
<td style="text-align: right;">0.37</td>
<td style="text-align: right;">0.00</td>
<td style="text-align: right;">0.00</td>
<td style="text-align: right;">0.00</td>
<td style="text-align: right;">0.00</td>
<td style="text-align: right;">1.00</td>
<td style="text-align: left;">▇▁▁▁▂</td>
</tr>
<tr class="odd">
<td style="text-align: left;">nougat</td>
<td style="text-align: right;">0</td>
<td style="text-align: right;">1</td>
<td style="text-align: right;">0.08</td>
<td style="text-align: right;">0.28</td>
<td style="text-align: right;">0.00</td>
<td style="text-align: right;">0.00</td>
<td style="text-align: right;">0.00</td>
<td style="text-align: right;">0.00</td>
<td style="text-align: right;">1.00</td>
<td style="text-align: left;">▇▁▁▁▁</td>
</tr>
<tr class="even">
<td style="text-align: left;">crispedricewafer</td>
<td style="text-align: right;">0</td>
<td style="text-align: right;">1</td>
<td style="text-align: right;">0.08</td>
<td style="text-align: right;">0.28</td>
<td style="text-align: right;">0.00</td>
<td style="text-align: right;">0.00</td>
<td style="text-align: right;">0.00</td>
<td style="text-align: right;">0.00</td>
<td style="text-align: right;">1.00</td>
<td style="text-align: left;">▇▁▁▁▁</td>
</tr>
<tr class="odd">
<td style="text-align: left;">hard</td>
<td style="text-align: right;">0</td>
<td style="text-align: right;">1</td>
<td style="text-align: right;">0.18</td>
<td style="text-align: right;">0.38</td>
<td style="text-align: right;">0.00</td>
<td style="text-align: right;">0.00</td>
<td style="text-align: right;">0.00</td>
<td style="text-align: right;">0.00</td>
<td style="text-align: right;">1.00</td>
<td style="text-align: left;">▇▁▁▁▂</td>
</tr>
<tr class="even">
<td style="text-align: left;">bar</td>
<td style="text-align: right;">0</td>
<td style="text-align: right;">1</td>
<td style="text-align: right;">0.25</td>
<td style="text-align: right;">0.43</td>
<td style="text-align: right;">0.00</td>
<td style="text-align: right;">0.00</td>
<td style="text-align: right;">0.00</td>
<td style="text-align: right;">0.00</td>
<td style="text-align: right;">1.00</td>
<td style="text-align: left;">▇▁▁▁▂</td>
</tr>
<tr class="odd">
<td style="text-align: left;">pluribus</td>
<td style="text-align: right;">0</td>
<td style="text-align: right;">1</td>
<td style="text-align: right;">0.52</td>
<td style="text-align: right;">0.50</td>
<td style="text-align: right;">0.00</td>
<td style="text-align: right;">0.00</td>
<td style="text-align: right;">1.00</td>
<td style="text-align: right;">1.00</td>
<td style="text-align: right;">1.00</td>
<td style="text-align: left;">▇▁▁▁▇</td>
</tr>
<tr class="even">
<td style="text-align: left;">sugarpercent</td>
<td style="text-align: right;">0</td>
<td style="text-align: right;">1</td>
<td style="text-align: right;">0.48</td>
<td style="text-align: right;">0.28</td>
<td style="text-align: right;">0.01</td>
<td style="text-align: right;">0.22</td>
<td style="text-align: right;">0.47</td>
<td style="text-align: right;">0.73</td>
<td style="text-align: right;">0.99</td>
<td style="text-align: left;">▇▇▇▇▆</td>
</tr>
<tr class="odd">
<td style="text-align: left;">pricepercent</td>
<td style="text-align: right;">0</td>
<td style="text-align: right;">1</td>
<td style="text-align: right;">0.47</td>
<td style="text-align: right;">0.29</td>
<td style="text-align: right;">0.01</td>
<td style="text-align: right;">0.26</td>
<td style="text-align: right;">0.47</td>
<td style="text-align: right;">0.65</td>
<td style="text-align: right;">0.98</td>
<td style="text-align: left;">▇▇▇▇▆</td>
</tr>
<tr class="even">
<td style="text-align: left;">winpercent</td>
<td style="text-align: right;">0</td>
<td style="text-align: right;">1</td>
<td style="text-align: right;">50.32</td>
<td style="text-align: right;">14.71</td>
<td style="text-align: right;">22.45</td>
<td style="text-align: right;">39.14</td>
<td style="text-align: right;">47.83</td>
<td style="text-align: right;">59.86</td>
<td style="text-align: right;">84.18</td>
<td style="text-align: left;">▃▇▆▅▂</td>
</tr>
</tbody>
</table>

yea

Q7. What do you think a zero and one represent for the candy$chocolate
column?

``` r
candy$chocolate
```

     [1] 1 1 0 0 0 1 1 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1 0 1 1 0 0 0 1 1 0 1 1 1
    [39] 1 1 1 0 1 1 0 0 0 1 0 0 0 1 1 1 1 0 1 0 0 1 0 0 1 0 1 1 0 0 0 0 0 0 0 0 1 1
    [77] 1 1 0 1 0 0 0 0 1

A value of 1 indicates that the candy contains chocolate, while 0
indicates that it does not.

**Exploratory analysis** Q8. Plot a histogram of winpercent values

``` r
hist(candy$winpercent)
```

![](class09.markdown_strict_files/figure-markdown_strict/unnamed-chunk-10-1.png)

``` r
library(ggplot2)
```

    Warning: package 'ggplot2' was built under R version 4.4.3

``` r
ggplot(candy, aes(x = winpercent)) +
  geom_histogram(bins = 14, fill = "pink", color = "yellow") +
  labs(
    title = "Histogram of Candy Win Percent",
    x = "Win Percent",
    y = "Count"
  )
```

![](class09.markdown_strict_files/figure-markdown_strict/unnamed-chunk-11-1.png)

Warning in geom_histogram(bins = 14, fill = “pink”, colo = “yellow”):
Ignoring unknown parameters: `colo`

> Q9. Is the distribution of winpercent values symmetrical?

NO \>Q10. Is the center of the distribution above or below 50%?

``` r
mean(candy$winpercent)
```

    [1] 50.31676

``` r
median(candy$winpercent)
```

    [1] 47.82975

> Q11. On average is chocolate candy higher or lower ranked than fruit
> candy?

``` r
mean(candy$winpercent[candy$chocolate == 1])
```

    [1] 60.92153

``` r
mean(candy$winpercent[candy$fruity == 1])
```

    [1] 44.11974

The average of chocolate is higher than candy \>Q12. Is this difference
statistically significant?

``` r
t.test(
  candy$winpercent[candy$chocolate == 1],
  candy$winpercent[candy$fruity == 1]
)
```


        Welch Two Sample t-test

    data:  candy$winpercent[candy$chocolate == 1] and candy$winpercent[candy$fruity == 1]
    t = 6.2582, df = 68.882, p-value = 2.871e-08
    alternative hypothesis: true difference in means is not equal to 0
    95 percent confidence interval:
     11.44563 22.15795
    sample estimates:
    mean of x mean of y 
     60.92153  44.11974 

p value is small \>Q13. What are the five least liked candy types in
this set?

``` r
head(candy[order(candy$winpercent), ], 5)
```

                       chocolate fruity caramel peanutyalmondy nougat
    Nik L Nip                  0      1       0              0      0
    Boston Baked Beans         0      0       0              1      0
    Chiclets                   0      1       0              0      0
    Super Bubble               0      1       0              0      0
    Jawbusters                 0      1       0              0      0
                       crispedricewafer hard bar pluribus sugarpercent pricepercent
    Nik L Nip                         0    0   0        1        0.197        0.976
    Boston Baked Beans                0    0   0        1        0.313        0.511
    Chiclets                          0    0   0        1        0.046        0.325
    Super Bubble                      0    0   0        0        0.162        0.116
    Jawbusters                        0    1   0        1        0.093        0.511
                       winpercent
    Nik L Nip            22.44534
    Boston Baked Beans   23.41782
    Chiclets             24.52499
    Super Bubble         27.30386
    Jawbusters           28.12744

> Q14. What are the top 5 all time favorite candy types out of this set?

``` r
head(candy[order(-candy$winpercent), ], 5)
```

                              chocolate fruity caramel peanutyalmondy nougat
    Reese's Peanut Butter cup         1      0       0              1      0
    Reese's Miniatures                1      0       0              1      0
    Twix                              1      0       1              0      0
    Kit Kat                           1      0       0              0      0
    Snickers                          1      0       1              1      1
                              crispedricewafer hard bar pluribus sugarpercent
    Reese's Peanut Butter cup                0    0   0        0        0.720
    Reese's Miniatures                       0    0   0        0        0.034
    Twix                                     1    0   1        0        0.546
    Kit Kat                                  1    0   1        0        0.313
    Snickers                                 0    0   1        0        0.546
                              pricepercent winpercent
    Reese's Peanut Butter cup        0.651   84.18029
    Reese's Miniatures               0.279   81.86626
    Twix                             0.906   81.64291
    Kit Kat                          0.511   76.76860
    Snickers                         0.651   76.67378

> Q15. Make a first barplot of candy ranking based on winpercent values.

``` r
library(ggplot2)
ggplot(candy) +
  aes(x = winpercent, y = rownames(candy)) +
  geom_col(fill = "lightblue") +
  labs(
x = "win %", 
   y = "Candy Type",
  title = "Candy by Win %"
) 
```

![](class09.markdown_strict_files/figure-markdown_strict/unnamed-chunk-19-1.png)

> Q16. This is quite ugly, use the reorder() function to get the bars
> sorted by winpercent?

``` r
library(ggplot2)
my_cols=rep("black", nrow(candy))
my_cols[as.logical(candy$chocolate)] = "lavender"
my_cols[as.logical(candy$bar)] = "brown"
my_cols[as.logical(candy$fruity)] = "pink"
ggplot(candy) +
  aes(x = winpercent, y = reorder(rownames(candy), winpercent)) +
  geom_col(fill=my_cols) +
  labs(
    x = "Win %",
    y = "Candy Type",
    title = "Candy Rankings by %"
)
```

![](class09.markdown_strict_files/figure-markdown_strict/unnamed-chunk-20-1.png)

> Q17. What is the worst ranked chocolate candy?

Sixlets \>Q18. What is the best ranked fruity candy?

Starburts \>Q19. Which candy type is the highest ranked in terms of
winpercent for the least money - i.e. offers the most bang for your
buck?

``` r
library(ggrepel)
my_cols=rep("black", nrow(candy))
my_cols[as.logical(candy$chocolate)] = "chocolate"
my_cols[as.logical(candy$bar)] = "purple"
my_cols[as.logical(candy$fruity)] = "red"
ggplot(candy, aes(x = winpercent, y =  pricepercent, label = rownames(candy))) +
  geom_point(col=my_cols) +
  geom_text_repel(col=my_cols, size=3.3, max.overlaps = 6)+
  labs(
    x = "Price Percentile",
   y = "Win %",
  title = "Candy Winpercent vs Pricepercent"
)
```

    Warning: ggrepel: 47 unlabeled data points (too many overlaps). Consider
    increasing max.overlaps

![](class09.markdown_strict_files/figure-markdown_strict/unnamed-chunk-21-1.png)

Warning: ggrepel: 65 unlabeled data points (too many overlaps). Consider
increasing max.overlaps

> Q20. What are the top 5 most expensive candy types in the dataset and
> of these which is the least popular?

``` r
 ord <- order(candy$pricepercent, decreasing = TRUE)
head( candy[ord,c(11,12)], n=5 )
```

                             pricepercent winpercent
    Nik L Nip                       0.976   22.44534
    Nestle Smarties                 0.976   37.88719
    Ring pop                        0.965   35.29076
    Hershey's Krackel               0.918   62.28448
    Hershey's Milk Chocolate        0.918   56.49050

Out of all of them Nik L lip is the least popular

``` r
 top5_expensive <- head( candy[ord,c(11,12)], n=5 )
rownames(top5_expensive)[which.min(top5_expensive$winpercent)]
```

    [1] "Nik L Nip"

``` r
library(corrplot)
```

    corrplot 0.95 loaded

``` r
 cij <- cor(candy)
corrplot(cij)
```

![](class09.markdown_strict_files/figure-markdown_strict/unnamed-chunk-25-1.png)

> Q22. Examining this plot what two variables are anti-correlated
> (i.e. have minus values)?

Chocolate and fruity are anti-correlated

> Q23. Similarly, what two variables are most positively correlated?

Chocolate and winpercent are more positively correlated

> Q24. Complete the code to generate the loadings plot above. What
> original vari- ables are picked up strongly by PC1 in the positive
> direction? Do these make sense to you? Where did you see this
> relationship highlighted previously?

``` r
pca <- prcomp(candy, scale = TRUE)
summary(pca)
```

    Importance of components:
                              PC1    PC2    PC3     PC4    PC5     PC6     PC7
    Standard deviation     2.0788 1.1378 1.1092 1.07533 0.9518 0.81923 0.81530
    Proportion of Variance 0.3601 0.1079 0.1025 0.09636 0.0755 0.05593 0.05539
    Cumulative Proportion  0.3601 0.4680 0.5705 0.66688 0.7424 0.79830 0.85369
                               PC8     PC9    PC10    PC11    PC12
    Standard deviation     0.74530 0.67824 0.62349 0.43974 0.39760
    Proportion of Variance 0.04629 0.03833 0.03239 0.01611 0.01317
    Cumulative Proportion  0.89998 0.93832 0.97071 0.98683 1.00000

``` r
plot(pca$x[,1:2], col=my_cols, pch=16)
```

![](class09.markdown_strict_files/figure-markdown_strict/unnamed-chunk-27-1.png)

``` r
my_data <- cbind(candy, pca$x[,1:3])
```

``` r
p <- ggplot(my_data) +
        aes(x= PC1, y= PC2,
    size=winpercent/100,
    text=rownames(my_data),
    label=rownames(my_data)) +
geom_point(col= my_cols) 
p
```

![](class09.markdown_strict_files/figure-markdown_strict/unnamed-chunk-29-1.png)

``` r
library(ggrepel)

p +
  geom_text_repel(size = 3.3, col = my_cols, max.overlaps = 7) +
  theme(legend.position = "none") +
  labs(
    title = "Halloween Candy PCA Space",
    subtitle = "Colored by type: chocolate bar (dark brown), chocolate other (light brown)",
    caption = "Data from 538"
  )
```

    Warning: ggrepel: 39 unlabeled data points (too many overlaps). Consider
    increasing max.overlaps

![](class09.markdown_strict_files/figure-markdown_strict/unnamed-chunk-30-1.png)

#library(plotly) #ggplotly(p)

``` r
 ggplot(pca$rotation, aes(x = PC1, y = reorder(rownames(pca$rotation), PC1))) +
  geom_col()
```

![](class09.markdown_strict_files/figure-markdown_strict/unnamed-chunk-31-1.png)

Q25. Based on your exploratory analysis, correlation findings, and PCA
results, what combination of characteristics appears to make a “winning”
candy? How do these different analyses (visualization, correlation, PCA)
support or complement each other in reaching this conclusion?

A “winning” candy is usually **chocolate**, **not fruity**, and often a
**bar**. The plots show that chocolate candies have higher winpercent
values. The correlation analysis supports this by showing chocolate is
positively related to winpercent, while fruity candies are negatively
related. The PCA results group high-winning candies with chocolate and
bar features. Together, all analyses point to chocolate bar candies as
the most popular.
