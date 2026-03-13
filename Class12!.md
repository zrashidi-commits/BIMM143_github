# Class12
Zahra Rashidi (A18561538)

``` r
url <- "https://bioboot.github.io/bggn213_W19/class-material/rs8067378_ENSG00000172057.6.txt"
dat <- read.table(url, header = TRUE, stringsAsFactors = TRUE)
```

``` r
head(dat)
```

       sample geno      exp
    1 HG00367  A/G 28.96038
    2 NA20768  A/G 20.24449
    3 HG00361  A/A 31.32628
    4 HG00135  A/A 34.11169
    5 NA18870  G/G 18.25141
    6 NA11993  A/A 32.89721

``` r
str(dat)
```

    'data.frame':   462 obs. of  3 variables:
     $ sample: Factor w/ 462 levels "HG00096","HG00097",..: 170 424 165 37 299 219 96 285 138 17 ...
     $ geno  : Factor w/ 3 levels "A/A","A/G","G/G": 2 2 1 1 3 1 2 1 3 2 ...
     $ exp   : num  29 20.2 31.3 34.1 18.3 ...

``` r
table(dat$geno)
```


    A/A A/G G/G 
    108 233 121 

> Q13.

The three genotypes show clear differences in ORMDL3 expression. The
median expression for individuals with the A/A genotype is 31.24847,
which is the highest among the groups. The A/G heterozygotes show an
intermediate median expression of 25.06486, while the G/G genotype has
the lowest median expression at 20.07363. This pattern suggests a
stepwise decrease in ORMDL3 expression as the number of G alleles
increases.

``` r
tapply(dat$exp, dat$geno, median)
```

         A/A      A/G      G/G 
    31.24847 25.06486 20.07363 

``` r
boxplot(exp ~ geno, data = dat,
        xlab = "Genotype at rs8067378",
        ylab = "ORMDL3 Expression",
        main = "ORMDL3 Expression by Genotype",
        col = c("lightblue", "lightgreen", "lightpink"))
```

![](Class12!.markdown_strict_files/figure-markdown_strict/unnamed-chunk-5-1.png)

> Q14.

The boxplot comparing A/A, A/G, and G/G genotypes shows a clear downward
trend in ORMDL3 expression from A/A to G/G. Because individuals with the
G/G genotype express ORMDL3 at substantially lower levels than those
with A/A, the data indicate that rs8067378 does influence ORMDL3
expression. In other words, this SNP acts as an eQTL, with the G allele
associated with reduced expression of the gene.
