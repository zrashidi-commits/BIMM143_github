# Homework_06

``` r
library(bio3d)
```

``` r
get.pdb("4AKE", path=".", overwrite=TRUE)
```

    [1] "./4AKE.pdb"

``` r
get.pdb("1AKE", path=".", overwrite=TRUE)
```

    [1] "./1AKE.pdb"

``` r
get.pdb("1E4Y", path=".", overwrite=TRUE)
```

    [1] "./1E4Y.pdb"

``` r
library(bio3d)

# read pdbs (local files)
s1 <- read.pdb("4AKE.pdb")
s2 <- read.pdb("1AKE.pdb")
```

       PDB has ALT records, taking A only, rm.alt=TRUE

``` r
s3 <- read.pdb("1E4Y.pdb")

# trim chain A, CA atoms only
s1.chainA <- trim.pdb(s1, chain="A", elety="CA")
s2.chainA <- trim.pdb(s2, chain="A", elety="CA")
s3.chainA <- trim.pdb(s3, chain="A", elety="CA")

# extract B-factors
s1.b <- s1.chainA$atom$b
s2.b <- s2.chainA$atom$b
s3.b <- s3.chainA$atom$b

# plot B-factors
plotb3(s1.b, sse=s1.chainA, typ="l", ylab="Bfactor")
```

![](homework_06.markdown_strict_files/figure-markdown_strict/unnamed-chunk-3-1.png)

``` r
plotb3(s2.b, sse=s2.chainA, typ="l", ylab="Bfactor")
```

![](homework_06.markdown_strict_files/figure-markdown_strict/unnamed-chunk-3-2.png)

``` r
plotb3(s3.b, sse=s3.chainA, typ="l", ylab="Bfactor")
```

![](homework_06.markdown_strict_files/figure-markdown_strict/unnamed-chunk-3-3.png)

``` r
# dendrogram (similarity)
hc <- hclust(dist(rbind(s1.b, s2.b, s3.b)))
plot(hc)
```

![](homework_06.markdown_strict_files/figure-markdown_strict/unnamed-chunk-3-4.png)

``` r
analyze_bfactor <- function(pdb_file) {
  pdb <- read.pdb(pdb_file)
  chainA <- trim.pdb(pdb, chain="A", elety="CA")
  b <- chainA$atom$b
  plotb3(b, sse=chainA, typ="l", ylab="Bfactor")
  return(b)
}
```

> Q1. What type of object is returned from the read.pdb() function?

A PDB object (a list) that contains the structure info (atoms, residues,
coordinates, etc.)

> Q2. What does the trim.pdb() function do?

It keeps only the parts you ask for (here: Chain A and CA atoms only),
and removes the rest.

> Q3. What input parameter would turn off the marginal black and grey
> rectangles in the plots and what do they represent in this case?

-   To turn them off: remove the sse= argument in plotb3() (don’t pass
    sse=…).

-   The black/grey rectangles represent secondary structure elements
    (like helices/sheets) along the protein sequence

> Q4. What would be a better plot to compare across the different
> proteins?

Put them on one shared plot (same axes) so it can be compare directly,
e.g. one plot with 3 lines (s1, s2, s3)

> Q5. Which proteins are more similar to each other in their B-factor
> trends. How could you quantify this?

From the dendrogram, 1AKE and 1E4Y are the most similar, while 4AKE is
more different.This can be quantified using the distance matrix (smaller
distance = more similar) or by correlation of the B-factor vectors.

> Q6. How would you generalize the original code above to work with any
> set of input protein structures?

The analysis can be generalized by writing a function that takes a PDB
file as input, trims chain A to Cα atoms, extracts B-factors, and plots
them. This avoids repeating code and allows the same analysis to be
applied easily to any protein structure.
