# class18
Zahra Rashidi (A18561538)

## Background

Pertussis (whooping cough) is a common lung infection cause by the
bacteria B. Pertussis. It can affect anyone but it is most deadly fro
infants (under 1 year of age)

## CDC trackind data

The CDC tracks the number of Pertussis cases:

> Q. I want a plot of year vs cases

``` r
library(ggplot2)
ggplot(cdc)+ aes(year,cases) + geom_point() + geom_line()
```

![](Class18.markdown_strict_files/figure-markdown_strict/unnamed-chunk-2-1.png)

> Q. Add annotation lines for the major miletstone of wP vaccination
> roll-out (1946) and the switch to the aP vaccine (1996).

``` r
ggplot(cdc) +
  aes(year, cases) +
  geom_point() +
  geom_line() + 
  geom_vline(xintercept = 1946, col= "blue", lty=2) +
  geom_vline(xintercept = 1996, col= "red", lty= 2) + 
  geom_vline(xintercept =2020, col="gray", lty=2)
```

![](Class18.markdown_strict_files/figure-markdown_strict/unnamed-chunk-3-1.png)

Cases decreased dramatically after 1946, largely due to the introduction
and widespread use of vaccines. However, cases began to rise again in
the early 2000s. One possible reason is that the newer vaccine may not
provide protection that lasts as long or is as strong as the older
version. Over time, the bacteria may also have evolved, making it better
able to survive despite vaccination. In addition, improvements in
diagnostic testing and disease surveillance have made it easier to
detect and report cases that may have previously gone unnoticed. Another
contributing factor could be declining vaccination rates in some
populations. The data also shows a correlation between peaks in cases
and countries that use this particular vaccine, although the newer
vaccine tends to show smaller peaks compared to earlier outbreaks.

## Exploring CMI-PD Poject

The CMI-PD project \< https://www.cmi-pb.org \> mission is to provide
the scientific community with a comprehensive, high-quality and freely
accessible resource of Pertussis booster vaccination.

Basically, make available a large dataset on the immune response to
Pertussis. They used a “booster” vaccination as a proxy for Pertussis
infection

They make their data available as JSON format API. We can read this into
R the `read_json()` function from the **jsonlite** package:

``` r
library(jsonlite)
subject <- read_json("https://www.cmi-pb.org/api/v5_1/subject", simplifyVector=TRUE)
head(subject)
```

      subject_id infancy_vac biological_sex              ethnicity  race
    1          1          wP         Female Not Hispanic or Latino White
    2          2          wP         Female Not Hispanic or Latino White
    3          3          wP         Female                Unknown White
    4          4          wP           Male Not Hispanic or Latino Asian
    5          5          wP           Male Not Hispanic or Latino Asian
    6          6          wP         Female Not Hispanic or Latino White
      year_of_birth date_of_boost      dataset
    1    1986-01-01    2016-09-12 2020_dataset
    2    1968-01-01    2019-01-28 2020_dataset
    3    1983-01-01    2016-10-10 2020_dataset
    4    1988-01-01    2016-08-29 2020_dataset
    5    1991-01-01    2016-08-29 2020_dataset
    6    1988-01-01    2016-10-10 2020_dataset

> Q. How many aP and wP individuals are there in this `subject` table?

``` r
table(subject$infancy_vac)
```


    aP wP 
    87 85 

> Q. How many male/female are there?

``` r
table(subject$biological_sex)
```


    Female   Male 
       112     60 

> What is the breakdown of `biological_sex` race subjects?

> Is this representtaive of the US population?

``` r
table(subject$race, subject$biological_sex)
```

                                               
                                                Female Male
      American Indian/Alaska Native                  0    1
      Asian                                         32   12
      Black or African American                      2    3
      More Than One Race                            15    4
      Native Hawaiian or Other Pacific Islander      1    1
      Unknown or Not Reported                       14    7
      White                                         48   32

We can now read more tales form the CMI-PB database

``` r
specimen <- read_json("https://www.cmi-pb.org/api/v5_1/specimen", simplifyVector=TRUE)
ab_titer <- read_json("https://www.cmi-pb.org/api/v5_1/plasma_ab_titer", simplifyVector=TRUE)
```

``` r
head(specimen)
```

      specimen_id subject_id actual_day_relative_to_boost
    1           1          1                           -3
    2           2          1                            1
    3           3          1                            3
    4           4          1                            7
    5           5          1                           11
    6           6          1                           32
      planned_day_relative_to_boost specimen_type visit
    1                             0         Blood     1
    2                             1         Blood     2
    3                             3         Blood     3
    4                             7         Blood     4
    5                            14         Blood     5
    6                            30         Blood     6

``` r
head(ab_titer)
```

      specimen_id isotype is_antigen_specific antigen        MFI MFI_normalised
    1           1     IgE               FALSE   Total 1110.21154       2.493425
    2           1     IgE               FALSE   Total 2708.91616       2.493425
    3           1     IgG                TRUE      PT   68.56614       3.736992
    4           1     IgG                TRUE     PRN  332.12718       2.602350
    5           1     IgG                TRUE     FHA 1887.12263      34.050956
    6           1     IgE                TRUE     ACT    0.10000       1.000000
       unit lower_limit_of_detection
    1 UG/ML                 2.096133
    2 IU/ML                29.170000
    3 IU/ML                 0.530000
    4 IU/ML                 6.205949
    5 IU/ML                 4.679535
    6 IU/ML                 2.816431

To make sense of all this data we need to “join” (a.k.a “merge” or
“link”) all these tables together. Only then will you know that a give
Ab measurement (from the `ab-titer` table) was collected on a certain
data from a certain date (from the `specimen` table) wP or aP subject
(from the `subject` table)

We can use **dplyr** and the `*join()` fmaily of functions to do this.

``` r
library(dplyr)
```


    Attaching package: 'dplyr'

    The following objects are masked from 'package:stats':

        filter, lag

    The following objects are masked from 'package:base':

        intersect, setdiff, setequal, union

``` r
meta <- inner_join(subject, specimen)
```

    Joining with `by = join_by(subject_id)`

``` r
head(meta)
```

      subject_id infancy_vac biological_sex              ethnicity  race
    1          1          wP         Female Not Hispanic or Latino White
    2          1          wP         Female Not Hispanic or Latino White
    3          1          wP         Female Not Hispanic or Latino White
    4          1          wP         Female Not Hispanic or Latino White
    5          1          wP         Female Not Hispanic or Latino White
    6          1          wP         Female Not Hispanic or Latino White
      year_of_birth date_of_boost      dataset specimen_id
    1    1986-01-01    2016-09-12 2020_dataset           1
    2    1986-01-01    2016-09-12 2020_dataset           2
    3    1986-01-01    2016-09-12 2020_dataset           3
    4    1986-01-01    2016-09-12 2020_dataset           4
    5    1986-01-01    2016-09-12 2020_dataset           5
    6    1986-01-01    2016-09-12 2020_dataset           6
      actual_day_relative_to_boost planned_day_relative_to_boost specimen_type
    1                           -3                             0         Blood
    2                            1                             1         Blood
    3                            3                             3         Blood
    4                            7                             7         Blood
    5                           11                            14         Blood
    6                           32                            30         Blood
      visit
    1     1
    2     2
    3     3
    4     4
    5     5
    6     6

Let’s do one more `inner_join` to join the `ab_titer` with all our
`meta` data.

``` r
abdata <- inner_join(ab_titer, meta)
```

    Joining with `by = join_by(specimen_id)`

``` r
head (abdata)
```

      specimen_id isotype is_antigen_specific antigen        MFI MFI_normalised
    1           1     IgE               FALSE   Total 1110.21154       2.493425
    2           1     IgE               FALSE   Total 2708.91616       2.493425
    3           1     IgG                TRUE      PT   68.56614       3.736992
    4           1     IgG                TRUE     PRN  332.12718       2.602350
    5           1     IgG                TRUE     FHA 1887.12263      34.050956
    6           1     IgE                TRUE     ACT    0.10000       1.000000
       unit lower_limit_of_detection subject_id infancy_vac biological_sex
    1 UG/ML                 2.096133          1          wP         Female
    2 IU/ML                29.170000          1          wP         Female
    3 IU/ML                 0.530000          1          wP         Female
    4 IU/ML                 6.205949          1          wP         Female
    5 IU/ML                 4.679535          1          wP         Female
    6 IU/ML                 2.816431          1          wP         Female
                   ethnicity  race year_of_birth date_of_boost      dataset
    1 Not Hispanic or Latino White    1986-01-01    2016-09-12 2020_dataset
    2 Not Hispanic or Latino White    1986-01-01    2016-09-12 2020_dataset
    3 Not Hispanic or Latino White    1986-01-01    2016-09-12 2020_dataset
    4 Not Hispanic or Latino White    1986-01-01    2016-09-12 2020_dataset
    5 Not Hispanic or Latino White    1986-01-01    2016-09-12 2020_dataset
    6 Not Hispanic or Latino White    1986-01-01    2016-09-12 2020_dataset
      actual_day_relative_to_boost planned_day_relative_to_boost specimen_type
    1                           -3                             0         Blood
    2                           -3                             0         Blood
    3                           -3                             0         Blood
    4                           -3                             0         Blood
    5                           -3                             0         Blood
    6                           -3                             0         Blood
      visit
    1     1
    2     1
    3     1
    4     1
    5     1
    6     1

> Q. How many different Ab “isotype” values are in this dataset?

``` r
table(abdata$isotype)
```


      IgE   IgG  IgG1  IgG2  IgG3  IgG4 
     6698  7265 11993 12000 12000 12000 

> Q. How many different “antigen” values are measured?

``` r
table(abdata$antigen)
```


        ACT   BETV1      DT   FELD1     FHA  FIM2/3   LOLP1     LOS Measles     OVA 
       1970    1970    6318    1970    6712    6318    1970    1970    1970    6318 
        PD1     PRN      PT     PTM   Total      TT 
       1970    6712    6712    1970     788    6318 

Let’s focus on IgG isotype

``` r
igg <- abdata |> filter(isotype=="IgG")
head(igg)
```

      specimen_id isotype is_antigen_specific antigen        MFI MFI_normalised
    1           1     IgG                TRUE      PT   68.56614       3.736992
    2           1     IgG                TRUE     PRN  332.12718       2.602350
    3           1     IgG                TRUE     FHA 1887.12263      34.050956
    4          19     IgG                TRUE      PT   20.11607       1.096366
    5          19     IgG                TRUE     PRN  976.67419       7.652635
    6          19     IgG                TRUE     FHA   60.76626       1.096457
       unit lower_limit_of_detection subject_id infancy_vac biological_sex
    1 IU/ML                 0.530000          1          wP         Female
    2 IU/ML                 6.205949          1          wP         Female
    3 IU/ML                 4.679535          1          wP         Female
    4 IU/ML                 0.530000          3          wP         Female
    5 IU/ML                 6.205949          3          wP         Female
    6 IU/ML                 4.679535          3          wP         Female
                   ethnicity  race year_of_birth date_of_boost      dataset
    1 Not Hispanic or Latino White    1986-01-01    2016-09-12 2020_dataset
    2 Not Hispanic or Latino White    1986-01-01    2016-09-12 2020_dataset
    3 Not Hispanic or Latino White    1986-01-01    2016-09-12 2020_dataset
    4                Unknown White    1983-01-01    2016-10-10 2020_dataset
    5                Unknown White    1983-01-01    2016-10-10 2020_dataset
    6                Unknown White    1983-01-01    2016-10-10 2020_dataset
      actual_day_relative_to_boost planned_day_relative_to_boost specimen_type
    1                           -3                             0         Blood
    2                           -3                             0         Blood
    3                           -3                             0         Blood
    4                           -3                             0         Blood
    5                           -3                             0         Blood
    6                           -3                             0         Blood
      visit
    1     1
    2     1
    3     1
    4     1
    5     1
    6     1

Make a plot `MFI_normalised` values for all `antigen` values

ˆ

``` r
ggplot(igg)+ aes(MFI_normalised, antigen) + geom_boxplot()
```

![](Class18.markdown_strict_files/figure-markdown_strict/unnamed-chunk-16-1.png)

The antigens “PT”, “FIM2/3”, and “FHA” appear to have the wide range of
values.

> Q. Is there a difference for these repsonses between aP and wP
> individuals?

``` r
ggplot(igg)+ aes(MFI_normalised, antigen, col=infancy_vac) + geom_boxplot() 
```

![](Class18.markdown_strict_files/figure-markdown_strict/unnamed-chunk-17-1.png)

``` r
ggplot(igg)+ aes(MFI_normalised, antigen) + geom_boxplot() + facet_wrap(~infancy_vac)
```

![](Class18.markdown_strict_files/figure-markdown_strict/unnamed-chunk-18-1.png)

> Q. Is there a difference with time (i.e. before booster shot vs after
> booster shot)?

``` r
ggplot(igg)+ aes(MFI_normalised, antigen, col=infancy_vac) + geom_boxplot() + facet_wrap(~visit)
```

![](Class18.markdown_strict_files/figure-markdown_strict/unnamed-chunk-19-1.png)

``` r
# Filter 2021 dataset 
abdata.21 <- abdata %>% filter(dataset == "2021_dataset")

abdata.21 %>% 
  filter(isotype == "IgG", antigen == "PT") %>%
  ggplot(aes(x = planned_day_relative_to_boost,
             y = MFI_normalised,
             col = infancy_vac,
             group = subject_id)) +
  
    # Individual subject trajectories (thin, faint)
    geom_line(alpha = 0.25, linewidth = 0.6) +
    geom_point(alpha = 0.4, size = 1.5) +
    
    # Smooth bold average trendlines for aP and wP
    stat_summary(fun = mean,
                 geom = "smooth",
                 se = FALSE,
                 linewidth = 2.8,
                 span = 0.5,
                 aes(group = infancy_vac, col = infancy_vac)) +
    
    # Reference lines
    geom_vline(xintercept = 0, linetype = "dashed") +
    geom_vline(xintercept = 14, linetype = "dashed") +
    
    labs(title = "CMI-PB 2021 dataset IgG PT",
         subtitle = "Dashed lines at day 0 (pre boost) and day 14 (post boost)",
         x = "Day relative to boost",
         y = "Normalised MFI") +
    
    theme_minimal(base_size = 14)
```

    Warning in stat_summary(fun = mean, geom = "smooth", se = FALSE, linewidth =
    2.8, : Ignoring unknown parameters: `span`

![](Class18.markdown_strict_files/figure-markdown_strict/unnamed-chunk-20-1.png)
