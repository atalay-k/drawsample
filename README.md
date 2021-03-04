
<!-- README.md is generated from README.Rmd. Please edit that file -->

# drawsample

<!-- badges: start -->
<!-- badges: end -->

The goal of drawsample is to sample data with the desired
properties.Samples can be drawn by purposive sampling with determining
distributional conditions, such as deviation from normality (skewness
and kurtosis), and sample size in quantitative research studies.

## Installation

You can install the released version of drawsample from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("drawsample")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("atalay-k/drawsample")
```

## Example

### Examples for *`draw_sample()`*

#### Draw a sample based on Score\_1 (from negatively skewed to normal)

``` r
library(drawsample)
draw_sample(dist=example_data[,c(1,2)],n=200,skew = 0,kurts = 0,
output_name = c("sample", "1"))
```

<img src="man/figures/README-example 1-1.png" width="100%" />

    #> $desc
    #>               n  mean   sd min max  skew kurtosis
    #> population 5000 14.61 4.90   0  25 -0.40    -0.35
    #> reference   200 11.86 4.29   0  25  0.03    -0.03
    #> sample      200 12.35 4.26   1  25  0.06    -0.20
    #> 
    #> $sample
    #> # A tibble: 200 x 2
    #>       id     x
    #>    <dbl> <dbl>
    #>  1   930     2
    #>  2   773     2
    #>  3  1610     1
    #>  4   615     4
    #>  5  4018     4
    #>  6  1569     4
    #>  7  3397     5
    #>  8  4372     6
    #>  9  3572     5
    #> 10  3724     5
    #> # ... with 190 more rows
    #> 
    #> $graph

<img src="man/figures/README-example 1-2.png" width="100%" />

#### Draw a sample based on Score\_2 (from negatively skewed to positively skewed

``` r
draw_sample(dist=example_data[,c(1,3)],n=200,skew = 1,kurts = 1,
output_name = c("sample", "2"))
```

<img src="man/figures/README-example 2-1.png" width="100%" />

    #> $desc
    #>               n  mean   sd min max  skew kurtosis
    #> population 5000 12.78 5.22   0  25 -0.17    -0.88
    #> reference   200  5.20 4.42   0  25  0.97     0.98
    #> sample      200  5.72 4.39   0  25  0.96     0.96
    #> 
    #> $sample
    #> # A tibble: 200 x 2
    #>       id     x
    #>    <dbl> <dbl>
    #>  1  1524     1
    #>  2   218     2
    #>  3  4239     2
    #>  4  3756     1
    #>  5  4280     2
    #>  6  1395     1
    #>  7  1002     2
    #>  8  2383     2
    #>  9  1358     1
    #> 10  3593     1
    #> # ... with 190 more rows
    #> 
    #> $graph

<img src="man/figures/README-example 2-2.png" width="100%" />

#### Draw a sample based on Score\_2 (from negatively skewed to positively skewed with replacement

``` r
draw_sample(dist=example_data[,c(1,3)],n=200,skew = 0.5,kurts = 0.4,
replacement=TRUE,output_name = c("sample", "3"))
```

<img src="man/figures/README-example 3-1.png" width="100%" />

    #> $desc
    #>               n  mean   sd min max  skew kurtosis
    #> population 5000 12.78 5.22   0  25 -0.17    -0.88
    #> reference   200  5.78 4.24   0  25  0.46     0.39
    #> sample      200  6.26 4.12   0  25  0.43     0.42
    #> 
    #> $sample
    #> # A tibble: 200 x 2
    #>       id     x
    #>    <dbl> <dbl>
    #>  1  2330     1
    #>  2  3169     2
    #>  3  1395     1
    #>  4  4239     2
    #>  5   264     2
    #>  6  2330     1
    #>  7  3242     2
    #>  8  2984     0
    #>  9  4958     1
    #> 10  4433     1
    #> # ... with 190 more rows
    #> 
    #> $graph

<img src="man/figures/README-example 3-2.png" width="100%" />

### examples for *`draw_sample_n()`*

#### Draw a sample based on Score\_1 with default values

``` r
draw_sample_n(dist=example_data[,c(1,2)],n=200,skew = 0,kurts = 0, location=0, delta_var=0,output_name = c("sample", "4"))
```

<img src="man/figures/README-example 4-1.png" width="100%" />

    #> $desc
    #>               n  mean   sd min max  skew kurtosis
    #> population 5000 14.61 4.90   0  25 -0.40    -0.35
    #> reference   200 13.17 5.11   0  25 -0.14    -0.25
    #> sample      200 13.56 5.06   1  25 -0.13    -0.23
    #> 
    #> $sample
    #> # A tibble: 200 x 2
    #>       id     x
    #>    <dbl> <dbl>
    #>  1  4371     1
    #>  2   416     2
    #>  3  4792     2
    #>  4   954     2
    #>  5  2550     2
    #>  6  3007     3
    #>  7  2335     3
    #>  8  4018     4
    #>  9  2844     6
    #> 10  4080     5
    #> # ... with 190 more rows
    #> 
    #> $graph

<img src="man/figures/README-example 4-2.png" width="100%" />

#### Draw a sample based on Score\_2 (location par)

``` r
draw_sample_n(dist=example_data[,c(1,3)],n=200,skew = 1,kurts = 1,location=-0.5,delta_var=0,output_name = c("sample", "5"))
```

<img src="man/figures/README-example 5-1.png" width="100%" />

    #> $desc
    #>               n  mean   sd min max  skew kurtosis
    #> population 5000 12.78 5.22   0  25 -0.17    -0.88
    #> reference   200 12.42 4.12   0  25 -0.02     0.16
    #> sample      200 12.91 4.18   1  25  0.09     0.08
    #> 
    #> $sample
    #> # A tibble: 200 x 2
    #>       id     x
    #>    <dbl> <dbl>
    #>  1    48     1
    #>  2    38     2
    #>  3  1537     4
    #>  4  2791     3
    #>  5  1936     6
    #>  6  1965     6
    #>  7  3199     6
    #>  8  2055     6
    #>  9  2452     6
    #> 10  1509     5
    #> # ... with 190 more rows
    #> 
    #> $graph

<img src="man/figures/README-example 5-2.png" width="100%" />

#### Draw a sample based on Score\_2 (delta\_var par)

``` r
# Draw a sample based on Score_2 (delta_var par)
draw_sample_n(dist=example_data[,c(1,3)],n=200,skew = 0.5,kurts = 0.4,location=0,delta_var=0.3,output_name = c("sample", "6"))
```

<img src="man/figures/README-example 6-1.png" width="100%" />

    #> $desc
    #>               n  mean   sd min max  skew kurtosis
    #> population 5000 12.78 5.22   0  25 -0.17    -0.88
    #> reference   200 10.91 3.72   0  25  0.21     0.97
    #> sample      200 11.41 3.77   1  25  0.23     0.76
    #> 
    #> $sample
    #> # A tibble: 200 x 2
    #>       id     x
    #>    <dbl> <dbl>
    #>  1  4912     1
    #>  2  1238     4
    #>  3   889     3
    #>  4  2500     4
    #>  5  2230     3
    #>  6  4898     3
    #>  7  3799     4
    #>  8  2884     4
    #>  9  4041     6
    #> 10  4047     5
    #> # ... with 190 more rows
    #> 
    #> $graph

<img src="man/figures/README-example 6-2.png" width="100%" />
