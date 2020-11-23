
<!-- README.md is generated from README.Rmd. Please edit that file -->

# drawsample

<!-- badges: start -->

<!-- badges: end -->

The goal of drawsample is to …

## Installation

You can install the released version of drawsample from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("drawsample")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("atalay-k/drawsample",dependencies = TRUE,upgrade = "never")
```

## Example

Same basic examples for the usage:

``` r
library(drawsample)
## basic example code
# Draw a sample based on Score_1(from negatively skewed to normal)
 draw_sample(dist=example_data[,c(1,2)],n=200,skew = 0,kurts = 0,
 output_name = c("sample", "1"))
```

<img src="man/figures/README-example-1.png" width="100%" />

    #> $desc
    #>               n  mean   sd min max  skew kurtosis
    #> population 5000 14.61 4.90   0  25 -0.40    -0.35
    #> reference   200 13.93 3.48   0  25 -0.01    -0.05
    #> sample      200 14.37 3.61   1  25  0.00    -0.29
    #> 
    #> $sample
    #> # A tibble: 200 x 2
    #>       id     x
    #>    <dbl> <dbl>
    #>  1   541     1
    #>  2  2936     9
    #>  3  4071     9
    #>  4  2222     9
    #>  5    59     9
    #>  6  2344     9
    #>  7  3874     9
    #>  8  1439    10
    #>  9    66     9
    #> 10  4368     9
    #> # ... with 190 more rows
    #> 
    #> $graph

<img src="man/figures/README-example-2.png" width="100%" />

``` r
# Draw a sample based on Score_2 (from negatively skewed to positively skewed)
 draw_sample(dist=example_data[,c(1,3)],n=200,skew = 1,kurts = 1,
 output_name = c("sample", "2"))
```

<img src="man/figures/README-example-3.png" width="100%" />

    #> $desc
    #>               n  mean   sd min max  skew kurtosis
    #> population 5000 12.78 5.22   0  25 -0.17    -0.88
    #> reference   200  6.10 4.44   0  25  0.99     0.96
    #> sample      200  6.70 4.45   0  25  0.92     0.74
    #> 
    #> $sample
    #> # A tibble: 200 x 2
    #>       id     x
    #>    <dbl> <dbl>
    #>  1  4807     2
    #>  2  2538     0
    #>  3  4149     2
    #>  4  1483     1
    #>  5  2984     0
    #>  6   930     0
    #>  7  4792     1
    #>  8   346     2
    #>  9  4620     1
    #> 10  4929     2
    #> # ... with 190 more rows
    #> 
    #> $graph

<img src="man/figures/README-example-4.png" width="100%" />

``` r
# Draw a sample based on Score_2 (from negatively skewed to positively skewed with replacement)
 draw_sample(dist=example_data[,c(1,3)],n=200,skew = 0.5,kurts = 0.4,
 replacement=TRUE,output_name = c("sample", "3"))
```

<img src="man/figures/README-example-5.png" width="100%" />

    #> $desc
    #>               n  mean   sd min max  skew kurtosis
    #> population 5000 12.78 5.22   0  25 -0.17    -0.88
    #> reference   200 10.85 3.88   0  25  0.47     0.38
    #> sample      200 11.38 3.88   1  25  0.44     0.28
    #> 
    #> $sample
    #> # A tibble: 200 x 2
    #>       id     x
    #>    <dbl> <dbl>
    #>  1  4792     1
    #>  2  3147     3
    #>  3  1877     4
    #>  4  4018     3
    #>  5    60     6
    #>  6  3039     5
    #>  7  2288     6
    #>  8  1710     5
    #>  9   678     6
    #> 10   157     5
    #> # ... with 190 more rows
    #> 
    #> $graph

<img src="man/figures/README-example-6.png" width="100%" />
