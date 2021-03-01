
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

examples for ‘draw\_sample()’

``` r
library(drawsample)
#'# Draw a sample based on Score_1(from negatively skewed to normal)
draw_sample(dist=example_data[,c(1,2)],n=200,skew = 0,kurts = 0,
output_name = c("sample", "1"))
```

<img src="man/figures/README-example 1-1.png" width="100%" />

    #> $desc
    #>               n  mean   sd min max  skew kurtosis
    #> population 5000 14.61 4.90   0  25 -0.40    -0.35
    #> reference   200 13.24 4.40   0  25 -0.03     0.03
    #> sample      200 13.81 4.46   2  25 -0.04    -0.26
    #> 
    #> $sample
    #> # A tibble: 200 x 2
    #>       id     x
    #>    <dbl> <dbl>
    #>  1  1197     2
    #>  2  4807     2
    #>  3  4081     6
    #>  4   587     5
    #>  5  1849     5
    #>  6  2884     5
    #>  7  2087     6
    #>  8  4912     6
    #>  9   908     6
    #> 10  3458     7
    #> # ... with 190 more rows
    #> 
    #> $graph

<img src="man/figures/README-example 1-2.png" width="100%" />

``` r
# Draw a sample based on Score_2 (from negatively skewed to positively skewed)
draw_sample(dist=example_data[,c(1,3)],n=200,skew = 1,kurts = 1,
output_name = c("sample", "2"))
```

<img src="man/figures/README-example 1-3.png" width="100%" />

    #> $desc
    #>               n  mean   sd min max  skew kurtosis
    #> population 5000 12.78 5.22   0  25 -0.17    -0.88
    #> reference   200  5.52 4.51   0  25  1.05     0.98
    #> sample      200  6.00 4.55   0  25  0.97     0.73
    #> 
    #> $sample
    #> # A tibble: 200 x 2
    #>       id     x
    #>    <dbl> <dbl>
    #>  1  4583     0
    #>  2  4280     2
    #>  3  4570     1
    #>  4  1856     1
    #>  5   346     2
    #>  6   390     1
    #>  7   930     0
    #>  8  4324     2
    #>  9  1395     1
    #> 10  1119     2
    #> # ... with 190 more rows
    #> 
    #> $graph

<img src="man/figures/README-example 1-4.png" width="100%" />

``` r
# Draw a sample based on Score_2 (from negatively skewed to positively skewed
# with replacement)
draw_sample(dist=example_data[,c(1,3)],n=200,skew = 0.5,kurts = 0.4,
replacement=TRUE,output_name = c("sample", "3"))
```

<img src="man/figures/README-example 1-5.png" width="100%" />

    #> $desc
    #>               n  mean   sd min max  skew kurtosis
    #> population 5000 12.78 5.22   0  25 -0.17    -0.88
    #> reference   200 10.51 4.07   0  25  0.48     0.44
    #> sample      200 10.98 4.12   1  25  0.42     0.26
    #> 
    #> $sample
    #> # A tibble: 200 x 2
    #>       id     x
    #>    <dbl> <dbl>
    #>  1  4822     2
    #>  2  3007     1
    #>  3  1856     1
    #>  4   615     3
    #>  5  3946     4
    #>  6  3814     4
    #>  7  1547     4
    #>  8  3645     5
    #>  9  4595     6
    #> 10   907     5
    #> # ... with 190 more rows
    #> 
    #> $graph

<img src="man/figures/README-example 1-6.png" width="100%" />

examples for ‘draw\_sample\_n()’

``` r
# Draw a sample based on Score_1
draw_sample_n(dist=example_data[,c(1,2)],n=200,skew = 0,kurts = 0, location=0, delta_var=0,output_name = c("sample", "4"))
```

<img src="man/figures/README-example 2-1.png" width="100%" />

    #> $desc
    #>               n  mean   sd min max  skew kurtosis
    #> population 5000 14.61 4.90   0  25 -0.40    -0.35
    #> reference   200 15.09 3.58   0  25 -0.22     0.79
    #> sample      200 15.65 3.57   2  25 -0.18     0.39
    #> 
    #> $sample
    #> # A tibble: 200 x 2
    #>       id     x
    #>    <dbl> <dbl>
    #>  1  4807     2
    #>  2  2860     8
    #>  3  4594     7
    #>  4  2162     9
    #>  5   195    10
    #>  6  4926     9
    #>  7  1477     9
    #>  8  1638    10
    #>  9   218    10
    #> 10  4408     9
    #> # ... with 190 more rows
    #> 
    #> $graph

<img src="man/figures/README-example 2-2.png" width="100%" />

``` r
# Draw a sample based on Score_2 (location par)
draw_sample_n(dist=example_data[,c(1,3)],n=200,skew = 1,kurts = 1,location=-0.5,delta_var=0,output_name = c("sample", "5"))
```

<img src="man/figures/README-example 2-3.png" width="100%" />

    #> $desc
    #>               n  mean   sd min max  skew kurtosis
    #> population 5000 12.78 5.22   0  25 -0.17    -0.88
    #> reference   200 10.63 3.71   0  25  0.23     0.46
    #> sample      200 11.20 3.60   1  25  0.28     0.51
    #> 
    #> $sample
    #> # A tibble: 200 x 2
    #>       id     x
    #>    <dbl> <dbl>
    #>  1  2967     1
    #>  2  3472     3
    #>  3  2920     4
    #>  4  1944     4
    #>  5  2759     4
    #>  6    50     4
    #>  7  1979     6
    #>  8  1694     6
    #>  9   158     6
    #> 10   976     6
    #> # ... with 190 more rows
    #> 
    #> $graph

<img src="man/figures/README-example 2-4.png" width="100%" />

``` r
# Draw a sample based on Score_2 (delta_var par)
draw_sample_n(dist=example_data[,c(1,3)],n=200,skew = 0.5,kurts = 0.4,location=0,delta_var=0.3,output_name = c("sample", "6"))
```

<img src="man/figures/README-example 2-5.png" width="100%" />

    #> Warning in draw_sample_n(dist = example_data[, c(1, 3)], n = 200, skew = 0.5, : It is not eligible to extract data with the specified properties from this data
    #>       without replacement.The data set nearest to the values listed will be drawn.
    #> $desc
    #>               n  mean   sd min max  skew kurtosis
    #> population 5000 12.78 5.22   0  25 -0.17    -0.88
    #> reference   200 11.59 4.54   0  25  0.09     0.04
    #> sample      200 12.04 4.52   1  24  0.04    -0.24
    #> 
    #> $sample
    #> # A tibble: 200 x 2
    #>       id     x
    #>    <dbl> <dbl>
    #>  1   400     2
    #>  2   740     1
    #>  3  4270     2
    #>  4   475     3
    #>  5  2118     4
    #>  6   763     3
    #>  7  3078     4
    #>  8  4537     3
    #>  9   591     4
    #> 10  3212     6
    #> # ... with 190 more rows
    #> 
    #> $graph

<img src="man/figures/README-example 2-6.png" width="100%" />

You’ll still need to render `README.Rmd` regularly, to keep `README.md`
up-to-date. `devtools::build_readme()` is handy for this. You could also
use GitHub Actions to re-render `README.Rmd` every time you push. An
example workflow can be found here:
<https://github.com/r-lib/actions/tree/master/examples>.

Y
