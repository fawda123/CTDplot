
## CTDplot

#### *Marcus W. Beck, mbafs2012@gmail.com*

[![Travis-CI Build Status](https://travis-ci.org/fawda123/CTDplot.svg?branch=master)](https://travis-ci.org/fawda123/CTDplot)

Create two-dimensional contour plots of CTD water quality data.  Currently only works with example data for Pensacola Bay, Florida. Install the package as follows:


```r
install.packages('devtools')
library(devtools)
install_github('fawda123/CTDplot')
library(CTDplot)
```



```r
# default plot
ctd_plot(dat_in = ctd_ex1, var_plo = 'Salinity', dep_in = PB_dep_pts, window = 5, ylim = c(-12, 0))
```

![](README_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
# make color ramp match contour categories in legend
ctd_plot(dat_in = ctd_ex1, var_plo = 'Salinity', dep_in = PB_dep_pts, ncol = 8, window = 5, ylim = c(-12, 0))
```

![](README_files/figure-html/unnamed-chunk-3-2.png)<!-- -->

```r
# change colors
ctd_plot(dat_in = ctd_ex1, var_plo = 'Salinity', dep_in = PB_dep_pts, cols = c('Blue', 'Purple', 'Orange'),
  window = 5, ylim = c(-12, 0))
```

![](README_files/figure-html/unnamed-chunk-3-3.png)<!-- -->

```r
# plot two dates with same color scaling
dat <- list(ctd_ex1, ctd_ex2)
ctd_plotmult(dat, 'Salinity', PB_dep_pts, var_labs = c('April', 'May'),
  window = 5, ylim = c(-12, 0))
```

![](README_files/figure-html/unnamed-chunk-4-1.png)<!-- -->
