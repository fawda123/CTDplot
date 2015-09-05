
## CTDplot

#### *Marcus W. Beck, mbafs2012@gmail.com*

[![Travis-CI Build Status](https://travis-ci.org/fawda123/CTDplot.svg?branch=master)](https://travis-ci.org/fawda123/CTDplot)

Create two-dimensional contour plots of CTD water quality data.  Currently only works with example data for Pensacola Bay, Florida. Install the package as follows:


```r
install.packages(devtools)
library(devtools)
install_github('fawda123/CTDplot')
library(CTDplot)
```



```r
data(ctd_ex)

# default plot
ctd_plot(ctd_ex, 'Salinity')
```

![](README_files/figure-html/unnamed-chunk-3-1.png) 

```r
# make color ramp match contour categories in legend
ctd_plot(ctd_ex, 'Salinity', ncol = 8)
```

![](README_files/figure-html/unnamed-chunk-3-2.png) 

```r
# change colors
ctd_plot(ctd_ex, 'Salinity', cols = c('Blue', 'Purple', 'Orange'))
```

![](README_files/figure-html/unnamed-chunk-3-3.png) 
