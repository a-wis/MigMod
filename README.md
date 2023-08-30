# MigMod

### Bayesian integrated Modelling of (European) Migration via Stan

This is an R package that estimates international migration flows by using data from sending and receiving countries. It uses
[Stan](https://mc-stan.org) (via the **rstan** package) for the back-end estimation. The primary target audience is students and researchers who would like to learn more about migration data and producing of synthetic migration data. 

### Installation

```r
devtools::install_github("a-wis/MigMod",build=F)
library(MigMod)
```

### How to use

The package contains one key function `migmod_m` that takes as arguments a data frame with all needed variables, sending and receiving countries for which bilateral migration will be estimated and years for which the estimation is required. The data have been put into a data set `mig_data`. Dots `...` denote other arguments that are passed to `stan` sampling function, such as the number of iterations, CPU cores to be used, etc. 

```r
modfit <- migmod_m(df = mig_data, 
                  sending = c("SE", "FI", "IT", "PL"),
                  receiving = c("SE", "FI", "IT", "PL"),
                  years = 2010:2019,...)
```

The results can be plotted by using a `plot_migmod` function that converts the `stanfit` into posterior summaries, binds with data and then plots by using `ggplot` functions.

```r
plot_migmod(df = mig_data, 
            mmfit = modfit,
            log.m = FALSE,
            sending = c("SE", "FI", "IT", "PL"),
            receiving = c("SE", "FI", "IT", "PL"),
            years = 2010:2019,...)
```
