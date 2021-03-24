# kogpsy R package

Not much to see yet.


## Installation

```r
remotes::install_github("awellis/kogpsy")
```

##  Diffusion decision model (DDM)

To generate a discrete-time diffusion process:

```r
library(kogpsy)
?drift_diffusion

out <- drift_diffusion(bias = 0.3, driftrate = 0.8)
```


```r
out %>% 
	ggplot(aes(time, dv)) + 
	geom_line()
```
