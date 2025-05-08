# ggdenstrip

The original plots are from the [{denstrip}](https://cran.r-project.org/web/packages/denstrip/index.html) package.

Since starting this code we have discovered the excellent [{ggdist}](https://mjskay.github.io/ggdist/index.html) package.
This has a feature-rich version of what we were trying here. In particular, the [`geom_interval()`](https://mjskay.github.io/ggdist/reference/geom_interval.html) is very similar.
This is then used for creating the `stat_*` functions [`stat_interval()`](https://mjskay.github.io/ggdist/reference/stat_interval.html) and [`stat_gradient()`](https://mjskay.github.io/ggdist/reference/stat_gradientinterval.html).



