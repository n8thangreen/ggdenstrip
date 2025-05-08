StatDENSTRIP <- ggproto("StatDENSTRIP", Stat,
  required_aes = c("x", "y"),  # assuming each x, y is a "group" of values
  compute_group = function(data, scales, n = 100, bw = "nrd0") {
    # Estimate density of y values at this x position
    dens <- density(data$y, bw = bw, n = n)
    
    # Return a data.frame with x fixed, y varying across the density
    data.frame(
      x = unique(data$x),
      y = dens$x,
      fill = dens$y,
      width = max(dens$y) / n,
      height = diff(range(dens$x)) / n
    )
  }
)

stat_denstrip <- function(mapping = NULL, data = NULL,
                          geom = "denstrip",
                          position = "identity",
                          ...,
                          bw = "nrd0",
                          n = 100,
                          na.rm = FALSE,
                          show.legend = NA,
                          inherit.aes = TRUE) {
  layer(
    stat = StatDENSTRIP,
    data = data,
    mapping = mapping,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(bw = bw, n = n, na.rm = na.rm, ...)
  )
}

# library(ggplot2)
# 
# # Simulated posterior at each x
# df <- data.frame(
#   x = rep(1:5, each = 100),
#   y = c(rnorm(100, 0), rnorm(100, 0.5), rnorm(100, 1), rnorm(100, 2), rnorm(100, 2.5))
# )
#
# ggplot(df, aes(x = x, y = y)) +
#   stat_denstrip() +
#   coord_cartesian(ylim = c(-3, 5))

