# ggplot version of denstrip underneath a main plot


library(ggplot2)
library(denstrip)


x <- seq(-4, 4, length=10000)
dens <- dnorm(x)
denstrip(x, dens, at=0)
denstrip(x, dens, width=0.5, at=0) 

#####

# ggplot2 version of denstrip function
#
# @include ggplot2
# @examples
#
# x <- rnorm(1000)  # sample data
# 
# mydens <- ggdenstrip(dat = x, at = 1,
#                      colmax = "black",
#                      ticks = c(-1, 0, 1))
# 
# empty_plot <- ggplot() + theme_bw()
# 
# ggpubr::ggarrange(empty_plot, mydens,
#                   heights = c(2, 0.7),
#                   ncol = 1, nrow = 2)
# 
# ## pass density directly 
# # beta distribution
# x <- seq(0, 1, by=0.01)
# mydens2 <- ggdenstrip(x = x,
#                       dens = dbeta(x, 5, 10))
# 
# ggpubr::ggarrange(empty_plot, mydens2,
#                   heights = c(2, 0.7),
#                   ncol = 1, nrow = 2)
# 
# # normal distribution
# x <- seq(-1, 1, by=0.01)
# mydens3 <- ggdenstrip(x = x,
#                       dens = dnorm(x, 0.5, 0.15))
# # dens = dnorm(x, -0.5, 0.15))
# 
# ggpubr::ggarrange(empty_plot, mydens3,
#                   heights = c(2, 0.7),
#                   ncol = 1, nrow = 2)
# 
ggdenstrip <- function(dat = NA,   # sample data
                       x = NA,     # x-coords
                       dens = NA,  # density values
                       at = 0,     # y-axis location
                       width = NULL,
                       height = NULL,
                       colmax = "black",  # colour maximum
                       colmin = "white",  # colour minimum
                       scale = 1,
                       x_scale = FALSE,  # rescale to interval [0, 1]
                       gamma = 1,
                       n = 512,
                       horiz = TRUE,
                       ticks = NULL,  ##TODO:
                       tlen = 1.5,
                       tcol = colmax,
                       twd = 0.5, ...) {
  
  if (!any(is.na(dat))) {
    
    # kernel estimation
    strip_dens <- density(dat, n = n, ...)
    
    # normalize and apply scale
    dens <- strip_dens$y / max(strip_dens$y) * scale
    
    x <- strip_dens$x
  }
  
  if (x_scale) {
    x_range <- max(x) - min(x)
    x <- (x - min(x))/x_range
  }
  
  # default width
  if (is.null(width)) {
    width <- diff(range(x)) / (length(x) - 1)
  }
  
  if (is.null(height)) {
    height <- width  
  }
  
  df <- data.frame(
    x = if (horiz) x else at,
    width = if (horiz) width else height,
    height = if (horiz) height else width,
    dens = dens,
    y = if (horiz) at else x
  )
  
  p <- 
    ggplot(df, aes(x = x, y = y, fill = dens,
                   width = width, height = height)) +
    geom_denstrip_continuous(low = "white", high = "black") +
    theme_void() +
    theme(legend.position = "none")
  
  # Optional ticks
  if (!is.null(ticks)) {
    tick_df <- data.frame(
      x = ticks,
      xend = ticks,
      y = at - width * tlen / 2,
      yend = at + width * tlen / 2
    )
    
    p <- p +
      geom_segment(data = tick_df,
                   aes(x = x, xend = xend,
                       y = y, yend = yend),
                   color = tcol,
                   linewidth = twd)
  }
  
  return(p)
}

x <- rnorm(1000)  # sample data

hdens <- ggdenstrip(dat = x, at = 1, height = 0.8,
                     colmax = "black")
                     # ticks = c(-1, 0, 1))

vdens <- ggdenstrip(dat = x, at = 1, 
                    horiz = FALSE,
                    height = 0.8,
                    colmax = "black")
                    # ticks = c(-1, 0, 1))

empty_plot <- ggplot() + theme_bw()

ggpubr::ggarrange(empty_plot, hdens,
                  heights = c(2, 0.7),
                  ncol = 1, nrow = 2)

ggpubr::ggarrange(empty_plot, vdens, hdens,
                  heights = c(2, 0.7),widths = c(2, 0.7),
                  ncol = 2, nrow = 2)

## pass density directly
# beta distribution
x <- seq(0, 1, by=0.01)
mydens2 <- ggdenstrip(x = x, height = 0.3,
                      dens = dbeta(x, 5, 10))

ggpubr::ggarrange(empty_plot, mydens2,
                  heights = c(2, 0.7),
                  ncol = 1, nrow = 2)

# normal distribution
x <- seq(-1, 1, by=0.01)
mydens3 <- ggdenstrip(x = x, height = 0.3,
                      dens = dnorm(x, 0.5, 0.15))
# dens = dnorm(x, -0.5, 0.15))

ggpubr::ggarrange(empty_plot, mydens3,
                  heights = c(2, 0.7),
                  ncol = 1, nrow = 2)

