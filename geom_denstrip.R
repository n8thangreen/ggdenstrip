library(ggplot2)
library(grid)

#
GeomDENSTRIP <- ggproto(
  "GeomDENSTRIP", Geom,
  required_aes = c("x", "y", "fill", "width", "height"),
  
  default_aes = aes(width = 1, height = 1, colour = NA, alpha = NA),
  
  draw_key = draw_key_rect,
  
  draw_panel = function(data, panel_params, coord) {
    coords <- coord$transform(data, panel_params)
    
    rects <- lapply(seq_len(nrow(coords)), function(i) {
      grid::rectGrob(
        x = coords$x[i],
        y = coords$y[i],
        width = coords$width[i],
        height = coords$height[i],
        just = "center",
        gp = grid::gpar(
          fill = alpha(coords$fill[i], coords$alpha[i]),
          col = coords$colour[i]
        )
      )
    })
    
    # return a single object
    grid::gTree(children = do.call(gList, rects))
  }
)

# wrapper
geom_denstrip <- function(mapping = NULL,
                          data = NULL,
                          stat = "identity",
                          position = "identity",
                          ...,
                          # width = 1, height = 1,
                          na.rm = FALSE,
                          show.legend = NA,
                          inherit.aes = TRUE) {
  layer(
    geom = GeomDENSTRIP,
    mapping = mapping,
    data = data,
    stat = stat,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

#
geom_denstrip_continuous <- function(mapping = NULL,
                                     data = NULL,
                                     ..., 
                                     low = "white", high = "black") {
  list(
    geom_denstrip(mapping = mapping, data = data, ...),
    scale_fill_continuous(low = low, high = high)
  )
}
