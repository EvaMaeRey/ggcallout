geom_labellink <- function(  mapping = NULL,
  data = NULL,
  position = "identity",
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE, ...){

  
  list( 
    
    ggplot2::layer("segment", 
        "labellink", 
        position = position, 
        data = data, 
        mapping = mapping,
            show.legend = show.legend,
    inherit.aes = inherit.aes,
        params = list(arrow = 
                        arrow(ends = "last", 
                              length = unit(.1, "inches"), 
                              type = "closed"), na.rm = na.rm,
                      ...)),
    
  ggplot2::layer("label", 
        "labellink", 
        position = position, 
        data = data, 
        mapping = mapping, 
            show.legend = show.legend,
    inherit.aes = inherit.aes,
        params = list(
                      alpha = 0,
                      lineheight = .8,
                      label.size = 0,
                      label.padding = unit(0.4, "lines"), 
                      na.rm = na.rm,
                      ...)) 
  )

  
}


