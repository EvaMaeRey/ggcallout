compute_labellink <- function(data, scales, label_direction = 180 + 45, prop_range = .1, prop_pointer_pad = .0175, hjust = NULL, vjust = NULL, which_index = NULL, which_id = NULL){
  
  if(is.null(data$id)){data$id <- "hello world"}
  if(is.null(which_index)){which_index <- which(data$id %in% which_id)}
  
  data$default_label <- data$id
  
  xmean <- mean(data$x)
  ymean <- mean(data$y)
  
  range_x <- diff(range(data$x))
  range_y <- diff(range(data$y)) # look at range of plot?
  xdir <- cos(pi*label_direction/180)
  ydir <- sin(pi*label_direction/180)
  xpush <- range_x * prop_range * xdir
  ypush <- range_y * prop_range * ydir
  
  xpointer_pad <- range_x *xdir* prop_pointer_pad
  ypointer_pad <- range_y *ydir* prop_pointer_pad
  
  more_x_than_y <- abs(xdir) > abs(ydir)
  
  if(is.null(hjust)){hjust <- ifelse(more_x_than_y, sign(xdir) != 1, .5)}
  if(is.null(vjust)){vjust <- ifelse(more_x_than_y, .5, sign(ydir) != 1)}

  data |> 
    dplyr::mutate(x = x + xpush) |>
    dplyr::mutate(y = y + ypush) |>
    dplyr::mutate(xend = .data$x - (xpush - xpointer_pad)) |>
    dplyr::mutate(yend = .data$y - (ypush - ypointer_pad)) |>
    dplyr::mutate(hjust = hjust) |>
    dplyr::mutate(vjust = vjust) |> 
    dplyr::slice(which_index)
  
}

StatLabellink <- ggplot2::ggproto("Labellink",
                         ggplot2::Stat,
                         compute_panel = compute_labellink,
                         default_aes = 
                           ggplot2::aes(label = ggplot2::after_stat(default_label)))




