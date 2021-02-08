library(ggplot2)
library(gridExtra)

plot_landcover_frequency <- function(ezg, catchment, classes, type) {
  p1 <- ggplot() +
    geom_sf(data = ezg) +
    geom_sf(data = catchment, aes(fill = name))
  
  if (type == "pie") {
    p2 <- create_piechart(classes)  
  } else if(type == "bar") {
    p2 <- create_barchart(classes)  
  } else {
    p2 <- create_barchart(classes)  
  }
  grid.arrange(p1, p2)
}

create_piechart <- function(classes) {
  ggplot(classes, aes(x = "", y = freq, fill = class)) +
    geom_bar(width = 1, stat = "identity") +
    coord_polar("y", start = 0) +
    labs(x = "", y = "") +
    geom_text(aes(label = paste0(round(freq * 100), "%")), position = position_stack(vjust = 0.)) +
    theme_void()
}

create_barchart <- function(classes) {
  ggplot(classes, aes(x = class, y = freq * 100, fill = class)) +
    geom_bar(stat = "identity") +
    labs(x = "", y = "") +
    geom_text(aes(label = paste0(round(freq * 100), "%")), position = position_stack(vjust = 0.5)) +
    coord_flip()
}