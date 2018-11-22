library(ggplot2)
library(gridExtra)
library(grid)


regre <- function(df){
  m <- lm(y ~ x, df);
  eq <- substitute(y == b %.% italic(x) + a *","~~r^2~"="~r2, 
                   list(a = round(coef(m)[[1]],digits=2), 
                        b = round(coef(m)[[2]], digits = 4), 
                        r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq));                 
}


grid.arrange_tp.final<- function(..., nrow = 1, ncol = length(list(...)), position = c("bottom", "right"),titulo) {
  
  plots <- list(...)
  position <- match.arg(position)
  g <- ggplotGrob(plots[[4]] + theme(legend.position = position))$grobs
  legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
  lheight <- sum(legend$height)
  lwidth <- sum(legend$width)
  gl <- lapply(plots, function(x) x + theme(legend.position = "none"))
  gl <- c(gl, nrow = nrow, ncol = ncol)
  
  combined <- switch(position,
                     "bottom" = arrangeGrob(do.call(arrangeGrob, gl),
                                            legend,
                                            ncol = 1,
                                            heights = unit.c(unit(1, "npc") - lheight, lheight)),
                     "right" = arrangeGrob(do.call(arrangeGrob,gl),top=titulo,
                                           legend,
                                           ncol = 2,
                                           widths = unit.c(unit(1, "npc") - lwidth, lwidth)))
  grid.newpage()
  grid.draw(combined)
  
}




