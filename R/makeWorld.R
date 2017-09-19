#' Make world
#'
#' @param size how big should each side of the the square world be in pixels?
#' @param range how spatially autocorrelated do you want the data?  Higher values are more coarsely autocorrelated.
#' @param nclasses how many classes (e.g. habitats) do you want to produce?
#' @param habitatColors a vector of colors for the plot
#' @param returnDF do you want to return the dataframe with the raw variables or not?
#' @param main a title for the plot
#'
makeWorld <- function(size=100, range=5, nclasses=2, habitatColors = c("#fff200","#1db125"), returnDF=FALSE, main=NULL){
  require(gstat)
  require(ggplot2)
  xy <- expand.grid(1:size, 1:size)
  names(xy) <- c('x','y')
  g.dummy <- gstat(formula=z~1, locations=~x+y, dummy=T, beta=1, model=vgm(psill=0.025, range=range, model='Exp'), nmax=20)
  yy <- predict(g.dummy, newdata=xy, nsim = 1)
  names(yy) <- c("x","y","category")
  yy$category <- cut(yy$category,nclasses,labels=F)
  theme_set(theme_minimal())
  thePlot <-
    ggplot(data=yy) +
    geom_raster(aes(x=x, y=y, fill=factor(category))) +
    coord_fixed() +
    theme(legend.position = "none") +
    scale_fill_manual(values=habitatColors)

  if(missing(main)) thePlot <- thePlot + labs(title=sprintf("%dx%d world, range=%d",size,size,range))
  print(thePlot)

  if(returnDF) return(yy)
}
