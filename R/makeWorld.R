#' Make world
#'
#' @param size Integer. How big should each side of the the square world be in pixels?
#' @param range Integer. How spatially autocorrelated do you want the data?  Higher values are more coarsely autocorrelated.
#' @param discrete Logical. Should the values of cells in the world be discrete? If set to \code{FALSE} then continuous values are returned.
#' @param nclasses Integer. How many classes (e.g. habitats) do you want to produce? This is ignored if \code{discrete=FALSE}.
#' @param habitatColors Character vector of colors for the plot
#' @param plot Logical Should the resulting plot of the world be printed? The plot is not returned in any case.
#' @param returnDF Logical. Do you want to return the dataframe with the raw variables or not? Default is TRUE.
#' @param main a title for the plot
#'
makeWorld <- function(size=50, range=5, discrete=TRUE, nclasses=2, habitatColors = c("#003d29","#cac958"), plot=FALSE, returnDF=TRUE, main=NULL){
  require(gstat)
  require(ggplot2)

  xy <- expand.grid(1:size, 1:size)
  names(xy) <- c('x','y')
  g.dummy <- gstat(formula=z~1, locations=~x+y, dummy=T, beta=1, model=vgm(psill=0.025, range=range, model='Exp'), nmax=20)
  yy <- predict(g.dummy, newdata=xy, nsim = 1)
  names(yy) <- c("x","y","value")

  if(discrete) yy$value <- cut(yy$value,nclasses,labels=F)

  theme_set(
    theme_minimal(14) +
      theme(
        panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank()
      )
  )

  if(plot){
    thePlot <-
      ggplot(data=yy) +
      coord_fixed() +
      theme(legend.position = "none")
    if(discrete){
      thePlot <- thePlot +
                  geom_raster(aes(x=x, y=y, fill=factor(value))) +
                  scale_fill_manual(values=habitatColors)
    } else {
      thePlot <- thePlot +
                  scale_fill_gradient(low=habitatColors[1], high=habitatColors[2]) +
                  geom_raster(aes(x=x, y=y, fill=value))
    }

    if(missing(main)) thePlot <- thePlot + labs(title=sprintf("%dx%d world, range=%d",size,size,range))
    print(thePlot)
  }

  if(returnDF) return(yy)
}
