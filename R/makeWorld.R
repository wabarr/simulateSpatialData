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
#' @param writeNetlogoImportFile Logical. Should the function write out a minimal Netlogo Import file?
#' @param NetlogoImportFilePath Character. path of file to write. 
#' @param NetlogoImportFilename Character. name of file to write
#'
makeWorld <- function(size=50, 
                      range=5, 
                      discrete=TRUE, 
                      nclasses=2, 
                      habitatColors = c("#003d29","#cac958"), 
                      plot=FALSE, 
                      returnDF=TRUE, 
                      main=NULL,
                      writeNetlogoImportFile=TRUE,
                      NetlogoImportFilePath="~/Desktop",
                      NetlogoImportFilename="world.txt"){
  require(gstat)
  require(ggplot2)
  require(scales)

  xy <- expand.grid(1:size, 1:size)
  names(xy) <- c('x','y')
  g.dummy <- gstat(formula=z~1, locations=~x+y, dummy=T, beta=1, model=vgm(psill=0.025, range=range, model='Exp'), nmax=20)
  yy <- predict(g.dummy, newdata=xy, nsim = 1)
  names(yy) <- c("pxcor","pycor","resource-value")
  yy$pxcor <- yy$pxcor - 1
  yy$pycor <- yy$pycor - 1
  if(discrete) yy$`resource-value` <- cut(yy$`resource-value`,nclasses,labels=F)

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
  
  yy$pcolor<-rescale(yy$`resource-value`, to=c(52,58))
  yy$plabel<-NA
  yy$`plabel-color`<-NA
  
   if(writeNetlogoImportFile){
     f <- file(paste(NetlogoImportFilePath,NetlogoImportFilename,sep="/"), open = "w")
     writeLines('"GLOBALS"', con=f)
     writeLines('"min-pxcor","max-pxcor","min-pycor","max-pycor", "nclasses"', con=f)
     writeLines(paste(c(0,size-1,0,size-1, nclasses),collapse=","), con=f)
     writeLines('"TURTLES"', con=f)
     writeLines('"who","color","heading","xcor","ycor","shape","label","label-color","breed","hidden?","size","pen-size","pen-mode"
', con=f)
     writeLines('"PATCHES"', con=f)
     writeLines('"pxcor","pycor","resource-value","pcolor","plabel","plabel-color"')
     write.table(yy,file=f, append=TRUE, row.names=FALSE, sep=",",na="")
     
     writeLines('"LINKS"', con=f)
     writeLines('"end1","end2","color","label","label-color","hidden?","breed","thickness","shape","tie-mode"', con=f)
     
     
     close(f)
   }

  if(returnDF) return(yy)
}
