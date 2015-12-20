#' @import grid
NULL

#' sliceData
#' extract a 2D slice from a \code{BrainVolume} instance.
#' 
#' @param vol an \code{BrainVolume} instance
#' @param slice the integer index of the slice to cut.
#' @param axis the axis number (1, 2, 3) defining fixed axis of the 2D slice.
#' @export
sliceData <- function(vol, slice, axis=3) {
  imslice <- switch(axis,
                    "1"=vol[slice,,],
                    "2"=vol[,slice,],
                    "3"=vol[,,slice])
  
  imslice <- t(imslice[nrow(imslice):1, ncol(imslice):1,drop=FALSE])    
  
}

#' mapToColors
#' 
#' map an matrix of intensity values to a matrix of color values.
#' 
#' @importFrom grDevices heat.colors
#' @param imslice an image matrix defining intensity values
#' @param col a color map
#' @param zero.col the background color.
#' @export
mapToColors <- function(imslice, col=heat.colors(128, alpha = 1), zero.col = "#00000000") {
  vrange <- range(imslice)
  imcols <- col[(imslice - vrange[1])/diff(vrange) * (length(col) -1) + 1]
  dim(imcols) <- dim(imslice)
  
  imcols[imslice == 0] <- zero.col
  imcols
}

#' image
#' @param slice the voxel index of the slice to display
#' @param col a color map
#' @param zero.col the color to use when the value is 0 (e.g background color)
#' @param ... extra arguments to passed to \code{grid.raster}
#' @export
#' @rdname image-methods
setMethod(f="image", signature=signature(x = "BrainVolume"),
          def=function(x, slice, col=gray((0:255)/255, alpha=1), zero.col = "#000000", axis=3, ...) {    
            imslice <- sliceData(x, slice, axis)
            imcols <- mapToColors(imslice, col, zero.col)
            ras <- as.raster(imcols)
            ras[imslice == 0] <- zero.col
            
            grid.newpage()
            grid.raster(ras, ...)
          })

#' Layer
#' 
#' create a \code{\linkS4class{Layer}} object
#' @param vol volume instance of \code{\linkS4class{BrainVolume}}
#' @param colorMap a lookup table defining mapping from image intensity values to colors.
#' @param thresh a range (min,max) defining the threshold window for determining image opacity.
#' @param axis the axis index of the axis perpendicular to the xy plane (options: 1,2,3; default is 3)
#' @param zero.col the color used when the value is zero.
#' @return an object of class \code{Layer}
#' @export
#' @rdname Layer
#' @importFrom grDevices gray
Layer <- function(vol, colorMap=gray((0:255)/255, alpha=1), thresh=c(0,0), axis=3, zero.col="#000000") {
  new("Layer", vol=vol, colorMap=colorMap, thresh=thresh, axis=axis, zero.col=zero.col)
}



#' as.grob
#' 
#' @param layer the \code{Layer} instance
#' @param zpos the z slice coordinate
#' @param thresh the threshold range
#' @param axis the axis index (1,2,3)
#' @param width the display width in pixels
#' @param height the display height in pixels
as.grob <- function(x,zpos,thresh,axis,width=NULL,height=NULL) {
        
}


#' as.raster
#' 
#' @export 
#' @param x the layer to convert
#' @param zpos the z coordinate in coordinate space
#' @param thresh the threshold range
#' @param axis the axis index (1,2,3)
#' @rdname as.raster-methods
setMethod(f="as.raster", signature=signature(x = "Layer"),
          def=function(x, zpos, thresh=c(0,0), axis=3) {  
            slice <- axisToIndex(space(x@vol), zpos, axis)
            imslice <- sliceData(x@vol, slice, axis)     
            vrange <- range(imslice)
            
            lookup <- (imslice - vrange[1])/diff(vrange) * (length(x@colorMap) -1) + 1
            
            imcols <- x@colorMap[lookup]
            
            if (length(thresh) == 1) {
              thresh <- c(-Inf, thresh)
            }
                      
            imcols[imslice == 0] <- x@zero.col
            
            if (diff(thresh) > 0) {
              imcols[(imslice >= thresh[1] & imslice <= thresh[2])] <- "#00000000"
            }
            
            dim(imcols) <- dim(imslice)
            ras <- as.raster(imcols)
            #ras[imslice == 0] <- zero.col            
            ras
          })




#' overlay
#' 
#' @export 
#' @rdname overlay-methods
setMethod(f="overlay", signature=signature(x = "Layer", y="Layer"),
          def=function(x, y) {  
            new("Overlay", layers=list(x,y))  
          })


#' @export 
#' @rdname overlay-methods
#' @param e1 the left operand
#' @param e2 the right operand
setMethod(f="+", signature=signature(e1 = "Overlay", e2="Layer"),
          def=function(e1, e2) {  
            new("Overlay", layers=c(e1@layers, e2))
          })

#' @export 
#' @rdname overlay-methods
setMethod(f="+", signature=signature(e1 = "Layer", e2="Layer"),
          def=function(e1, e2) {  
            new("Overlay", layers=list(e1, e2))
          })

#' image
#' @param x the object to display
#' @param zpos the z coordinate
#' @param axis the axis index
#' @export
#' @rdname image-methods
setMethod(f="image", signature=signature(x = "Overlay"),
          def=function(x, zpos, axis=3) {  
            #grid.newpage()
            for (layer in x@layers) {
              ras <- as.raster(layer, zpos, layer@thresh, axis=axis) 
              grid.raster(ras, interpolate=TRUE)
            }             
          })

#' image
#' @rdname image-methods
#' @export
setMethod(f="image", signature=signature(x = "Layer"),
          def=function(x, zpos, axis=3) {  
            ras <- as.raster(x, zpos, x@thresh,axis=axis)      
            #grid.newpage()
            grid.raster(ras, interpolate=TRUE)
          })


# plotMontage <- function(x, layout=c(3,3), zstart, zend) {
#   
#   zslices <- seq(zstart, zend,by=(zend-zstart)/prod(layout))
#   
#   raslist <- lapply(zslices, function(z) {
#     print(z)
#     zind <- axisToIndex(space(x@vol), z, x@axis)
#     dat <- slice(x@vol, zind, x@axis,"")
#     xy <- indexToCoord(space(dat), 1:length(dat))
#     list(xy=as.matrix(xy), values=as.numeric(dat@.Data), slice=z)
#   })
#   
#   xy <- do.call(rbind, lapply(raslist, "[[", "xy"))
#   dfras <- data.frame(x=xy[,1], y=xy[,2], values=unlist(lapply(raslist, "[[", "values")), slice=sapply(raslist, "[[", "slice"))
#   dfras$slice <- factor(dfras$slice)
#   
#   p <- ggplot(data=dfras, aes(x=x,y=y)) +
#     theme_bw() + coord_equal() +
#     geom_raster(aes(fill=values)) +
#     facet_grid(. ~ slice)
#   
#     
#   
# }
# 

#' imageGrid
#' 
#' Display a set of images slices in a 2D montage
#' 
#' @param layer the layer to display
#' @param gridDim the dimensions of the 2D grid montage
#' @param zstart the z coordinate of the first slice
#' @param zend the z coordinate of the last slice
#' @param panelSize the size of each panel in the montage (default unit is inches)
#' @param panelUnit the unit for the panel size (default is inches)
#' @param interpolate whether to interpolate pixel values
#' @param fontCol color of labels indicating slice level
#' @rdname imageGrid
imageGrid <- function(layer, gridDim=c(3,3), zstart, zend, panelSize=3, panelUnit="inches", interpolate=FALSE, fontCol="red") {
  slices <- seq(zstart, zend, length.out=prod(gridDim))
  grid.newpage()
  layout <- grid.layout(gridDim[1], gridDim[2], widths=rep(unit(panelSize, panelUnit), gridDim[2]),  
                                                heights=rep(unit(panelSize, panelUnit), gridDim[1]))
  
  grid.rect(unit(0, "npc"), y=unit(0, "npc"), just=c("left", "bottom"), gp=gpar(fill="black"))
  pushViewport(viewport(layout=layout))
  
  scount = 1
  for (i in 1:gridDim[1]) {
    for (j in 1:gridDim[2]) {
      pushViewport(viewport(layout.pos.col=j, layout.pos.row=i))
      ras <- as.raster(layer, slices[scount], layer@thresh, layer@axis)
      
      grid.raster(ras, interpolate=interpolate)
      grid.text(paste(round(slices[scount])), x=unit(.15, "npc"), y=unit(.1, "npc"), 
                just="centre", gp=gpar(fontsize=14, col=fontCol))
      popViewport()
      scount <- scount+1
    }
      
  }
  
}
          

