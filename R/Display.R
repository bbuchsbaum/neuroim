#' @import grid
NULL


#' image
#' @param slice the voxel index of the slice to display
#' @param col a color map
#' @param zero.col the color to use when the value is 0 (e.g background color)
#' @rdname image-methods
setMethod(f="image", signature=signature(x = "BrainVolume"),
          def=function(x, slice, col=heat.colors(128, alpha = 1), zero.col = "#00000000") {    
            imslice <- t(x[,,slice])
            vrange <- range(imslice)
            imcols <- col[(imslice - vrange[1])/diff(vrange) * (length(col) -1) + 1]
            dim(imcols) <- dim(imslice)
            ras <- as.raster(imcols)
            ras[imslice == 0] <- zero.col
            
            grid.newpage()
            grid.raster(ras)
          })

#' create a Layer object
#' @param vol an image volume
#' @param colorMap a lookup table defining mapping from image intensity values to colors
#' @param thresh a range (min,max) defining the threshold window for determining image opacity
#' @return an object of class \code{Layer}
#' @export
Layer <- function(vol, colorMap=gray((0:255)/255, alpha=1), thresh=c(0,0)) {
  new("Layer", vol=vol, colorMap=colorMap, thresh=thresh)
}

#' as.raster
#' 
#' @export 
#' @param x the layer to convert
#' @param zpos the z coordinate 
#' @param thresh the threshold range
#' @param axis the axis index (1,2,3)
#' @rdname as.raster-methods
setMethod(f="as.raster", signature=signature(x = "Layer"),
          def=function(x, zpos, thresh=c(0,0), axis=3) {  
            slice <- axisToIndex(space(x@vol), zpos, axis)
            imslice <- switch(axis,
                   "1"=x@vol[slice,,],
                   "2"=x@vol[,slice,],
                   "3"=x@vol[,,slice])
            
            imslice <- t(imslice[nrow(imslice):1, ncol(imslice):1,drop=FALSE])          
            vrange <- range(imslice)
            
            lookup <- (imslice - vrange[1])/diff(vrange) * (length(x@colorMap) -1) + 1
            
            imcols <- x@colorMap[lookup]
            
            if (length(thresh) == 1) {
              thresh <- c(-Inf, thresh)
            }
                      
            if (diff(thresh) > 0)
              imcols[(imslice >= thresh[1] & imslice <= thresh[2])] <- "#00000000"
              
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

#' image
#' @param zpos the z coordinate
#' @param axis the axis index
#' @rdname image-methods
setMethod(f="image", signature=signature(x = "Overlay"),
          def=function(x, zpos, axis=3) {  
            grid.newpage()
            for (layer in x@layers) {
              ras <- as.raster(layer, zpos, layer@thresh, axis=axis) 
              grid.raster(ras)
            }             
          })

#' image
#' @rdname image-methods
setMethod(f="image", signature=signature(x = "Layer"),
          def=function(x, zpos, axis=3) {  
            ras <- as.raster(x, zpos, x@thresh,axis=axis)      
            grid.newpage()
            grid.raster(ras, interpolate=TRUE)
          })
            
          

