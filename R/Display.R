#' @import grid
NULL

#' sliceData
#' 
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
#' @param imslice vector or matrix of intensity values
#' @param col a color map
#' @param zero_col the background color.
#' @param alpha transparency multiplier
#' @importFrom grDevices col2rgb
#' @export
mapToColors <- function(imslice, col=heat.colors(128, alpha = 1), zero_col = "#00000000", alpha=1) {
  vrange <- range(imslice)
  imcols <- col[as.integer((imslice - vrange[1])/diff(vrange) * (length(col) -1) + 1)]
  
  if (!is.vector(imslice)) {
    dim(imcols) <- dim(imslice)
  }
  
  imcols[imslice == 0] <- zero_col
  
  if (alpha < 1) {
    rgbmat <- col2rgb(imcols, alpha=TRUE)
    rgbmat <- rgbmat/255
    rgbmat[4,] <- rgbmat[4,] * alpha
    
    if (is.vector(imslice)) {
      array(t(rgbmat), c(length(imslice), 4))
    } else {
      array(t(rgbmat), c(dim(imslice), 4))
    }
  } else {
    imcols
  }
  
}

#' image
#' 
#' @param slice the voxel index of the slice to display
#' @param col a color map
#' @param zero_col the color to use when the value is 0 (e.g background color)
#' @param ... extra arguments to passed to \code{grid.raster}
#' @export
#' @rdname image-methods
setMethod(f="image", signature=signature(x = "BrainVolume"),
          def=function(x, slice=dim(vol)[3]/2, col=gray((0:255)/255, alpha=1), 
                       zero_col = "#000000", axis=3, ...) {    
            imslice <- sliceData(x, slice, axis)
            imcols <- mapToColors(imslice, col, zero_col)
            ras <- as.raster(imcols)
            ras[imslice == 0] <- zero_col
            
            grid.newpage()
            grid.raster(ras, ...)
          })


#' as.raster
#' 
#' @export 
#' @param x the layer to convert
#' @param zpos the z coordinate in coordinate space
#' @rdname as.raster-methods
setMethod(f="as.raster", signature=signature(x = "Layer"),
          def=function(x, zpos) {  
            slice <- axisToIndex(x@view_space, zpos, 3)
            imslice <- sliceData(x@vol, slice, 3)     
            vrange <- range(imslice)
            
            thresh <- x@thresh
            
            lookup <- (imslice - vrange[1])/diff(vrange) * (length(x@color_map) -1) + 1
            imcols <- x@color_map[lookup]
            
            if (length(thresh) == 1) {
              thresh <- c(-Inf, thresh)
            }
                      
            imcols[imslice == 0] <- x@zero_col
            
            if (diff(thresh) > 0) {
              imcols[(imslice >= thresh[1] & imslice <= thresh[2])] <- "#00000000"
            }
            
            dim(imcols) <- dim(imslice)
            ras <- as.raster(imcols)
            #ras[imslice == 0] <- zero_col            
            ras
          })





#' image
#' 
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


#' @rdname image-methods
#' @export
setMethod(f="image", signature=signature(x = "Layer"),
          def=function(x, zpos, axis=3) {  
            ras <- as.raster(x, zpos, x@thresh,axis=axis)      
            #grid.newpage()
            grid.raster(ras, interpolate=TRUE)
          })


#' @export
#' @rdname render-methods
#' @param zero_col color used when background intensity is 0.
#' @param alpha transparency multiplier
#' @param units grid unit type, e.g. "points", "mm", "inches", "npc"
#' 
setMethod(f="render", signature=signature(x="BrainSlice", width="numeric", height="numeric", colmap="character"),
          def=function(x, width, height, colmap, zero_col="#00000000", alpha=1, units="points") {
            imslice <- t(x[1:nrow(x), ncol(x):1,drop=FALSE]) 
            imcols <- mapToColors(imslice, colmap, zero_col, alpha=alpha)
            ras <- as.raster(imcols)
  
            grob <- rasterGrob(ras, 
                               width=unit(width, units), 
                               height=unit(height, units), 
                               interpolate=TRUE)
    
          })



#orthoPlot <- function(layer, zpos) {
#  
#  vpmain <- viewport(x=0, y=0, width=1, height=1)
#  vptopright <- viewport(x=unit(.5, "npc"), y=unit(0, "npc"), width=unit(.5, "npc"), height=unit(.5, "npc"))
#  vpbottomright <- viewport(x=unit(.5, "npc"), y=unit(.5, "npc"), width=unit(.5, "npc"), height=unit(.5, "npc"))
#  vpleft <- viewport(x=unit(0, "npc"), y=unit(0,"ncp"), width=unit(.5, "npc"), height=unit(1, "npc"))
#}

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

# image_grid
# 
#' Display a set of images slices in a 2D montage
#' 
# @param layer the layer to display
# @param gridDim the dimensions of the 2D grid montage
# @param zstart the z coordinate of the first slice
# @param zend the z coordinate of the last slice
# @param panelSize the size of each panel in the montage (default unit is inches)
# @param panelUnit the unit for the panel size (default is inches)
# @param interpolate whether to interpolate pixel values
# @param fontCol color of labels indicating slice level
# @rdname imageGrid
# image_grid <- function(layer, gridDim=c(3,3), zstart, zend, panelSize=3, panelUnit="inches", interpolate=FALSE, fontCol="red") {
#   slices <- seq(zstart, zend, length.out=prod(gridDim))
#   grid.newpage()
#   layout <- grid.layout(gridDim[1], gridDim[2], widths=rep(unit(panelSize, panelUnit), gridDim[2]),  
#                                                 heights=rep(unit(panelSize, panelUnit), gridDim[1]))
#   
#   grid.rect(unit(0, "npc"), y=unit(0, "npc"), just=c("left", "bottom"), gp=gpar(fill="black"))
#   pushViewport(viewport(layout=layout))
#   
#   scount = 1
#   for (i in 1:gridDim[1]) {
#     for (j in 1:gridDim[2]) {
#       pushViewport(viewport(layout.pos.col=j, layout.pos.row=i))
#       ras <- as.raster(layer, slices[scount])
#       
#       grid.raster(ras, interpolate=interpolate)
#       grid.text(paste(round(slices[scount])), x=unit(.15, "npc"), y=unit(.1, "npc"), 
#                 just="centre", gp=gpar(fontsize=14, col=fontCol))
#       popViewport()
#       scount <- scount+1
#     }
#       
#   }
#   
# }



# as.raster
# 
# @export 
# @param x the layer to convert
# @param zpos the z coordinate in coordinate space
# @rdname as.raster-methods
# setMethod(f="as.raster", signature=signature(x = "Layer"),
#           def=function(x, zpos) {  
#             slice <- axisToIndex(x@view_space, zpos, 3)
#             imslice <- sliceData(x@vol, slice, 3)     
#             vrange <- range(imslice)
#             
#             thresh <- x@thresh
#             
#             lookup <- (imslice - vrange[1])/diff(vrange) * (length(x@colorMap) -1) + 1
#             imcols <- x@colorMap[lookup]
#             
#             if (length(thresh) == 1) {
#               thresh <- c(-Inf, thresh)
#             }
#                       
#             imcols[imslice == 0] <- x@zero_col
#             
#             if (diff(thresh) > 0) {
#               imcols[(imslice >= thresh[1] & imslice <= thresh[2])] <- "#00000000"
#             }
#             
#             dim(imcols) <- dim(imslice)
#             ras <- as.raster(imcols)
#             #ras[imslice == 0] <- zero.col            
#             ras
#           })
# 


