
#' @importFrom R6 R6Class
Layer <- R6Class("Layer",
             portable = TRUE,  
             public = list(
               vol = NULL,
               color_map=NULL,
               thresh=NULL,
               view=NULL,
               zero_col=NULL,
               alpha=NULL,
               view_space=NULL,
               view_axes=NULL,
               desc=NULL,
               
               initialize = function(vol, color_map=gray((0:255)/255, alpha=1), thresh=c(0,0), 
                                     view="LPI", zero_col="#000000FF", alpha=1, desc="") {
                 self$vol <- vol
                 self$color_map <- color_map
                 self$thresh=thresh
                 self$view=view
                 self$zero_col=zero_col
                 self$alpha=alpha
                 
                 view <- if (length(view) == 1 && nchar(view) == 3) {
                   sapply(1:3, function(i) substr(view, i,i))
                 } else {
                   view
                 }
                 
                 stopifnot(length(view) == 3)
                 
                 self$view_axes <- findAnatomy3D(view[1], view[2], view[3])
                 self$view_space <- reorient(space(vol), view)
               },
               
               slice=function(zpos) {
                 slice(self$vol, zpos, self$view_space, self$view_axes)
               },
               
               get_zpos = function(zlevel) {
                 zprop <- zlevel/self$zdim()
                 
                 zr <- self$zrange()
                 zoffset <- zprop * diff(zr)
                 
                 zoffset + zr[1]
                 
               },
               
               zdim=function() { dim_of(space(self$vol), self$view_axes@k) },
               
               zrange=function() {
                 bds <- bounds(self$view_space)
                 zrange <- sort(bds[3,])
               },
                 
               zspacing=function() { 
                 dnum <- which_dim(space(self$vol), self$view_axes@k)
                 spacing(self$vol)[dnum]
               },
               
               render_slice=function(zpos, width=NULL, height=NULL) {
                 bds <- bounds(self$view_space)
                 zran <- self$zrange()
                 
                 zlevel <- (zpos - zran[1])/self$zspacing()
                 zdim <- self$zdim()
                
                 
                 if (zlevel >= 1 && zlevel <= zdim) {
                   zlevel <- round(zlevel)
                 } else if (zlevel >= 0 && zlevel <= 1) {
                   zlevel <- 1
                 } else if (zlevel >= zdim && zlevel <= (zdim+1)) {
                   zlevel <- zdim
                 } else {
                   stop(paste("zpos outside z bounds: ", zpos, " bounds: ", zran))
                 }
                 
                 
                 
                 bds <- t(apply(bds,1,sort))[1:2,]
                
                 slice <- slice(self$vol, zlevel, self$view_space, self$view_axes)
              
                 bds <- bounds(self$view_space)
                 bds <- t(apply(bds,1,sort))[1:2,]
                 
                 
                 wi <- diff(bds[1,])
                 hi <- diff(bds[2,])
                 aspect_ratio <- wi/hi
                 
                 if (is.null(width) && is.null(height)) {
                   width <- dim(slice)[1]
                   height <- dim(slice)[2]
                 } else if (is.null(width)) {
                   wx <- dim(slice)[1] * spacing(slice)[1]
                   wy <- dim(slice)[2] * spacing(slice)[2]
                   rat <-  wx/wy
                   width <- height * rat
                 } else if (is.null(height)) {
                   wx <- dim(slice)[1] * spacing(slice)[1]
                   wy <- dim(slice)[2] * spacing(slice)[2]
                   rat <-  wy/wx
                   height <- width * rat
                 } else {
                   rs <- width/height
                   if (rs > aspect_ratio) {
                     width <- wi * height/hi
                   } else {
                     height <- hi * width/wi
                   }
                 }
                 
                 
                 grob <- render(slice, width, height, 
                                colmap=self$color_map, 
                                zero_col=self$zero_col, 
                                alpha=self$alpha, 
                                units="points")
                 
                 RenderedSlice$new(slice=slice, width=width, height=height, xbounds=bds[1,], ybounds=bds[2,], raster=grob)
               }
             )
)


Overlay <- R6Class("Overlay",
                   portable=TRUE,
                   public = list(
                     layers=NULL,
                     view_space=NULL,
                     view_anat=NULL,
                     layer_names=NULL,
                     
                     initialize = function(...) {
                      layers=list(...)
                      lapply(layers, function(x) stopifnot(inherits(x, "Layer")))
                     
                      splist <- lapply(layers, function(layer) space(layer$vol))
                      axes <- sapply(splist, function(x) x@axes)
                      bds <- lapply(splist, function(x) signif(dim(x) * spacing(x),3))
                      views <- lapply(layers, function(x) x$view_anat)
                      
                      if (length(layers) > 1) {
                        orgs <- do.call(rbind, lapply(splist, function(x) signif(origin(x),3)))
                        dorgs <- apply(orgs, 2, function(x) max(abs(diff(x))))
                        assertthat::assert_that(all(dorgs < 2))
                      }
                      
                      
                      assertthat::assert_that(length(unique(views)) == 1)
                      assertthat::assert_that(length(unique(axes)) == 1)
                      assertthat::assert_that(length(unique(bds)) == 1)
                      
                     
                      self$view_space=layers[[1]]$view_space
                      self$view_anat=layers[[1]]$view_anat
                      
                      lnames <- names(layers)
                      
                      if (is.null(lnames)) {
                        lnames <- paste0("Layer_", 1:length(layers))
                      }
                      self$layer_names=lnames
                      self$layers=layers
                     
                   },
                   
                   zdim = function() { self$layers[[1]]$zdim() },
                   
                   get_zpos = function(zlevel) { self$layers[[1]]$get_zpos() },
                   
                   names = function() { self$layer_names },
                   
                   length = function() { length(self$layers) },
                   
                   render_slice=function(zpos, width=NULL, height=NULL) {
                     
                     sliceList <- lapply(self$layers, function(layer) {
                       layer$render_slice(zpos=zpos, width=width, height=height)
                     })
                     
                     slices <- lapply(sliceList, function(x) x$slice)
                     grobs <- lapply(sliceList, function(x) x$raster)
                     gl <- do.call(gList, grobs)
                     
                     RenderedSliceStack$new(slices=slices, width=width, height=height, 
                                            xbounds=sliceList[[1]]$xbounds, 
                                            ybounds=sliceList[[1]]$ybounds, rasterList=gl)
                     
                     
                   }
                )
)



RenderedSlice <- R6Class("RenderedSlice",
                         portable=TRUE,
                         public = list(
                           slice=NULL,
                           width=NULL, 
                           height=NULL, 
                           xbounds=NULL, 
                           ybounds=NULL, 
                           raster=NULL,
                           
                           initialize = function(slice, width,height, xbounds,  ybounds, raster) {
                             self$slice=slice
                             self$width=width
                             self$height=height
                             self$xbounds=xbounds
                             self$ybounds=ybounds
                             self$raster=raster
                           },
                           
                           draw = function() {
                             grid.newpage()
                             grid.rect(gp=gpar(fill="black"))
                             grid.draw(self$raster)
                           }
                           
                         )
                         
)


RenderedSliceStack <- R6Class("RenderedSliceStack",
                              portable=TRUE,
                              public = list(
                                slices=NULL,
                                width=NULL, 
                                height=NULL, 
                                xbounds=NULL, 
                                ybounds=NULL, 
                                rasterList=NULL,
                                
                                initialize = function(slices, width,height, xbounds,  ybounds, rasterList) {
                                  self$slices=slices
                                  self$width=width
                                  self$height=height
                                  self$xbounds=xbounds
                                  self$ybounds=ybounds
                                  self$rasterList=rasterList
                                },
                                
                                draw = function() {
                                  grid.newpage()
                                  grid.rect(gp=gpar(fill="black"))
                                  grid.draw(self$rasterList)
                                }
                                
                              )
)
                       

