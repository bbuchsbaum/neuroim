
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
                                     view="LPI", zero_col="#000000", alpha=1, desc="") {
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
               
               render_slice=function(zpos, width=NULL, height=NULL) {
                
                 slice <- slice(self$vol, zpos, self$view_space, self$view_axes)
                 browser()
                 bds <- permMat(space(vol)) %*% bounds(self$view_space)
                
                 dnum1 <- which_dim(self$view_space, self$view_space@axes@i)
                 dnum2 <- which_dim(self$view_space, self$view_space@axes@j)
                 
                 bds <- bds[c(dnum1, dnum2),]
                 
                 
                 
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
                 }
                     
                 grob <- render(slice, width, height, 
                                colmap=self$color_map, 
                                zero.col=self$zero_col, 
                                alpha=self$alpha, 
                                units="points")
                 
                 new("RenderedSlice", slice=slice, width=width, height=height, xbounds=bds[1,], ybounds=bds[2,], raster=grob)
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
                     
                   })
)


