

#' Create an spherical random searchlight iterator
#' 
#' @param mask an volumetric image mask of type \code{\linkS4class{BrainVolume}} 
#'        containing valid searchlight voxel set.
#' @param radius width in mm of spherical searchlight
#' @export
RandomSearchlight <- function(mask, radius) {
  
  done <- array(FALSE, dim(mask))
  
  mask.idx <- which(mask != 0)
  
  grid <- indexToGrid(mask, as.numeric(mask.idx))
  
  prog <- function() { sum(done)/length(mask.idx) }
  
  nextEl <- function() {
    if (!all(done[mask.idx])) {
      center <- .resample(which(!done[mask.idx]), 1)
      search <- RegionSphere(mask, grid[center,], radius, nonzero=TRUE) 
      vox <- coords(search)
      vox <- vox[!done[vox],,drop=FALSE]
      #done[center] <<- TRUE
      done[vox] <<- TRUE
      attr(vox, "center") <- grid[center,]
      attr(vox, "center.index") <- mask.idx[center]
      attr(vox, "indices") <- gridToIndex(mask, vox)
      attr(vox, "length") <- nrow(vox)
      vox
      
    } else {
      stop('StopIteration')
    }
  }
  obj <- list(nextElem=nextEl, progress=prog)
  class(obj) <- c("RandomSearchlight", 'abstractiter', 'iter')
  obj
}

#' Create a spherical searchlight iterator that samples regions from within a mask.
#' 
#' searchlight centers are sampled without replacement, but the same surround voxel can belong to multiple searchlight samples.
#' 
#' @param mask an image volume containing valid central voxels for roving searchlight
#' @param radius in mm of spherical searchlight (can be a vector which is randomly sampled)
#' @param iter the total number of searchlights to sample (default is 100).
#' @export
BootstrapSearchlight <- function(mask, radius=8, iter=100) {
  mask.idx <- which(mask != 0)
  grid <- indexToGrid(mask, mask.idx)
  index <- 0
  
  sample.idx <- sample(1:nrow(grid), iter)
  
  prog <- function() { index/length(mask.idx) }
  
  nextEl <- function() {
    if (index <= iter & length(sample.idx) > 0) { 
      index <<- index + 1
      
      cenidx <- sample.idx[1]
      sample.idx <<- sample.idx[-1]
      
      search <- RegionSphere(mask, grid[cenidx,], sample(radius,1), nonzero=TRUE) 
      vox <- search@coords
      attr(vox, "center") <- grid[cenidx,]
      attr(vox, "center.index") <- mask.idx[cenidx]
      attr(ret, "indices") <-  gridToIndex(mask, vox)
      attr(ret, "length") <- nrow(vox)
      vox
    } else {
      stop('StopIteration')
    }
  }
  
  obj <- list(nextElem=nextEl, progress=prog)
  class(obj) <- c("BootstrapSearchlight", 'abstractiter', 'iter')
  obj
  
}




#' Create an exhaustive searchlight iterator
#' 
#' @param mask an image volume containing valid central voxels for roving searchlight
#' @param radius in mm of spherical searchlight
#' @return an \code{iter} class 
#' @importFrom rflann RadiusSearch
#' @export
Searchlight <- function(mask, radius, eager=FALSE) {
  mask.idx <- which(mask != 0)
  grid <- indexToGrid(mask, mask.idx)
  index <- 0
  
  prog <- function() { index/length(mask.idx) }
  
  if (eager) {
    cds <- indexToCoord(mask, as.numeric(mask.idx))
    nabelist <- rflann::RadiusSearch(cds, cds, radius^2, max_neighbour=1000, build="kdtree")
  }
  
  nextEl <- if (!eager) {
    function() {
      if (index < nrow(grid)) { 
        index <<- index + 1
        search <- RegionSphere(mask, grid[index,], radius, nonzero=TRUE) 
        vox <- coords(search)
        attr(vox, "center") <- grid[index,]
        attr(vox, "center.index") <- mask.idx[index]
        attr(vox, "length") <- nrow(vox)
        vox
      } else {
        stop('StopIteration')
      }
    }
  } else {
    function() {
      if (index < nrow(grid)) { 
        index <<- index + 1
        vox <- grid[nabelist$indices[[index]],]
        attr(vox, "center") <- grid[index,]
        attr(vox, "center.index") <- mask.idx[index]
        attr(vox, "length") <- nrow(vox)
        vox
      } else {
        stop('StopIteration')
      }
    }
  }
  
  obj <- list(nextElem=nextEl, progress=prog)
  class(obj) <- c("Searchlight", 'abstractiter', 'iter')
  obj
  
}

#' Create a clustered Searchlight iterator
#' 
#' @param mask an image volume containing valid central voxels for roving searchlight
#' @param csize the number of clusters
#' @return an \code{iter} class 
#' @export
ClusteredSearchlight <- function(mask, csize) {
  mask.idx <- which(mask != 0)
  grid <- indexToCoord(mask, as.numeric(mask.idx))
  vox <- indexToGrid(mask, as.numeric(mask.idx))
  
  kres <- kmeans(grid, centers=csize, iter.max=500)
  
  index_list <- split(1:length(mask.idx), kres$cluster)
  
  index <- 0
  
  prog <- function() { index/csize }
  
  nextEl <- function() {
    if (index < csize) { 
      index <<- index + 1
      ind <- index_list[[index]]
      ret <- vox[index_list[[index]],]
      attr(ret, "mask_indices") <- mask.idx[ind]
      attr(ret, "indices") <- ind
      attr(ret, "length") <- length(ind)
      ret
    } else {
      stop('StopIteration')
    }
  }
  
  obj <- list(nextElem=nextEl, progress=prog, index_list=index_list, clusters=kres$cluster)
  class(obj) <- c("Searchlight", 'abstractiter', 'iter')
  obj
  
}