#' @include AllClass.R
{}
#' @include AllGeneric.R
{}


#' names
#' @rdname names-methods
#' @export
#' @param x the object to get \code{names} of
setMethod("names", signature=c("BrainBucketSource"),
          def=function(x) {
            x@metaInfo@label[x@indices]
          })


#' @rdname names-methods
#' @export
setMethod("names", signature=c("BrainBucket"),
          def=function(x) {
            x@labels
          })



#' loadBucket
#' 
#' load a BrainBucket object from file
#' 
#' @param fileName the name of the file to load
#' @param pattern optional regular expression used to filter the sub-volumes using associated labels
#' @param indices optional set of sub-volume indices to load
#' @export loadBucket
loadBucket <- function(fileName, pattern=NULL, indices=NULL) {
  bsource <- BrainBucketSource(fileName, pattern, indices)
  
  meta <- bsource@metaInfo	
  labels <- meta@label
  idx <- bsource@indices
  
  D <- c(meta@Dim[1:3], length(idx))
  bspace <- BrainSpace(D, meta@spacing, meta@origin, meta@spatialAxes)
  
  ## TODO chceck: does not actually provide data ....
  buck <- new("BrainBucket", source=bsource, space=bspace, labels=labels[idx])
}

#' BrainBucketSource
#' 
#' Constructor function for \code{\linkS4class{BrainBucketSource}} class
#' 
#' @param fileName the name of the bucket file
#' @param pattern optional regular expression used to filter the sub-volumes using associated labels
#' @param indices optional set of sub-volume indices to load
#' @export BrainBucketSource
#' @rdname BrainBucketSource-class
BrainBucketSource <- function(fileName, pattern=NULL, indices=NULL) {
  stopifnot(is.character(fileName))
  stopifnot(file.exists(fileName))
  
  
  metaInfo <- readHeader(fileName)
  
  labels <- metaInfo@label
  nvols <- length(labels)	
  
  if (is.null(indices)) {
    indices <- seq_along(labels)
  } else {
    stopifnot(all(indices >0 & indices < nvols))
  }
  
  if (!is.null(pattern)) {
    idx <- grep(pattern, labels)
    
    if (length(idx) < 1) {
      stop(paste("pattern: ", pattern, "does not match any volume labels"))
    }
    
    indices <- intersect(idx, indices)
  }
  
  
  sourceList <- lapply(indices, function(i) { new("BrainVolumeSource", metaInfo=metaInfo, index=as.integer(i)) })	
  new("BrainBucketSource", metaInfo=metaInfo, indices=indices, sourceList=sourceList, cache=new.env(hash=TRUE))	
}



#' BrainBucket
#' 
#' Constructor function for \code{\linkS4class{BrainBucket}} class
#' 
#' @param volumeList a named list of \code{\linkS4class{BrainVolume}} instances
#' @export BrainBucket
#' @rdname BrainBucket-class
#' @examples 
#' vol1 <- BrainVolume(rnorm(24*24*24), BrainSpace(c(24,24,24), c(1,1,1)))
#' vol2 <- BrainVolume(rnorm(24*24*24), BrainSpace(c(24,24,24), c(1,1,1)))
#' vol3 <- BrainVolume(rnorm(24*24*24), BrainSpace(c(24,24,24), c(1,1,1)))
#' vlist <- list(vol1,vol2,vol3)
#' names(vlist) <- paste0("V", 1:3)
#' bucket <- BrainBucket(vlist)
#' all.equal(dim(bucket[[1]]), dim(vol1))
#' @return an instance of class \code{\linkS4class{BrainBucket}}
#' 
BrainBucket <- function(volumeList) {
  
  isvol <- sapply(volumeList, function(vol) inherits(vol, "BrainVolume"))
  
  if (length(volumeList) < 1 || any(!isvol)){
    stop("BrainBucket: 'volumeList' must be a nonempty list of instances of or extending 'BrainVolume'")
  }
  
  vnames <- names(volumeList)
  if (is.null(vnames)) {
    vnames <- paste0("V", 1:length(volumeList))
  } else if (any(vnames == "")) {
    whichEmpty <- which(vnames == "")
    vnames[whichEmpty] <- paste0("V", whichEmpty)
  }
  
  sp <- space(volumeList[[1]])
  D <- c(dim(sp), length(volumeList))
  
  meta <- BrainMetaInfo(D, spacing(sp), origin=origin(sp), dataType="FLOAT", label=vnames, spatialAxes=axes(sp))
  
  sourceList <- lapply(1:length(volumeList), function(i) { 
    volumeList[[i]]@source
    
  })
  
  bsource <- new("BrainBucketSource", metaInfo=meta, indices=1:length(volumeList), sourceList=sourceList, cache=new.env(hash=TRUE))
  
  new("BrainBucket", source=bsource,space=addDim(sp, length(volumeList)), labels=vnames, data=volumeList)
}


#' extract labeled volume from \code{BrainBucket}
#' @param x the object
#' @param i the first index
setMethod(f="[[", signature=signature(x="BrainBucket", i = "index", j = "missing"),
          def=function(x, i) {
            x@data[[i]]
            #loadData(x@source, i)
          })


#' extract labeled volume from \code{BrainBucket}
#' @export
#' @param x the object
#' @param i first index
setMethod(f="[", signature=signature(x="BrainBucket", i = "index", j = "missing"),
          def=function(x, i) {
            x@data[i]
            #loadData(x@source, i)
          })

#' Load data from a \code{\linkS4class{BrainBucketSource}}
#' 
#' @param key the name or index of the bucket to load
#' @return an instance of class \code{\linkS4class{BrainVolume}} 
#' @rdname loadData-methods
setMethod(f="loadData", signature=signature("BrainBucketSource"), 
          def=function(x, key) {
            
            if (is.numeric(key)) {
              labs <- names(x)
              if (any(key < 1) || any(key > length(labs))) {
                stop(paste("illegal index: ", key))
              }
              
              key <- labs[key]							
            }
            
            
            ret <- lapply(key, function(k) {				
              haskey <- exists(k, envir=x@cache, inherits=FALSE)
              if (!haskey) {
                idx <- which(names(x) == k) 
                vol <- loadData(x@sourceList[[idx]])
                assign(k, vol, envir=x@cache)
              } else {					
                vol <- get(k, envir=x@cache, inherits=FALSE)
              }		
              attr(vol, "label") <- k
              vol
            })
            
            if (length(ret) == 1) {
              ret[[1]]
            } else {
              ret
            }
            
          })


