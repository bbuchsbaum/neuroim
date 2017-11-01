#' @include AllClass.R
{}
#' @include AllGeneric.R
{}
#' @include common.R
{}
#' @include SparseBrainVector.R
{}




.BrainVectorFromMatrix <- function(data, space) {
	nvols <- dim(space)[4]
	nelements <-  prod(dim(space)[1:3])
	
	if ( (dim(data)[1] == nvols) && (dim(data)[2] == nelements) ) {
		#fourth dimension is rows
		DenseBrainVector(t(data), space)        
	} else if ((dim(data)[2] == nvols) && (dim(data)[1] == nelements )) {
		#fourth dimension is columns
		DenseBrainVector(data, space=space)
	} else {
		stop(paste("illegal matrix dimension ", dim(data)))
	}
}

#' makeVector
#' 
#' Construct a \code{\linkS4class{BrainVector}} instance, using default (dense) implementation
#' @param data a four-dimensional \code{array}
#' @param refdata an instance of class \code{\linkS4class{BrainVector}} or \code{\linkS4class{BrainVolume}} containing the reference space for the new vector.
#' @param label a \code{character} string
#' @param source an instance of class \code{\linkS4class{BrainSource}}
#' @return \code{\linkS4class{DenseBrainVector}} instance 
#' @export makeVector
makeVector <- function(data, refdata, source=NULL, label="") {	
	stopifnot(length(dim(refdata)) == 4)
	rspace <- if (ndim(space(refdata)) == 4) {
		dropDim(space(refdata))
	} else if (ndim(space(refdata)) == 3) {
		space(refdata)
	} else {
		stop("refdata must have 3 or 4 dimensions")
	}

	DenseBrainVector(data,addDim(rspace, dim(data)[4]),source, label)
	
}


#' BrainVector
#' 
#' constructor function for virtual class \code{\linkS4class{BrainVector}}
#' 
#' @param data the image data which can be a \code{matrix}, a 4d \code{array}, or a list of \code{BrainVolumes}. 
#'        If the latter, the geometric space of the data \code{BrainSpace} will be inferred from the constituent volumes, 
#'        which must all be identical.
#' @param space a \code{\linkS4class{BrainSpace}} object. Does not ned to be included if \code{data} argument is a list of \code{BrainVolumes}
#' @param mask an optional \code{array} of type \code{logical}
#' @param source an optional \code{\linkS4class{BrainSource}} object
#' @param label a label of type \code{character} 
#' @return a concrete instance of \code{\linkS4class{BrainVector}} class. 
#' If \code{mask} is provided then \code{\linkS4class{SparseBrainVector}}, otherwise \code{\linkS4class{DenseBrainVector}}
#' @export BrainVector
#' @rdname BrainVector-class
BrainVector <- function(data, space=NULL, mask=NULL, source=NULL, label="") {
  if (is.list(data)) {
    space <- space(data[[1]])
    space <- addDim(space, length(data))
    data <- do.call(cbind, lapply(data, function(x) as.vector(x)))
      
  }
    
	if (prod(dim(space)) != length(data)) {
		stop("dimensions of data argument do not match dimensions of space argument")
	}
	if (is.null(mask)) {
		DenseBrainVector(data,space, source, label)
	} else {
		SparseBrainVector(data,space,mask,source, label)
	}
	
}


#' DenseBrainVector
#' 
#' constructor function for class \code{\linkS4class{DenseBrainVector}}
#' 
#' @param data a 4-dimensional \code{array} or a 2-dimension \code{matrix} that is either nvoxels by ntime-points or ntime-points by nvoxels
#' @param space a \code{\linkS4class{BrainSpace}} object
#' @param source an optional \code{\linkS4class{BrainSource}} object
#' @param label a label of type \code{character} 
#' @return \code{\linkS4class{DenseBrainVector}} instance 
#' @export DenseBrainVector
#' @rdname DenseBrainVector-class
DenseBrainVector <- function(data, space, source=NULL, label="") {
	
	if (is.matrix(data)) {
		splen <- prod(dim(space)[1:3])
		data <- if (ncol(data) == splen) {
			t(data)
		} else if (nrow(data) == splen) {
			data
		}
    
    if (length(dim(space)) == 3) {
      ## add 4th dim to space arg
      space <- addDim(space, ncol(data))
    }

		dim(data) <- dim(space)
	}
  
  
	
	if (is.null(source)) {
		meta <- BrainMetaInfo(dim(data), spacing(space), origin(space), "FLOAT", label)
		source <- new("BrainSource", metaInfo=meta)	
	}
	
	new("DenseBrainVector", .Data=data, source=source, space=space)
	
}




#' loadData
#' @return an instance of class \code{\linkS4class{BrainVector}} 
#' @param mmap use memory-mapped file
#' @importFrom RNifti readNifti
#' @rdname loadData-methods
setMethod(f="loadData", signature=c("BrainVectorSource"), 
		def=function(x, mmap=FALSE) {		
			
			meta <- x@metaInfo
      
     
			#if (mmap && (.Platform$endian != meta@endian)) {
			#	message("cannot create memory mapped file when image endianness does not equal OS endianess")
			#  mmap <- FALSE
			#}
			
			if (mmap && neuroim:::.isExtension(meta@dataFile, ".gz")) {
				warning("cannot memory map to a gzipped file. ")		
			  mmap <- FALSE
			}
						
			stopifnot(length(meta@Dim) == 4)
						
			nels <- prod(meta@Dim[1:4]) 		
			ind <- x@indices
	
			if (mmap) {
			  mappedData <- .makeMMap(meta)
			  arr <- array(mappedData, c(meta@Dim[1:4]))
			} else {
			  
			  ## use RNifti
			  arr <- RNifti::readNifti(meta@dataFile)
			  
			  #### old R-level File IO
			  #reader <- dataReader(meta, 0)	
			  #arr <- array(readElements(reader, nels), c(meta@Dim[1:4]))
			  #close(reader)
			}
			
			## bit of a hack to deal with scale factors
			if (.hasSlot(meta, "slope")) {
        
        if (meta@slope != 0) {		  
			    arr <- arr* meta@slope
        }
			}
      
      bspace <- BrainSpace(c(meta@Dim[1:3], length(ind)),meta@spacing, meta@origin, meta@spatialAxes, trans(meta))
			DenseBrainVector(arr[,,,ind,drop=FALSE], bspace, x)
			
		})



#' BrainVectorSource
#' 
#' Construct a \code{\linkS4class{BrainVectorSource}} object
#' 
#' @param fileName name of the 4-dimensional image file
#' @param indices the subset of integer volume indices to load -- if \code{NULL} then all volumes will be loaded
#' @param mask image volume indicating the subset of voxels that will be loaded. If provided, function returns \code{\linkS4class{SparseBrainVectorSource}}
#' @return a instance deriving from \code{\linkS4class{BrainVectorSource}}
#' 
#' @details If a \code{mask} is supplied then it should be a \code{\linkS4class{LogicalBrainVolume}} or \code{\linkS4class{BrainVolume}} instance. If the latter, then the mask will be defined by nonzero elements of the volume.
#'
#' @rdname BrainVectorSource
#' @importFrom assertthat assert_that
#' @export 
BrainVectorSource <- function(fileName, indices=NULL, mask=NULL) {
	assert_that(is.character(fileName))
	assert_that(file.exists(fileName))
	
	
	metaInfo <- readHeader(fileName)
	
	if (!is.null(indices) && max(indices) > 1) {
	  assert_that(length(dim(metaInfo)) == 4)
	  assert_that(max(indices) <= dim(metaInfo)[4])
	  assert_that(min(indices) > 0)
	}
	
  if (length(metaInfo@Dim) == 2) {
    stop(paste("cannot create BrainVector with only two dimensions: ", paste(metaInfo@Dim, collapse=" ")))  
  }
	
  if ( length(metaInfo@Dim) == 3) {
		indices <- 1
    metaInfo@Dim <- c(metaInfo@Dim,1)
	} else if (length(metaInfo@Dim) == 4 && is.null(indices)) {
		indices=seq(1, metaInfo@Dim[4])
	}
	

	if (is.null(mask)) {
		new("BrainVectorSource", metaInfo=metaInfo, indices=as.integer(indices))		
	} else {
		SparseBrainVectorSource(metaInfo, as.integer(indices), mask)		
	}
	
}


#' Get length of \code{BrainVector}. This is the numbe rof volumes in the volume vector (e.g. the 4th image dimension)
#' 
#' @export
#' @rdname length-methods
setMethod("length", signature=c("BrainVector"),
		def=function(x) {
			dim(x)[4]
		})



#' loadVolumeList
#' 
#' load a list of image volumes and return a \code{\linkS4class{BrainVector}} instance
#' 
#' @param fileNames a list of files to load
#' @param mask an optional mask indicating subset of voxels to load
#' @return an instance of class \code{\linkS4class{BrainVector}}
#' @export loadVolumeList
loadVolumeList <- function(fileNames, mask=NULL) {
	stopifnot(all(sapply(fileNames, file.exists)))
	metaInfo <- lapply(fileNames, readHeader)
	
	dims <- do.call(rbind, lapply(metaInfo, dim))
	if (!all(sapply(1:nrow(dims), function(i) all.equal(dims[1,], dims[i,])))) {
		stop("list of volumes must all have same dimensions")
	}
	
	if (!all(apply(dims, 1, length) == 3)) {
		stop("all volumes in list must have dim = 3")
	}
	
	nvols <- length(fileNames)	
	sourceList <- lapply(fileNames, function(fname) {
		BrainVolumeSource(fname, 1)
	})

	vols <- lapply(sourceList, loadData)
	if (is.null(mask)) {
		mat <- do.call(cbind, vols)
		dspace <- addDim(space(vols[[1]]), length(vols))	
		DenseBrainVector(mat, dspace, label=sapply(metaInfo, function(m) m@label))
	} else {
		mat <- do.call(cbind, vols)
		dspace <- addDim(space(vols[[1]]), length(vols))
		if (is.vector(mask)) {
			## mask supplied as index vector, convert to logical
			M <- array(logical(prod(dim(dspace)[1:3])), dim(dspace)[1:3])
			M[mask] <- TRUE
			mask <- M
		} else {
			mask <- as.logical(mask)
		}
		
		
		SparseBrainVector(mat[mask,], dspace, mask=mask, label=sapply(metaInfo, function(m) m@label))
		
	}
}


setAs("DenseBrainVector", "array", function(from) from@.Data)

setAs("BrainVector", "array", function(from) from[,,,])

#' show a \code{BrainVectorSource}
#' @param object the object
#' @export
setMethod(f="show",
		signature=signature(object="BrainVectorSource"),
		def=function(object) {
			cat("an instance of class",  class(object), "\n\n")
			cat("   indices: ", object@indices, "\n\n")
			cat("   metaInfo: \n")
			show(object@metaInfo)
			cat("\n\n")
			
		})




#' show a \code{BrainVector}
#' @param object the object
#' @export
setMethod(f="show", signature=signature("BrainVector"),
          def=function(object) {
            sp <- space(object)
            cat(class(object), "\n")
            cat("  Type           :", class(object), "\n")
            cat("  Dimension      :", dim(object), "\n")
            cat("  Spacing        :", paste(paste(sp@spacing[1:(length(sp@spacing)-1)], " X ", collapse=" "), 
                                            sp@spacing[length(sp@spacing)], "\n"))
            cat("  Origin         :", paste(paste(sp@origin[1:(length(sp@origin)-1)], " X ", collapse=" "), 
                                            sp@origin[length(sp@origin)], "\n"))
            cat("  Axes           :", paste(sp@axes@i@axis, sp@axes@j@axis,sp@axes@k@axis), "\n")
            cat("  Coordinate Transform :", zapsmall(sp@trans), "\n")
            
          }
)


#' @rdname eachVolume-methods
#' @export
setMethod(f="eachVolume", signature=signature(x="BrainVector", FUN="function", withIndex="missing", mask="missing"),
		def=function(x, FUN, withIndex, mask, ...) {
			lapply(1:(dim(x)[4]), function(tt) FUN(x[,,,tt], ...))				
		})


#' @rdname eachVolume-methods
#' @export
setMethod(f="eachVolume", signature=signature(x="BrainVector", FUN="function", withIndex="missing", mask="BrainVolume"),
          def=function(x, FUN, withIndex, mask, ...) {
            mask.idx <- which(mask > 0)
            lapply(1:(dim(x)[4]), function(tt) {
              vals <- x[,,,tt]
              FUN(vals[mask.idx], ...)
            })
          })

 
#' @rdname eachVolume-methods
#' @export
setMethod(f="eachVolume", signature=signature(x="BrainVector", FUN="function", withIndex="missing", mask="missing"),
          def=function(x, FUN, withIndex, mask, ...) {   
            lapply(1:(dim(x)[4]), function(tt) {
              vals <- x[,,,tt]
              FUN(vals, ...)
            })
          })



#' @rdname eachVolume-methods
#' @export
setMethod(f="eachVolume", signature=signature(x="BrainBucket", FUN="function", withIndex="missing",mask="missing"),
		def=function(x, FUN, withIndex, ...) {
			lapply(1:(dim(x)[4]), function(tt) FUN(x[[tt]], ...))				
		})



#' @rdname eachVolume-methods
#' @export
setMethod("eachVolume", signature=signature(x="BrainBucket", FUN="function", withIndex="logical"),
		def=function(x, FUN, withIndex, ...) {
			lapply(1:(dim(x)[4]), function(tt) {					
						vol <- x[[tt]]
						if (withIndex) FUN(vol,tt,...) else FUN(vol,...)
					})
		})




#' @rdname eachVolume-methods
#' @export
setMethod("eachVolume", signature=signature(x="BrainVector", FUN="function", withIndex="logical"),
		def=function(x, FUN, withIndex, ...) {
			lapply(1:(dim(x)[4]), function(tt) {					
						vol <- x[,,,tt]
						if (withIndex) FUN(vol,tt,...) else FUN(vol,...)
					})
		})



#' @rdname subVector-methods
#' @export
setMethod(f="subVector", signature=signature(x="DenseBrainVector", i="numeric"),
          def=function(x, i) {
            assertthat::assert_that(max(i) <= dim(x)[4])
            xs <- space(x)
            dat <- x[,,,i]
            
            newdim <- c(dim(x)[1:3], length(i))
            bspace <- BrainSpace(newdim, spacing=spacing(xs), origin=origin(xs), axes(xs), trans(xs))
            DenseBrainVector(dat, bspace)
          })



#' @rdname BrainVector-methods
#' @param i the volume index
#' @export
setMethod(f="[[", signature=signature(x="BrainVector", i="numeric"),
          def = function(x, i) {
            xs <- space(x)
            dat <- x[,,,i]
            newdim <- dim(x)[1:3]
            bspace <- BrainSpace(newdim, spacing=spacing(xs), origin=origin(xs), axes(xs), trans(xs))
            DenseBrainVolume(dat, bspace)
          })
          
          
          
#' @rdname takeVolume-methods
#' @param merge concatenate extracted volumes
#' @export
setMethod(f="takeVolume", signature=signature(x="BrainVector", i="numeric"),
		def=function(x, i, merge=FALSE) {
			## TODO this is VERY slow
			## TODO should be renamed "volSlice"
		  
			xs <- space(x)
			bspace <- BrainSpace(dim(x)[1:3], spacing=spacing(xs), origin=origin(xs), axes(xs), trans(xs))
			
			makevol <- function(i) {				
				BrainVolume(x[,,,i], bspace)
			}
			
			res <- lapply(i, makevol)
			
			if (length(res) > 1 && merge) {
				res <- do.call("concat", res)				
			}
			
			if (length(res) == 1) {
			  ## TODO should be consistent, e.g. always return list
				res[[1]]
			} else {
				res
			}											
		})



#' @rdname eachSeries-methods
# @importFrom purrr array_branch map
#' @export
setMethod(f="eachSeries", signature=signature(x="DenseBrainVector", FUN="function", withIndex="missing"),
          def=function(x, FUN, withIndex=FALSE, ...) {
            #stop()
            #map(array_branch(x, 1:3), FUN)
            callGeneric(x,FUN, withIndex,...)
          })
          
            

#' @rdname eachSeries-methods
#' @export
setMethod(f="eachSeries", signature=signature(x="BrainVector", FUN="function", withIndex="missing"),
		def=function(x, FUN, withIndex=FALSE, ...) {
			
			NX <- dim(x)[1]
			NY <- dim(x)[2]
			NZ <- dim(x)[3]
			ret <- vector("list", prod(NX, NY, NZ))
			index <- 1
			for (i in 1:NZ) {
				for (j in 1:NY) {
					for (k in 1:NX) {
						ret[[index]] <- FUN(x[k,j,i,])
						index <- index+1
					}
				}
			}
			
			ret
			
		})




#' loadVector
#' 
#' load an image volume from a file
#' 
#' @param fileName the name of the file to load
#' @param indices the indices of the sub-volumes to load (e.g. if the file is 4-dimensional)
#' @param mask a mask defining the spatial elements to load 
#' @param mmap memory mapping if possible
#' @return an \code{\linkS4class{BrainVector}} object
#' @export 
loadVector  <- function(fileName, indices=NULL, mask=NULL, mmap=FALSE) {
	src <- BrainVectorSource(fileName, indices, mask)
	loadData(src,mmap)
}



#' @rdname concat-methods
#' @export
setMethod(f="concat", signature=signature(x="BrainVector", y="BrainVolume"),
		def=function(x,y, ...) {
			.concat4D(x,y,...)			
		})


#' @rdname concat-methods
#' @export
setMethod(f="concat", signature=signature(x="BrainVolume", y="BrainVector"),
  def=function(x,y, ...) {
    .concat4D(x,y,...)			
  })

#' @rdname scaleSeries-methods
#' @export
setMethod(f="scaleSeries", signature=signature(x="BrainVector", center="logical", scale="logical"),
          def=function(x, center, scale) {
            M <- as.matrix(x)
            Ms <- scale(t(M), center, scale)
            BrainVector(Ms, space(x))
             		
          })

#' @rdname scaleSeries-methods
#' @export
setMethod(f="scaleSeries", signature=signature(x="BrainVector", center="missing", scale="logical"),
          def=function(x, center, scale) {
            callGeneric(x, TRUE, scale)
          })


#' @rdname scaleSeries-methods
#' @export
setMethod(f="scaleSeries", signature=signature(x="BrainVector", center="missing", scale="missing"),
          def=function(x, center, scale) {
            callGeneric(x, TRUE, TRUE)
          })

#' @rdname scaleSeries-methods
#' @export
setMethod(f="scaleSeries", signature=signature(x="BrainVector", center="logical", scale="missing"),
          def=function(x, center, scale) {
            callGeneric(x, center, TRUE)
          })

#' @export
#' @rdname splitScale-methods
#' @importFrom abind abind
setMethod(f="splitScale", signature=signature(x = "DenseBrainVector", f="factor", center="missing", scale="missing"),
          def=function(x, f) {
            callGeneric(x, f, TRUE, TRUE)
            
          })

#' @export
#' @rdname splitScale-methods
#' @importFrom abind abind
setMethod(f="splitScale", signature=signature(x = "DenseBrainVector", f="factor", center="logical", scale="missing"),
          def=function(x, f, center) {
            callGeneric(x, f, center, TRUE)
            
          })

#' @export
#' @rdname splitScale-methods
#' @importFrom abind abind
setMethod(f="splitScale", signature=signature(x = "DenseBrainVector", f="factor", center="logical", scale="logical"),
          def=function(x, f, center, scale) {
            m <- callGeneric(t(as.matrix(x)), f, center, scale)
            BrainVector(m, space(x))
          })



#' @rdname concat-methods
#' @export
setMethod(f="concat", signature=signature(x="BrainVector", y="BrainVector"),
		def=function(x,y,...) {
			.concat4D(x,y,...)
		})


#' @rdname series-methods
#' @export
setMethod("series", signature(x="BrainVector", i="matrix"),
		def=function(x,i) {
			assertthat::assert_that(ncol(i) == 3)
		  
		  # old, slower method
			#apply(i, 1, function(i) x[i[1], i[2], i[3],])
		  
		  d4 <- dim(x)[4]
		  expanded <- i[rep(1:nrow(i), each=d4),]
		  expanded <- cbind(expanded, 1:4)
	    vec <- x[expanded]
	    matrix(vec, d4, nrow(i))
		})


#' @rdname series-methods
#' @export
setMethod("series_roi", signature(x="BrainVector", i="matrix"),
          def=function(x,i) {
            mat <- series(x, i)
            ROIVector(space(x), coords=i, data=mat)
            
          })



#' @rdname series-methods
#' @export
setMethod("series", signature(x="BrainVector", i="ROIVolume"),
          def=function(x,i) {
            grid <- coords(i)
            callGeneric(x, grid)
          })


#' @rdname series-methods
#' @export
setMethod("series_roi", signature(x="BrainVector", i="ROIVolume"),
          def=function(x,i) {
            rvol <- series(x, i)
            ROIVector(space(x), coords=coords(rvol), data=as.matrix(values(rvol)))
          })
          

#' @rdname series-methods
#' @export
setMethod("series", signature(x="BrainVector", i="LogicalBrainVolume"),
          def=function(x,i) {
            assertthat::assert_that(all.equal(dim(x)[1:3], dim(i)[1:3]))
            idx <- which(i == TRUE)
            assertthat::assert_that(length(idx) > 0)
            
            grid <- indexToGrid(i, idx)
            callGeneric(x, grid)
            
          })

#' @rdname series-methods
#' @export
setMethod("series_roi", signature(x="BrainVector", i="LogicalBrainVolume"),
          def=function(x,i) {
            mat <- as.matrix(series(x, i))
            ROIVector(space(x), coords=indexToGrid(which(i == TRUE), idx), data=as.matrix(mat))
            
          })

#' @rdname series-methods
#' @export
setMethod("series", signature(x="BrainVector", i="numeric"),
		def=function(x, i, j, k) {	
			if (missing(j) && missing(k)) {
				vdim <- dim(x)[1:3]
				mat <- arrayInd(i, vdim)
				apply(mat, 1, function(i) x[i[1], i[2], i[3],])			
			} else {
				x[i,j,k,]	
			}
		})


#' @rdname series-methods
#' @export
setMethod("series_roi", signature(x="BrainVector", i="numeric"),
          def=function(x, i, j, k) {	
            mat <- if (missing(j) && missing(k)) {
              vdim <- dim(x)[1:3]
              vox <- arrayInd(i, vdim)
              callGeneric(x, vox)
            } else if (missing(i) || missing(j) || missing(k)) {
              stop("series_roi: must provide either 1D 'i' or 3D ('i', 'k', 'j') vector indices")
            }
            else {
              vox <- cbind(i,j,k)
              callGeneric(x, as.matrix(vox))
            }
            
            
          })



#' @describeIn seriesIter get a series iterator for a \code{\linkS4class{BrainVector}} instance
#' @export
setMethod(f="seriesIter", signature=signature(x="BrainVector"), 
		def=function(x) {
			len <- prod(dim(x)[1:3])
			vdim <- dim(x)[1:3]
			i <- 1
			nextEl <- function() {
				if (i <= len) {
					vox <- .indexToGrid(i, vdim)
					i <<- i + 1
					x[vox[1], vox[2], vox[3],]
					
				} else {
					stop("StopIteration") 
				}		
			}
			
			hasNx <- function() {
				i <= len
			}
			
			obj <- list(nextElem = nextEl, hasNext=hasNx) 
			class(obj) <- c("seriesIter", "abstractiter", "iter") 
			obj
			
			
		})


#' @export
setAs(from="DenseBrainVector", to="matrix",
		function(from) {
			data <- from@.Data
			dm <- dim(data)
			d123 <- prod(dm[1:3])
			d4 <- dm[4]
			
			dim(data) <- c(d123,d4)
			return(data)
			
		})


#' convert a \code{BrainVector} to \code{list} of volumes. 
#' 
#' @rdname as.list-methods
#' @param x the object
#' @export 
setMethod(f="as.list", signature=signature(x = "BrainVector"), def=function(x) {
  out = list()
  for (i in 1:dim(x)[4]) {
    out[[i]] <- takeVolume(x,i)
  }
  
  out
})


#' convert a \code{DenseBrainVector} to a matrix
#' 
#' @rdname as.matrix-methods
#' @param x the object
#' @export 
setMethod(f="as.matrix", signature=signature(x = "DenseBrainVector"), def=function(x) {
			as(x, "matrix")						
		})

 
#' @rdname as.sparse-methods
#' @export
setMethod(f="as.sparse", signature=signature(x="DenseBrainVector", mask="LogicalBrainVolume"),
          def=function(x, mask) {
            assert_that(all(dim(x)[1:3] == dim(mask)))
            assert_that(all(spacing(space(x)) == spacing(space(mask))))
            
            vdim <- dim(x)[1:3]
            dat <- as.matrix(x)[mask == TRUE,]
            bvec <- SparseBrainVector(dat, space(x), mask)
            
          })

 
#' @rdname as.sparse-methods
setMethod(f="as.sparse", signature=signature(x="DenseBrainVector", mask="numeric"),
		def=function(x, mask) {
			vdim <- dim(x)[1:3]
			m <- array(0, vdim)
			m[mask] <- TRUE
			
			logivol <- LogicalBrainVolume(m, dropDim(space(x)))
			
			dat <- as.matrix(x)[mask,]
			
			bvec <- SparseBrainVector(dat, space(x), logivol)
			
		})


#' @export
#' @rdname writeVector-methods
setMethod(f="writeVector",signature=signature(x="BrainVector", fileName="character", format="missing", dataType="missing"),
		def=function(x, fileName) {
			write.nifti.vector(x, fileName)           
		})


#' @export 
#' @rdname writeVector-methods
setMethod(f="writeVector",signature=signature(x="BrainVector", fileName="character", format="character", dataType="missing"),
		def=function(x, fileName, format) {
			if (toupper(format) == "NIFTI" || toupper(format) == "NIFTI1" || toupper(format) == "NIFTI-1") {
				callGeneric(x, fileName)
			} else {
				stop(paste("sorry, cannot write format: ", format))
			}      
		})


#' @export writeVector
#' @rdname writeVector-methods
#' @aliases writeVector,BrainVector,character,missing,character,ANY-method
setMethod(f="writeVector",signature=signature(x="BrainVector", fileName="character", format="missing", dataType="character"),
		def=function(x, fileName, dataType) {
			write.nifti.vector(x, fileName, dataType)   
			
		})


