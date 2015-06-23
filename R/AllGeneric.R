roxygen <- function() NULL

#' Generic function to print an object 
#' @param x the object to print
#' @param ... additional arguments
#' @export 
#' @rdname print-methods
setGeneric(name="print", def=function(x, ...) standardGeneric("print"))

#' Generic function to extract data values of object 
#' @param x the object to get values from
#' @export 
#' @rdname values-methods
setGeneric(name="values", def=function(x, ...) standardGeneric("values"))


#' Generic function to load data from a data source
#' @param x a data source
#' @param ... additional arguments
#' @export 
#' @rdname loadData-methods
setGeneric(name="loadData", def=function(x, ...) standardGeneric("loadData"))

#' Generic function to apply a function to an object
#' @param x the object that is mapped
#' @param m the mapping object
#' @param ... additional arguments
#' @export 
#' @rdname map-methods
setGeneric(name="map", def=function(x, m, ...) standardGeneric("map"))


#' Generic function to extract the number of dimensions of an object
#' @param x n-dimensional object
#' @param ... additional arguments
#' @export 
#' @rdname ndim-methods
setGeneric(name="ndim", def=function(x, ...) standardGeneric("ndim"))

#' Generic function to add a dimension to an object
#' @param x a dimensioned object
#' @param n the size of the dimension to add
#' @export 
#' @rdname addDim-methods
setGeneric(name="addDim", def=function(x, n) standardGeneric("addDim"))

#' Generic function to drop a dimension from an object
#' @param x a dimensioned object
#' @param dimnum the index of the dimension to drop
#' @export 
#' @rdname dropDim-methods
setGeneric(name="dropDim", def=function(x, dimnum) standardGeneric("dropDim"))

#' Generic function to extract the \code{space} member variable
#' @param x the object to query
#' @param ... additional arguments
#' @return an object representing the geometric space of the image
#' @export space
#' @rdname space-methods
setGeneric(name="space", def=function(x, ...) standardGeneric("space"))

#' Generic function to fill disjoint sets of values with the output of a function
#' @param x the object to split
#' @param fac the factor to split by
#' @param FUN the function to summarize the the clusters
#' @return a new object where the original values have been replaced by the function output
#' @export 
#' @rdname splitFill-methods
setGeneric(name="splitFill", def=function(x, fac, FUN) standardGeneric("splitFill"))

#' Generic function to map values from one set to another using a user-supplied lookup table
#' @param x the object to map values from
#' @param lookup the lookup table. The first column is the "key" the second column is the "value".
#' @return a new object where the original values have been filled in with the values in the lookup table
#' @export 
#' @rdname fill-methods
setGeneric(name="fill", def=function(x, lookup) standardGeneric("fill"))



#' Generic function to center/scale subsets of an object
#' @param x a numeric matrix(like) object
#' @param f the conditioning expression (usually a factor)
#' @param center should values be centered?
#' @param scale should values be scaled?
#' @return a new matrix(like) object where the original values have been grouped by a factor and then centered and/or scaled for each grouping
#' @docType methods
#' @export 
#' @rdname splitScale-methods
setGeneric(name="splitScale", def=function(x, f, center, scale) standardGeneric("splitScale"))

#' Generic function to summarize subsets of an object
#' @param x a numeric matrix(like) object
#' @param fac the factor to define subsets of the object
#' @param FUN the function to apply to each subset
#' @return a new matrix(like) object where the original values have been scaled
#' @docType methods
#' @export 
#' @rdname splitReduce-methods
setGeneric(name="splitReduce", def=function(x, fac, FUN) standardGeneric("splitReduce"))


#' Generic function to extract the voxel dimensions of an image
#' @param x the object
#' @return a numeric vector
#' @export 
#' @rdname spacing-methods
setGeneric(name="spacing", def=function(x) standardGeneric("spacing"))

#' Generic function to extract the spatial bounds (origin + dim * spacing) of an image
#' param x the object
#' @export
#' @param x the object with \code{bounds} property
#' @rdname bounds-methods
setGeneric(name="bounds",     def=function(x) standardGeneric("bounds"))


#' Generic getter function to extract image axes
#' @param x an object with a set of axes
#' @export 
#' @rdname axes-methods
setGeneric(name="axes",  def=function(x) standardGeneric("axes"))

#' Generic getter to extract image origin
#' @param x an object with an origin
#' @export 
#' @rdname origin-methods
setGeneric(name="origin", def=function(x) standardGeneric("origin"))

#' Generic getter to extract image coordinate transformation
#' @param x an object with a transformation
#' @export 
#' @rdname trans-methods
setGeneric(name="trans",  def=function(x) standardGeneric("trans"))

#' Generic getter to extract inverse image coordinate transformation
#' @param x an object
#' @export 
#' @rdname inverseTrans-methods
setGeneric(name="inverseTrans", def=function(x) standardGeneric("inverseTrans"))

#' Generic function to read a sequence of elements from an input source
#' @param x the input channel
#' @param numElements the number of elements to read
#' @return the elements as a vector
#' @export 
#' @rdname readElements-methods
setGeneric(name="readElements", def=function(x, numElements) standardGeneric("readElements"))


#' Generic function to write a sequence of elements from an input source
#' @param x the output channel
#' @param els the elements to write
#' @export 
#' @rdname writeElements-methods
setGeneric(name="writeElements", def=function(x, els) standardGeneric("writeElements"))


#' Generic function to write an image volume to disk
#' @param x an image object
#' @param fileName a file name
#' @param format file format string
#' @param dataType output data type
#' @export 
#' @rdname writeVolume-methods
setGeneric(name="writeVolume",  def=function(x, fileName, format, dataType) standardGeneric("writeVolume"))


#' Generic function to write an image vector to disk
#' @param x the image to write
#' @param fileName the bane of the file to write
#' @param format the file format
#' @param dataType the numeric data type
#' @export 
#' @rdname writeVector-methods
setGeneric(name="writeVector",  def=function(x, fileName, format, dataType) standardGeneric("writeVector"))

# Generic function to extract a value
# @param object
# @param x
# @param y
# @param ... additional arguments
# setGeneric(name="value",       def=function(object, x,y, ...) standardGeneric("value"))

#' Generic function to convert 1D indices to N-dimensional grid coordinates
#' @param x the object
#' @param idx the 1D indices
#' @return a matrix of grid coordinates
#' @export 
#' @rdname indexToGrid-methods
setGeneric(name="indexToGrid",   def=function(x, idx) standardGeneric("indexToGrid"))

#' Generic function to convert 1D indices to N-dimensional real world coordinates
#' @param x the object
#' @param idx the 1D indices
#' @return a matrix of real coordinates
#' @export 
#' @rdname indexToCoord-methods
setGeneric(name="indexToCoord",   def=function(x, idx) standardGeneric("indexToCoord"))

#' Generic function to convert N-dimensional real world coordinates to 1D indices
#' @param x the object
#' @param coords a matrix of real world coordinates
#' @return a vector of indices
#' @export 
#' @rdname coordToIndex-methods
setGeneric(name="coordToIndex",   def=function(x, coords) standardGeneric("coordToIndex"))

#' Generic function to convert N-dimensional real world coordinates to grid coordinates
#' @param x the object
#' @param coords a matrix of real world coordinates
#' @return a matrix of grid coordinates
#' @export 
#' @rdname coordToGrid-methods
setGeneric(name="coordToGrid",   def=function(x, coords) standardGeneric("coordToGrid"))

#' Generic function to convert N-dimensional grid coordinate coordinates to real world coordinates
#' @param x the object
#' @param coords a matrix of grid coordinates
#' @return a matrix of real coordinates
#' @export 
#' @rdname gridToCoord-methods
setGeneric(name="gridToCoord",   def=function(x, coords) standardGeneric("gridToCoord"))



#' Generic function to convert 1-dimensional real axis coordinates along a single axis dimension to an 1D index along the same axis
#' @param x the object
#' @param real the axis coordinates
#' @param dimNum the dimension number of the axis (e.g.  1, 2, 3)
#' @return a vector of axis indices
#' @export 
#' @rdname axisToIndex-methods
setGeneric(name="axisToIndex",   def=function(x, real, dimNum) standardGeneric("axisToIndex"))

#' Generic function to convert N-dimensional grid coordinate to 1D indices
#' @param x the object
#' @param coords a matrix where each row is a corodinate or a vector of length N
#' @return a vector of indices
#' @export 
#' @rdname gridToIndex-methods
setGeneric(name="gridToIndex",   def=function(x, coords) standardGeneric("gridToIndex"))


#' Generic function to apply a function to each volume of a four-dimensional image
#' @param x four-dimensional image
#' @param FUN a \code{function} taking one or two arguments (depending on the value of \code{withIndex}
#' @param withIndex whether the index of the volume supplied as the second argument to the function
#' @param mask an image mask indicating subset of volume elements to apply function over
#' @param ... additional arguments
#' @export 
#' @rdname eachVolume-methods
setGeneric(name="eachVolume", def=function(x, FUN, withIndex, mask, ...) standardGeneric("eachVolume"))

#' Generic function to extract a volume from a four-dimensional image
#' @param x four-dimensional image
#' @param i the indices of the volume(s) to extract
#' @param ... additional arguments
#' @export 
#' @rdname takeVolume-methods
setGeneric(name="takeVolume", def=function(x, i, ...) standardGeneric("takeVolume"))


#' Generic functions to apply a function to each (2D) slice of an image
#' @param x the object
#' @param FUN a \code{function} taking one or two arguments (depending on the value of \code{withIndex}
#' @param withIndex whether the index of the slice is supplied as the second argument to the function
#' @param ... additional arguments
#' @export 
#' @rdname eachSlice-methods
setGeneric(name="eachSlice", def=function(x, FUN, withIndex, ...) standardGeneric("eachSlice"))

#' Generic functions to apply a function to each series of a 4D image
#' That is, if the 4th dimension is 'time' each series is a 1D time series.
#' @param x a four dimensional image
#' @param FUN a \code{function} taking one or two arguments (depending on the value of \code{withIndex}
#' @param withIndex whether the index of the series is supplied as the second argument to the function
#' @param ... additional arguments
#' @export 
#' @rdname eachSeries-methods
setGeneric(name="eachSeries", def=function(x, FUN, withIndex, ...) standardGeneric("eachSeries"))

#' Generic functions to scale (center and/or normalize by standard deviation) each series of a 4D image
#' That is, if the 4th dimension is 'time' each series is a 1D time series.
#' @param x a four dimensional image
#' @param center a \code{logical} value indicating whether series should be centered
#' @param scale a \code{logical} value indicating whether series should be divided by standard deviation
#' @export 
#' @rdname scaleSeries-methods
setGeneric(name="scaleSeries", def=function(x, center, scale) standardGeneric("scaleSeries"))



#' Generic function to extract a set of series from a 4D image
#' @param x a four dimensional image
#' @param indices the indices of the series' to extract
#' @param ... additional arguments
#' @export 
#' @rdname takeSeries-methods
setGeneric(name="takeSeries", def=function(x, indices, ...) standardGeneric("takeSeries"))


#' Convert to sparse representation
#' @param x the object to sparsify
#' @param mask the elements to retain
#' @param ... additional arguments
#' @export
#' @rdname as.sparse-methods
setGeneric(name="as.sparse", def=function(x, mask, ...) standardGeneric("as.sparse"))

#' Convert to a LogicalBrainVolume
#' @param x the object to binarize
#' @param indices the indices to set to TRUE
#' @export 
#' @rdname as.mask-methods
setGeneric(name="as.mask", def=function(x, indices) standardGeneric("as.mask"))


#' tesselate
#' @param x the object to tesselate
#' @param K the number of partitions
#' @param ... extra arguments
#' @export 
#' @rdname tesselate-methods
setGeneric(name="tesselate", def=function(x, K, ...) standardGeneric("tesselate"))

#' partition
#' @param x the object to partition
#' @param K the number of partitions
#' @param features the features used to define the partition
#' @param ... additional arguments
#' @export 
#' @rdname partition-methods
setGeneric(name="partition", def=function(x, K, features, ...) standardGeneric("partition"))

#' mergePartitions
#' @param x the object to merge
#' @param K the number of merged partitions
#' @param features the features used to define the partition
#' @param ... additional arguments
#' @export 
#' @rdname mergePartitions-methods
setGeneric(name="mergePartitions", def=function(x, K, features, ...) standardGeneric("mergePartitions"))

#' numClusters
#' @param x the object to extract number of clusters 
#' @export 
#' @rdname numClusters-methods
setGeneric(name="numClusters", def=function(x) standardGeneric("numClusters"))

#' clusterCenters
#' @param x the object to extract cluster centers from
#' @param features additional features
#' @param FUN a user-supplied function
#' @export 
#' @rdname clusterCenters-methods
setGeneric(name="clusterCenters", def=function(x, features, FUN) standardGeneric("clusterCenters"))


#' pick
#' @param x the object to pick from
#' @param mask a mask object
#' @param ... addiitonal arguments
#' @export
#' @rdname pick-methods
setGeneric(name="pick", def=function(x, mask, ...) standardGeneric("pick"))


#' Extract coordinates
#' @param x the object to extract coordinates from
#' @param ... additional arguments
#' @export 
#' @rdname coords-methods
setGeneric(name="coords", def=function(x, ...) standardGeneric("coords"))

#' Extract indices
#' @param x the object to extract indices
#' @export 
#' @rdname indices-methods
setGeneric(name="indices", def=function(x) standardGeneric("indices"))

#' Index Lookup operation
#' @param x the object to query
#' @param i the index to lookup
#' @param ... additional arguments
#' @export 
#' @rdname lookup-methods
setGeneric(name="lookup", def=function(x, i, ...) standardGeneric("lookup"))

#' Extract vector series from object
#' @param x the object
#' @param i the series index
#' @param ... additional arguments
#' @export 
#' @rdname series-methods
setGeneric(name="series", def=function(x, i, ...) standardGeneric("series"))   

#' Extract a 2D slice from an image volume
#' @param x the object
#' @param zlevel coordinate (in voxel units) along the sliced axis
#' @param along the axis along which to slice
#' @param orientation the target orientation of the 2D slice
#' @param ... additional arguments
#' @export 
#' @rdname slice-methods
setGeneric(name="slice", def=function(x, zlevel, along, orientation, ...) standardGeneric("slice"))   


#' Extract permutation matrix
#' @param x the object
#' @param ... additional arguments
#' @export 
#' @rdname permMat-methods
setGeneric(name="permMat", def=function(x, ...) standardGeneric("permMat"))   

#' Concatenate two objects
#' @param x the first object
#' @param y the second object
#' @param ... additional objects
#' @export 
#' @rdname concat-methods
setGeneric(name="concat", def=function(x,y, ...) standardGeneric("concat"))

#' Find connected components
#' @name connComp
#' @param x the image object
#' @param ... additonal arguments
#' @export
#' @rdname connComp-methods
setGeneric(name="connComp", def=function(x, ...) standardGeneric("connComp"))

#' seriesIter
#' 
#' Construct a series iterator
#' @param x the object to be iterated over. This is typically an instance of class \code{\linkS4class{BrainVector}}
#' @return an \code{iter} object from the \code{iterators} package.
#' @export
#' @examples 
#' 
#' ## create a BrainVector with 10X10X10X10, where the last dimension is by convention the 'series' dim.
#' bvec <- BrainVector(array(rnorm(10*10*10*10), c(10,10,10,10)), BrainSpace(c(10,10,10,10), c(1,1,1)))
#' iter <- seriesIter(bvec)
#' 
#' ## compute mean of each series
#' foreach(i=iter, .combine=c) %do% { mean(i) }
#' iter <- seriesIter(bvec)
#' 
#' ## combine all series into a matrix
#' foreach(i=iter, .combine=rbind) %do% { i }
#' 
#' ## scale all series, add as columns in matrix.
#' foreach(i=seriesIter(bvec), .combine=cbind) %do% { scale(i) }
setGeneric(name="seriesIter", def=function(x) standardGeneric("seriesIter"))


#' extract voxel coordinates
#' @param x the object to extract voxels from
#' @param ... additional arguments to function
#' @export 
#' @rdname voxels-methods
setGeneric(name="voxels", def=function(x, ...) standardGeneric("voxels"))


if (!isGeneric("image"))
  setGeneric("image", function(x, ...) standardGeneric("image"))

if (!isGeneric("as.raster"))
  setGeneric("as.raster", function(x, ...) standardGeneric("as.raster"))

#' overlay two objects
#' @param x the underlay object
#' @param y the overlay object
#' @param ... additional arguments for class-specific implementations
#' @export 
#' @rdname overlay-methods
setGeneric("overlay", function(x, y, ...) standardGeneric("overlay"))

