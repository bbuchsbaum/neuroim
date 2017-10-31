#' @include AllClass.R
{}
#' @include AllGeneric.R
{}


#' @export
BasisBrainVector <- function(mask, basis, coeffs) {
  mask <- as.logical(mask)
  mask.idx <- which(mask>0)
  map <- IndexLookupVolume(space(mask), mask.idx)
  
  stopifnot(inherits(mask, "LogicalBrainVolume"))
  
  if (!nrow(coeffs) == length(mask.idx)) {
    stop("'coeffs' matrix must have number of rows equal to cardinality of 'mask'")
  }
  
  if (ncol(coeffs) != ncol(basis)) {
    stop("'coeffs' matrix must have number of columns equal number of columns of 'basis' matrix")
  }
  
  new("BasisBrainVector", mask=mask, basis=Matrix(basis), coeffs=Matrix(coeffs), map=map)
}


#' @rdname series-methods
#' @export
setMethod("series", signature(x="BasisBrainVector", i="matrix"),
          def=function(x,i) {
            assertthat::assert_that(ncol(i) == 3)
            idx <- gridToIndex(x@mask, i)
            colidx <- lookup(x@map, idx)
            x@basis %*% Matrix::t(x@coeffs[as.numeric(colidx),])
          })

#' @rdname dim-methods
#' @export
setMethod("dim", signature(x="BasisBrainVector"), def=function(x) c(dim(x@mask), nrow(x@basis)))

#' @export
#' @rdname series-methods 
#' @param j index for 2nd dimension
#' @param k index for 3rd dimension
setMethod("series", signature(x="BasisBrainVector", i="numeric"),
          def=function(x,i, j, k) {	
            if (missing(j) && missing(k)) { 
              idx <- lookup(x@map, i)
              idx.nz <- idx[idx!=0]
              if (length(idx.nz) == 0) {
                matrix(0, dim(x)[4], length(i))
              } else {
                mat <- matrix(0, dim(x)[4], length(i))
                mat[, idx !=0] <-  x@basis %*% Matrix::t(x@coeffs[as.numeric(idx.nz),])
                mat
              }
            } else {
              vdim <- dim(x)
              slicedim <- vdim[1] * vdim[2]
              idx <- slicedim*(k-1) + (j-1)*vdim[1] + i
              callGeneric(x, idx)
            }
            
          })

