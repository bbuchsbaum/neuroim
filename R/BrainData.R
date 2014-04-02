#' @include AllClass.R
{}
#' @include AllGeneric.R
{}


#' ndim
#' 
#' @export
#' @rdname ndim-methods
setMethod(f="ndim", signature=signature(x = "BrainData"),
          def=function(x) numdim(x@space))

  
#' dim
#' 
#' @export
#' @rdname dim-methods
#' @aliases dim,BrainData,ANY-method
setMethod(f="dim", signature=signature(x = "BrainData"),
          def=function(x) dim(x@space))

  
#' space
#' 
#' @export
#' @rdname space-methods
setMethod(f="space", signature=signature(x = "BrainData"),
          def=function(x) x@space)

  
#' spacing
#' 
#' @export
#' @rdname spacing-methods
setMethod(f="spacing",signature= signature(x = "BrainData"),
          def=function(x) {
            sp <- space(x)
            spacing(sp)
          })


#' @export
setMethod(f="as.matrix", signature=signature(x = "BrainData"), def=function(x) as(x, "matrix"))

#' @export
setMethod(f="as.array", signature=signature(x = "BrainData"), def=function(x) as(x, "array"))

#' @export
setMethod(f="as.vector", signature=signature(x = "BrainData"), def=function(x) as(x, "vector"))
