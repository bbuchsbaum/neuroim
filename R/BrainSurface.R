

#' load an surface from a set of files
#' @param surfaceName the name of the file containing the surface geometry.
#' @param surfaceDataName the name of the file containing the values to be mapped to the surface.
#' @return an instance of the class \code{\linkS4class{BrainSurface}}
#' @export loadSurface
loadSurface  <- function(surfaceName, surfaceDataName) {
  src <- BrainSurfaceSource(surfaceName, surfaceDataName)
  loadData(src)
}

#' Constructor for BrainSurfaceSource
#' 
#' @param surfaceName the name of the file containing the surface geometry.
#' @param surfaceDataName the name of the file containing the data values to be mapped to the surface.
#' @param index the integer offset into the surface data matrix
#' @export 
#' @rdname BrainSurfaceSource-class
BrainSurfaceSource <- function(surfaceName, surfaceDataName, index=1) {
  stopifnot(is.character(surfaceName))
  stopifnot(is.character(surfaceDataName))
  
  stopifnot(file.exists(surfaceName))
  stopifnot(file.exists(surfaceDataName))
  
  metaInfo <- readHeader(surfaceName)
  #surfaceDataName = "/Users/bbuchsbaum/rstudio/rMVPA/test_data/surface_mouth/1005a_mprage/betas_hhh.niml.dset"
  # surfaceName = "/Users/bbuchsbaum/rstudio/rMVPA/test_data/surface_mouth/1005a_mprage/surface/SUMA/lh.smoothwm.asc"
  metaInfo2 <- readHeader(surfaceDataName)
  
  new("BrainSurfaceSource", metaInfo=metaInfo, dataMetaInfo=metaInfo2, index=as.integer(index))								
  
}



#' load a BrainSurface
#' @export loadData
#' @importFrom utils read.table
#' @rdname loadData-methods
setMethod(f="loadData", signature=c("BrainSurfaceSource"), 
          def=function(x) {
            reader <- loadData(x@metaInfo,0)
            
            reader <- dataReader(x@dataMetaInfo,0)
            nodes <- readColumns(reader,0)
            
          })


meshToGraph <- function(vertices, nodes) {
  edge1 <- nodes[,1:2 ]
  edge2 <- nodes[,2:3 ]
  edges <- rbind(e1,e2) + 1
  
  gg1 <- igraph::simplify(igraph::graph_from_edgelist(edges, directed=FALSE))
  gg1 <- set.vertex.attribute(gg1, "x", V(gg1), vertices[,1])
  gg1 <- set.vertex.attribute(gg1, "y", V(gg1), vertices[,2])
  gg1 <- set.vertex.attribute(gg1, "z", V(gg1), vertices[,3])
  
  gg1
  
}

#' load Freesurfer ascii surface
#' @param mesh file name of mesh to read in.
loadFSSurface <- function(mesh) {
  if (!requireNamespace("rgl", quietly = TRUE)) {
    stop("Pkg rgl needed for this function to work. Please install it.",
         call. = FALSE)
  }
  
  ninfo <- as.integer(strsplit(readLines(mesh, n=2)[2], " ")[[1]])
  asctab <- read.table(mesh, skip=2)
  
  vertices <- as.matrix(asctab[1:ninfo[1],1:3])
  dat <- asctab[1:mninfo[1],4]
  nodes <- as.matrix(asctab[(ninfo[1]+1):nrow(asctab),1:3])
  
  
  
  mesh <- rgl::tmesh3d(as.vector(t(vertices)), as.vector(t(nodes))+1, homogeneous=FALSE)
  new("BrainSurface", mesh=mesh, dat)
}



