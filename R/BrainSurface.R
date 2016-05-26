

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


#' construct a graph from a set of vertices and nodes faces.
#' 
#' @export
#' @param N-by-3 matrix of vertices 
#' @param matrix of node faces, where each row is a set of three vertex indices.
#' @return an \code{igraph} instance representing th mesh connectivity.
meshToGraph <- function(vertices, nodes) {
  edge1 <- as.matrix(nodes[,1:2 ])
  edge2 <- as.matrix(nodes[,2:3 ])
  edges <- rbind(edge1,edge2) + 1
  
  gg1 <- igraph::simplify(igraph::graph_from_edgelist(edges, directed=FALSE))
  emat <- get.edgelist(gg1)
  v1 <- vertices[emat[,1],]
  v2 <- vertices[emat[,2],]
  
  ED <- sqrt(rowSums((v1 - v2)^2))
  
  gg1 <- set.vertex.attribute(gg1, "x", V(gg1), vertices[,1])
  gg1 <- set.vertex.attribute(gg1, "y", V(gg1), vertices[,2])
  gg1 <- set.vertex.attribute(gg1, "z", V(gg1), vertices[,3])
  
  gg1 <- set.edge.attribute(gg1, "dist", E(gg1), ED)
  gg1
  
}

####

## curvature: 

## curv <- Rvcg::vcgCurve(mesh)
## normalize <- function(vals) (vals - min(vals))/(max(vals)-min(vals))
## vbmean <- normalize(curv$meanitmax)
## ccol = ifelse(curv$meanitmax > 0, "red", "green")
## make triangles: tri = misc3d::makeTriangles(t(vertices), t(nodes)+1, color=ccol)
## draw: drawScene.rgl(tri)
## for normal rgl using shade3d:
## ccol <- rep(ccol,each=3)
####

#' load Freesurfer ascii surface
#' @param mesh file name of mesh to read in.
loadFSSurface <- function(meshname) {
  if (!requireNamespace("rgl", quietly = TRUE)) {
    stop("Pkg rgl needed for this function to work. Please install it.",
         call. = FALSE)
  }
  
  ninfo <- as.integer(strsplit(readLines(meshname, n=2)[2], " ")[[1]])
  message("loading ", meshname)
  asctab <- read.table(meshname, skip=2)
  
  vertices <- as.matrix(asctab[1:ninfo[1],1:3])
  dat <- asctab[1:ninfo[1],4]
  nodes <- as.matrix(asctab[(ninfo[1]+1):nrow(asctab),1:3])
  message("constructing graph")
  graph <- meshToGraph(vertices, nodes)
  
  mesh <- rgl::tmesh3d(as.vector(t(vertices)), as.vector(t(nodes))+1, homogeneous=FALSE)
  new("BrainSurface", mesh=mesh, data=dat, graph=graph)
}

knnAdjacency <- function(surf, knn=10, edge_weights=E(surf@graph)$dist, mutual=TRUE) {
  nabeinfo <- lapply(V(graph), function(v) {
  
    cand <- ego(surf@graph, order= sqrt(knn), nodes=v)[[1]]
    D <- distances(surf@graph, v, cand, weights=edge_weights, algorithm="dijkstra")
    ord <- order(D)[2:(knn+1)]
    knabes <- cand[ord]
    cbind(i=rep(v, length(knabes)), j=knabes, d=D[ord])
  })
  
  mat <- plyr::rbind.fill.matrix(nabeinfo)
  adj <- sparseMatrix(i=mat[,1], j=mat[,2], x=mat[,3])
  
  S <- rbind(summary(adj), summary(t(adj)))
  outS <- aggregate(x ~ i + j, data = S, max)
  symmAdj <- sparseMatrix(i = outS$i,
               j = outS$j,
               x = outS$x,
               dims = c(nrow(adj), ncol(adj)))
  
  
}



