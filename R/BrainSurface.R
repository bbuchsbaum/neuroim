

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
#' @param indices the indices to load from surface data matrix (if provided)
#' @export 
#' @rdname BrainSurfaceSource-class
BrainSurfaceSource <- function(surfaceName, surfaceDataName=NULL, indices=NULL) {
  stopifnot(is.character(surfaceName))
  
  stopifnot(file.exists(surfaceName))
  
  if (!is.null(surfaceDataName)) {
    stopifnot(file.exists(surfaceDataName))
    metaInfo <- readHeader(surfaceName)
    dataMetaInfo <- readHeader(surfaceDataName)
    if (is.null(indices)) {
      indices <- 1:dataMetaInfo@nels
    }
    new("BrainSurfaceVectorSource", metaInfo=metaInfo, dataMetaInfo=dataMetaInfo, indices=indices)
  } else {
  
    metaInfo <- readHeader(surfaceName)
    new("BrainSurfaceSource", metaInfo=metaInfo)	
  }
  
}


#' construct a new BrainSurfaceVector 
BrainSurfaceVector <- function(geometry, datanodes, datamat) {
  
}


#' load a BrainSurface
#' @export loadData
#' @importFrom utils read.table
#' @rdname loadData-methods
setMethod(f="loadData", signature=c("BrainSurfaceVectorSource"), 
          def=function(x) {
            geometry <- loadData(x@metaInfo)
            
            reader <- dataReader(x@dataMetaInfo,0)
            nodes <- readColumns(reader,0) + 1
            mat <- readColumns(reader, seq(1, dataMetaInfo@nels))
            nvert <- ncol(geometry@mesh$vb)
            
            mat <- if (nvert > length(nodes) && length(nodes)/nvert < .5) {
              M <- do.call(rbind, lapply(1:ncol(mat), function(i) {
                cbind(i=nodes, j=i, x=mat[,i])
              }))
              
              sparseMatrix(i=M[,1], j=M[,2], x=M[,3])
            } else if (nvert > length(nodes)) {
              m <- matrix(0, nvert, ncol(mat))
              m[nodes, 1:ncol(mat)] <- mat
              Matrix(m)
            } else {
              Matrix(mat)
            }
            
            svec <- new("BrainSurfaceVector", source=x, mesh=geometry@mesh, nodes=as.integer(nodes), data=mat, graph=geometry@graph)
            
          })
          


#' construct a graph from a set of vertices and nodes faces.
#' 
#' @export
#' @param N-by-3 matrix of vertices 
#' @param matrix of node faces, where each row is a set of three vertex indices.
#' @return an \code{igraph} instance representing th mesh connectivity.
#' @import igraph 
meshToGraph <- function(vertices, nodes) {
  edge1 <- as.matrix(nodes[,1:2 ])
  edge2 <- as.matrix(nodes[,2:3 ])
  edges <- rbind(edge1,edge2) + 1
  
  gg1 <- igraph::simplify(igraph::graph_from_edgelist(edges, directed=FALSE))
  emat <- igraph::get.edgelist(gg1)
  v1 <- vertices[emat[,1],]
  v2 <- vertices[emat[,2],]
  
  ED <- sqrt(rowSums((v1 - v2)^2))
  
  gg1 <- igraph::set.vertex.attribute(gg1, "x", igraph::V(gg1), vertices[,1])
  gg1 <- igraph::set.vertex.attribute(gg1, "y", igraph::V(gg1), vertices[,2])
  gg1 <- igraph::set.vertex.attribute(gg1, "z", igraph::V(gg1), vertices[,3])
  
  gg1 <- igraph::set.edge.attribute(gg1, "dist", igraph::E(gg1), ED)
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

#' load a Freesurfer surface geometry
#' @export loadData
#' @importFrom utils read.table
#' @rdname loadData-methods
setMethod(f="loadData", signature=c("FreesurferSurfaceGeometryMetaInfo"), 
          def=function(x) {
            loadFSSurface(x@headerFile)
          })



#' load Freesurfer ascii surface
#' 
#' @param meshname file name of mesh to read in.
#' @details requires rgl library
#' @return a class of type \code{BrainSurface}
#' @export
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


#' Construct a neighborhood graph from a \code{BrainSurface} object using edge weights.
#' @param surf the \code{BrainSurface} instance
#' @param radius the edge radius defining the neighborhood (default is 8)
#' @param edge_weights the weights defining the "cost" of each connection (default is Euclidean edge distance).
#' @return an \code{igraph} instance.
#' @import igraph
neighborGraph <- function(surf, radius=8, edge_weights=igraph::E(surf@graph)$dist) {
  avg_weight <- mean(edge_weights)
  est_order <- radius/avg_weight + (2*avg_weight)
  nabeinfo <- lapply(igraph::V(surf@graph), function(v) {
    cand <- igraph::ego(surf@graph, order= est_order, nodes=v)[[1]]
    D <- igraph::distances(surf@graph, v, cand, weights=edge_weights, algorithm="dijkstra")
    keep <- which(D < radius)[-1]
    knabes <- cand[keep]
    cbind(i=rep(v, length(knabes)), j=knabes, d=D[keep])
  })
  
  mat <- plyr::rbind.fill.matrix(nabeinfo)
  adj <- Matrix::sparseMatrix(i=mat[,1], j=mat[,2], x=mat[,3])
  g <- igraph::graph.adjacency(adj, mode="undirected", weighted=TRUE)
}

# knn_graph <- function(surf, knn=10, edge_weights=E(surf@graph)$dist) {
#   nabeinfo <- lapply(V(graph), function(v) {
#     cand <- ego(surf@graph, order= sqrt(knn), nodes=v)[[1]]
#     D <- distances(surf@graph, v, cand, weights=edge_weights, algorithm="dijkstra")
#     ord <- order(D)[2:(knn+1)]
#     knabes <- cand[ord]
#     cbind(i=rep(v, length(knabes)), j=knabes, d=D[ord])
#   })
#   
#   mat <- plyr::rbind.fill.matrix(nabeinfo)
#   adj <- sparseMatrix(i=mat[,1], j=mat[,2], x=mat[,3])
#   
#   S <- rbind(summary(adj), summary(t(adj)))
#   outS <- aggregate(x ~ i + j, data = S, max)
#   symmAdj <- sparseMatrix(i = outS$i,
#                j = outS$j,
#                x = outS$x,
#                dims = c(nrow(adj), ncol(adj)))
#   
#   ## to make true knn, search for nodes with more than 10 neighbors
#   ## 
#   g <- graph.adjacency(symmAdj, mode="undirected", weighted=TRUE)
#   
# }



