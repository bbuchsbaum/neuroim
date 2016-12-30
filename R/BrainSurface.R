

#' load an surface from a surface geometry with optional mapped surface data
#' 
#' @param surfaceName the name of the file containing the surface geometry.
#' @param surfaceDataName the name of the file containing the values to be mapped to the surface (optional).
#' @param indices indices to be used (optional), only if \code{surfaceDataName = NULL}
#' @param keepZero NOT USED!!
#' @return an instance of the class:
#'  \code{\linkS4class{SurfaceGeometry}} 
#'  or \code{\linkS4class{BrainSurface}} 
#'  or \code{\linkS4class{BrainSurfaceVector}} 
#' @export loadSurface
loadSurface  <- function(surfaceName, surfaceDataName=NULL, indices=NULL, keepZero=FALSE) {
  if (is.null(surfaceDataName)) {
    surfSource <- SurfaceGeometrySource(surfaceName)
    loadData(surfSource)
  } else {
    src <- BrainSurfaceSource(surfaceName, surfaceDataName, indices)
    loadData(src)
  }
}

#' load surface data and attach to \code{\linkS4class{SurfaceGeometry}}
#' @param geometry a \code{\linkS4class{SurfaceGeometry}} instance
#' @param surfaceDataName the name of the file containing the values to be mapped to the surface.
#' @param indices indices to load (optional)
#' @return an instance of the class \code{\linkS4class{BrainSurface}} or \code{\linkS4class{BrainSurfaceVector}} 
#' @export loadSurfaceData
loadSurfaceData  <- function(geometry, surfaceDataName, indices=NULL) {
  src <- BrainSurfaceSource(geometry, surfaceDataName, NULL)
  loadData(src)
}

#' load surface geometry
#' @param surfaceName the name of the file containing the surface geometry.
#' @export loadSurfaceGeometry
loadSurfaceGeometry <- function(surfaceName) {
  surfSource <- SurfaceGeometrySource(surfaceName)
  loadData(surfSource)

}

#' Constructor for SurfaceGeometrySource
#' 
#' @param surfaceName the name of the file containing the surface geometry.
#' @export 
#' @rdname SurfaceGeometrySource-class
SurfaceGeometrySource <- function(surfaceName) {
  stopifnot(is.character(surfaceName))
  stopifnot(file.exists(surfaceName))
  metaInfo <- readHeader(surfaceName)
  new("SurfaceGeometrySource", metaInfo=metaInfo)	
}

#' show a \code{SurfaceGeometry}
#' @param object the object
#' @export
setMethod(f="show", signature=signature("SurfaceGeometry"), 
          def=function(object) {
            cat("SurfaceGeometry \n")
          })



#' Constructor for BrainSurfaceSource
#' 
#' @param surfaceGeom the name of the file containing the surface geometry or a \code{SurfaceGeometry} instance
#' @param surfaceDataName the name of the file containing the data values to be mapped to the surface.
#' @param indices the indices to load from surface data matrix (if provided)
#' @export 
#' @rdname BrainSurfaceSource-class
BrainSurfaceSource <- function(surfaceGeom, surfaceDataName, indices=NULL) {
  if (is.character(surfaceGeom)) {
    assert_that(file.exists(surfaceGeom))
    src <- SurfaceGeometrySource(surfaceGeom)
    surfaceGeom <- loadData(src)
  }
  
  dataMetaInfo <- readHeader(surfaceDataName)
  
  if (is.null(indices)) {
    indices <- 1:dataMetaInfo@nels
  } 
  
  if (length(indices) > 1 && dataMetaInfo@nels > 1) {
    new("BrainSurfaceVectorSource", geometry=surfaceGeom, 
        dataMetaInfo=dataMetaInfo, 
        indices=as.integer(indices))
  } else {
    new("BrainSurfaceSource", geometry=surfaceGeom, 
        dataMetaInfo=dataMetaInfo, 
        index=as.integer(indices))	
  }
  
}


#' @rdname vertices-methods
#' @export
setMethod(f="vertices", signature=c("BrainSurface"),
          def=function(x) {
            vertices(x@geometry)
          })

#' @rdname vertices-methods
#' @param indices a vector of indices specifying the valid surface nodes.  
#' @export
setMethod(f="vertices", signature=c("BrainSurfaceVector"),
          def=function(x, indices) {
            callGeneric(x@geometry, indices)
          })

#' @rdname vertices-methods
#' @export
setMethod(f="vertices", signature=c("SurfaceGeometry"),
          def=function(x, indices) {
            t(x@mesh$vb[1:3,indices, drop=FALSE])
          })

#' @rdname nodes-methods
#' @export
setMethod(f="nodes", signature=c("SurfaceGeometry"),
          def=function(x) {
            seq(1, ncol(x@mesh$vb))
          })

#' @rdname nodes-methods
#' @export
setMethod(f="indices", signature=c("BrainSurfaceVector"),
          def=function(x) {
            x@indices
          })

#' @rdname nodes-methods
#' @export
setMethod(f="nodes", signature=c("BrainSurface"),
          def=function(x) {
            callGeneric(x@geometry)
          })

#' @rdname series-methods
#' @importFrom Matrix Matrix
#' @importFrom Matrix t
#' @return a class of type \code{Matrix}
#' @export
setMethod("series", signature(x="BrainSurfaceVector", i="numeric"),
          def=function(x, i) {	
            Matrix::t(x@data[i,])
          })


#' @rdname series-methods
#' @return a class of type \code{ROISurfaceVector}
#' @export
setMethod("series_roi", signature(x="BrainSurfaceVector", i="numeric"),
          def=function(x, i) {	
            m <- as.matrix(series(x,i))
            ROISurfaceVector(geometry=x@geometry, indices=i, data=m)
          })

#' @rdname series-methods
#' @importFrom Matrix Matrix
#' @export
setMethod("series", signature(x="BrainSurfaceVector", i="integer"),
          def=function(x, i) {	
            Matrix::t(x@data[i,])
          })

#' @rdname series-methods
#' @export
setMethod("series", signature(x="BrainSurfaceVector", i="ROISurface"),
          def=function(x, i) {	
            callGeneric(x, indices(i))
          })

#' @rdname series-methods
#' @export
setMethod("series_roi", signature(x="BrainSurfaceVector", i="ROISurface"),
          def=function(x, i) {	
            mat <- series(x, indices(i))
            ROISurfaceVector(x@geometry, indices(i), as.matrix(mat))
          })

#' @rdname series-methods
#' @export
setMethod("series", signature(x="BrainSurface", i="numeric"),
          def=function(x, i) {	
            stop("not implemented")
          })

#' @rdname graph-methods
#' @export
setMethod("graph", signature(x="BrainSurface"),
          def=function(x,...) {	
            callGeneric(x@geometry)
          })

#' @rdname graph-methods
#' @export
setMethod("graph", signature(x="BrainSurfaceVector"),
          def=function(x, ...) {	
            callGeneric(x@geometry)
          })

#' @rdname graph-methods
#' @export
setMethod("graph", signature(x="SurfaceGeometry"),
          def=function(x) {	
            x@graph
          })


#' construct a new BrainSurfaceVector 
#' @param geometry a \code{SurfaceGeometry} instance
#' @param indices a vector of indices specifying the valid surface nodes. 
#' @param mat a \code{matrix} of data values (rows=nodes, columns=variables)
#' @export
BrainSurfaceVector <- function(geometry, indices, mat) {
  new("BrainSurfaceVector", source=NullSource(), geometry=geometry, indices=as.integer(indices), 
      data=Matrix::Matrix(mat))
  
}

#' construct a new BrainSurface object
#' @param geometry a \code{SurfaceGeometry} instance
#' @param indices a vector of indices specifying the valid surface nodes. 
#' @param data a \code{vector} of data values.
#' @export
BrainSurface <- function(geometry, indices, data) {
  new("BrainSurface", source=NullSource(), geometry=geometry, indices=as.integer(indices), 
      data=data)
  
}

#' show a \code{BrainSurfaceVector}
#' @param x the object
#' @param ... extra arguments
#' @export
setMethod(f="show", signature=signature("BrainSurfaceVector"), 
          def=function(object) {
            cat("BrainSurfaceVector \n")
          })



#' load a BrainSurfaceVector
#' @export loadData
#' @importFrom Matrix Matrix
#' @rdname loadData-methods
setMethod(f="loadData", signature=c("BrainSurfaceVectorSource"), 
          def=function(x) {
          
            geometry <- x@geometry
            
            reader <- dataReader(x@dataMetaInfo,0)
            nodes <- readColumns(reader,0) + 1
            mat <- readColumns(reader, x@indices)
            nvert <- ncol(geometry@mesh$vb)
            
            allzero <- apply(mat, 1, function(vals) all(vals == 0))
            nodes <- nodes[!allzero]
            
            
            mat <- if (nvert > length(nodes) && length(nodes)/nvert < .5) {
              M <- do.call(rbind, lapply(1:ncol(mat), function(i) {
                cbind(i=nodes, j=i, x=mat[,i])
              }))
              
              Matrix::sparseMatrix(i=M[,1], j=M[,2], x=M[,3])
            } else if (nvert > length(nodes)) {
              m <- matrix(0, nvert, ncol(mat))
              m[nodes, 1:ncol(mat)] <- mat[nodes,]
              Matrix::Matrix(m)
            } else {
              Matrix::Matrix(mat)
            }
            
            svec <- new("BrainSurfaceVector", source=x, geometry=geometry, 
                        indices=as.integer(nodes), data=mat)
            
          })

#' load a \code{SurfaceGeometry} instance
#' @export loadData
#' @rdname loadData-methods
setMethod(f="loadData", signature=c("SurfaceGeometrySource"), 
          def=function(x) {
            geometry <- loadData(x@metaInfo)
          })


#' load a BrainSurface
#' @export loadData
#' @rdname loadData-methods
setMethod(f="loadData", signature=c("BrainSurfaceSource"), 
          def=function(x) {
            geometry <- x@geometry
            reader <- dataReader(x@dataMetaInfo,0)
            nodes <- readColumns(reader,0) + 1
            vals<- readColumns(reader, x@index)[,1]
            nvert <- ncol(geometry@mesh$vb)
            
            avals <- numeric(nvert)
            avals[nodes] <- vals
            surf<- new("BrainSurface", source=x, geometry=geometry, data=avals)
            
          })
          


#' construct a graph from a set of vertices and nodes faces.
#' 
#' @export
#' @param vertices N-by-3 matrix of vertices 
#' @param nodes matrix of node faces, where each row is a set of three vertex indices.
#' @return an \code{igraph} instance representing th mesh connectivity.
#' @importFrom igraph set.vertex.attribute simplify get.edgelist
#' @importFrom igraph graph_from_edgelist get.edgelist set.vertex.attribute 
#' @importFrom igraph simplify graph.adjacency
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
            loadFSSurface(x)
          })



#' load Freesurfer ascii surface
#' 
#' @param metaInfo instance of type \code{FreesurferSurfaceGeometryMetaInfo}
#' @details requires rgl library
#' @return a class of type \code{BrainSurface}
#' @importFrom plyr rbind.fill.matrix
#' @export
loadFSSurface <- function(metaInfo) {
  if (!requireNamespace("rgl", quietly = TRUE)) {
    stop("Pkg rgl needed for this function to work. Please install it.",
         call. = FALSE)
  }
  
  meshname <- metaInfo@headerFile
  ninfo <- as.integer(strsplit(readLines(meshname, n=2)[2], " ")[[1]])
  message("loading ", meshname)
  asctab <- read.table(meshname, skip=2)
  
  vertices <- as.matrix(asctab[1:ninfo[1],1:3])
  nodes <- as.matrix(asctab[(ninfo[1]+1):nrow(asctab),1:3])
  message("constructing graph")
  graph <- meshToGraph(vertices, nodes)
  
  mesh <- rgl::tmesh3d(as.vector(t(vertices)), as.vector(t(nodes))+1, homogeneous=FALSE)
  new("SurfaceGeometry", source=new("SurfaceGeometrySource", metaInfo=metaInfo), mesh=mesh, graph=graph)
}


findNeighbors <- function(graph, node, radius, edgeWeights, max_order=NULL) {
  if (is.null(max_order)) {
    avg_weight <- mean(edgeWeights)
    max_order <- radius/avg_weight + (2*avg_weight)
  }
  
  cand <- igraph::ego(graph, order= max_order, nodes=node)[[1]]
  D <- igraph::distances(graph, node, cand, weights=edgeWeights, algorithm="dijkstra")
  keep <- which(D < radius)[-1]
  cand[keep]
}


findAllNeighbors <- function(g, radius, edgeWeights, nodes=NULL) {
  avg_weight <- mean(edgeWeights)
  est_order <- ceiling(radius/avg_weight) + 1
  
  if (is.null(nodes)) {
    nodes <- igraph::V(g)
  }
  
  nabeinfo <- lapply(nodes, function(v) {
    cand <- igraph::ego(g, order= est_order, nodes=v)[[1]]
    D <- igraph::distances(g, v, cand, weights=edgeWeights, algorithm="dijkstra")
    keep <- which(D < radius)
    if (length(keep) > 0) {
      knabes <- cand[keep]
      cbind(i=rep(v, length(knabes)), j=knabes, d=D[keep])
    } else {
      matrix(0,0,0)
    }
  })
  
  mat <- plyr::rbind.fill.matrix(nabeinfo)
  adj <- Matrix::sparseMatrix(i=mat[,1], j=mat[,2], x=mat[,3])
  igraph::graph.adjacency(adj, mode="undirected", weighted=TRUE)
}


#' @rdname neighborGraph-methods
#' @importFrom grDevices rainbow
#' @export
#' @aliases neighborGraph,igraph,numeric,missing,missing
setMethod(f="neighborGraph", signature=c(x="igraph", radius="numeric", edgeWeights="missing", nodes="missing"),
          def=function(x, radius ) {
            edgeWeights=igraph::E(x)$dist    
            findAllNeighbors(x, radius, as.vector(edgeWeights))
})

#' @rdname neighborGraph-methods
#' @export
#' @aliases neighborGraph,igraph,numeric,numeric,missing
setMethod(f="neighborGraph", signature=c(x="igraph", radius="numeric", edgeWeights="numeric", nodes="missing"),
          def=function(x, radius, edgeWeights) {
            stopifnot(length(edgeWeights) == length(igraph::E(x))) 
            findAllNeighbors(x, radius, edgeWeights)
          })



#' @rdname neighborGraph-methods
#' @export
#' @aliases neighborGraph,igraph,numeric,numeric,integer
setMethod(f="neighborGraph", signature=c(x="igraph", radius="numeric", edgeWeights="numeric", nodes="integer"),

          def=function(x,radius, edgeWeights, nodes) {
            stopifnot(length(edgeWeights) == length(igraph::E(x)))
            findAllNeighbors(x,radius, edgeWeights, nodes)
          })

#' @rdname neighborGraph-methods
#' @export
#' @aliases neighborGraph,igraph,numeric,missing,integer
setMethod(f="neighborGraph", signature=c(x="igraph", radius="numeric", edgeWeights="missing", nodes="integer"),
          def=function(x,radius, nodes) {
            findAllNeighbors(x, radius, igraph::E(x)$dist, nodes) 
          })

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


viewSurface <- function(surfgeom, vals, col=heat.colors(128, alpha = 1), 
                        zero.col = "#00000000", 
                        alpha=1, 
                        geom.col="lightgray") {
#   v1 <- vals[surfgeom@mesh$it[1,]]
#   v2 <- vals[surfgeom@mesh$it[2,]]
#   v3 <- vals[surfgeom@mesh$it[3,]]
#   avgvals <- (v1 + v2 + v3)/3
#   
#   clrs <- mapToColors(avgvals, col=rev(rainbow(60)))
#   ccol <- rep(clrs,each=3)
#   shade3d(surfgeom@mesh, col=ccol)
#   
  v2 <- vals[as.vector(surfgeom@mesh$it)]
  clrs <- mapToColors(v2, col=rev(rainbow(60)), alpha=1)
  rgl::shade3d(surfgeom@mesh, col=clrs, alpha=.5)
}



