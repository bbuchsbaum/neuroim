#' Extract connected components from a 3D mask
#' @export
#' @param mask a 3D binary array
#' @param connect the connectiivty constraint: "6-connect", "18-connect", or "26-connect"
#' @return a two-element list of the connected components (cluster \code{index} and cluster \code{size})
#' The first element \code{index} is a 3D array containing the cluster index of the connected component for each voxel.
#' The second element \code{size} is a 3D array consisting of the size of the connected component inhabited by each voxel.
#' 
#' @examples 
#' 
#' dat <- array(as.logical(rnorm(10*10*10)>.5), c(10, 10, 10))
#' res1 <- connComp3D(dat, connect="6-connect")
#' res2 <- connComp3D(dat, connect="18-connect")
#' res3 <- connComp3D(dat, connect="26-connect")
#' 
connComp3D <- function(mask, connect=c("6-connect", "18-connect", "26-connect")) {
	stopifnot(length(dim(mask)) == 3 && is.logical(mask[1]))
  
  connect <- match.arg(connect)
	
	nodes <- numeric(length(mask)/9)
	labels <- array(0, dim(mask))
	
	DIM <- dim(mask)
	xdim <- DIM[1]
	ydim <- DIM[2]
	zdim <- DIM[3]
	
	local.mask <- if (connect == "6-connect") {
	  as.matrix(
	    rbind(expand.grid(x=c(-1,0,1), y=0, z=0),
	      expand.grid(x=0, y=c(-1,1), z=0),
	      expand.grid(x=0, y=0, z=c(-1,1)))
	  )
	} else if (connect == "18-connect") {
	  as.matrix(rbind(
	        expand.grid(x=c(-1,0,1), y=0, z=0),
	        expand.grid(x=0, y=c(-1,1), z=0),
	        expand.grid(x=0, y=0, z=c(-1,1)),
	        expand.grid(x=c(-1,1), y=c(-1,1), z=0),
	        expand.grid(x=c(-1,1), y=0, z=c(-1,1)),
	        expand.grid(x=0, y=c(-1,1), z=c(-1,1)))
	  )
	} else {
	  as.matrix(expand.grid(x=c(-1,0,1), y=c(-1,0,1), z=c(-1,0,1)))
	}
	dimnames(local.mask) <- NULL
	local.mask <- local.mask[-(ceiling(nrow(local.mask)/2)),]
	
	neighbors <- function(vox) {
		vox.hood <- sweep(local.mask, 2, vox, "+", check.margin=FALSE)
		if (any(vox == 1) || any(vox == DIM)) {
			vox.hood <- vox.hood[apply(vox.hood, 1, function(coords) {
				all(coords > 1 & coords <= DIM)
			}),,drop=FALSE]			
		}
		
		vox.hood[labels[vox.hood] != 0,,drop=F]	
	}

	find <- function(i) {
		while (nodes[i] != i) {
			i <- nodes[i]
		}
		
		nodes[i]
	}
	
	
	nextlabel <- 1
	
	grid <-  .indexToGrid(which(mask>0), dim(mask))
  
	for (i in 1:NROW(grid)) {
		vox <- grid[i,]
		nabes <- neighbors(vox)
		if (nrow(nabes) == 0) {
			nodes[nextlabel] <- nextlabel				
			labels[vox[1],vox[2],vox[3]] <- nextlabel
		} else {
			L <- labels[nabes]					
			ML <- min(L)
			labels[vox[1],vox[2], vox[3]] <- ML	
			nodes[nextlabel] <- ML
			for (lab in L) {
				rootx <- find(lab)
				nodes[rootx] <- find(ML)				
			}
		}
		
		nextlabel <- nextlabel + 1	
	}
	
	##for (k in 1:zdim) {
	##	for (j in 1:ydim) {
	##		for (i in 1:xdim) {
	##			if (mask[i,j,k]) {
	##				nabes <- neighbors(c(i,j,k))			
	##			
	##				if (length(nabes) == 0) {	
	##					nodes[nextlabel] <- nextlabel				
	##					labels[i,j,k] <- nextlabel									
	##				} else {
	##				
	##					L <- labels[nabes]					
	##					ML <- min(L)
	##					labels[i,j,k] <- ML	
	##					nodes[nextlabel] <- ML
	##												
	##					for (lab in L) {
	##						rootx <- find(lab)
	##						nodes[rootx] <- find(ML)				
	##					}
	##				}
	##			
	##				nextlabel <- nextlabel + 1	
	##			}
	##					
	##		}
	##	}
	##}

	## pass2
	for (k in 1:zdim) {
		for (j in 1:ydim) {
			for (i in 1:xdim) {
				if (labels[i,j,k] > 0) {
					labels[i,j,k] <- find(labels[i,j,k])
				}
			}
		}
	}
	
	labs <- labels[labels!=0]
	forelabs <- labels > 0
	
	clusters <- sort(table(labs), decreasing=TRUE)
	SVol <- array(0, dim(mask))
	SVol[forelabs] <- clusters[as.character(labs)]
	
	indices <- 1:length(clusters)
	names(indices) <- names(clusters)
	IVol <- array(0, dim(mask))
	IVol[forelabs] <- indices[as.character(labs)]
	
	
	
	list(index=IVol, size=SVol)
	
}		




