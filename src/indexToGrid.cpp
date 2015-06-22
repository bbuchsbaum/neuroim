
#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericMatrix testCpp(IntegerVector idx, IntegerVector array_dim) {
  NumericMatrix omat(idx.size(), array_dim.size());
  
  for (int i=0; i<array_dim.size(); i++) {
    omat(0,i) = 3;
  }
  return omat;
}
  
//stopifnot(length(dimensions) == 3)
//slicedim = dimensions[1]*dimensions[2]

//apply(vmat, 1, function(vox) {
//  (slicedim*(vox[3]-1)) + (vox[2]-1)*dimensions[1] + vox[1]   
//})	
  
  
// [[Rcpp::export]]
IntegerVector gridToIndex3DCpp(IntegerVector array_dim, NumericMatrix voxmat) {
  int slicedim = array_dim[0]*array_dim[1];
  IntegerVector out = IntegerVector(voxmat.nrow());
  
  for (int i=0; i < voxmat.nrow(); i++) {
    out[i] = (int)((slicedim * (voxmat(i,2) -1)) + ((voxmat(i,1)-1) * array_dim(0)) + voxmat(i,0));
  }
  
  return out;
  
}
  
// [[Rcpp::export]]
NumericMatrix indexToGridCpp(IntegerVector idx, IntegerVector array_dim) {
  int rank = array_dim.size();
  
  int N = idx.size();
  NumericMatrix omat(idx.size(), array_dim.size());
  
  for(int i = 0; i < N; i++) {
    int wh1 = idx(i)-1;
    int tmp = wh1 % array_dim(0);
    IntegerVector wh = IntegerVector(rank, tmp);
    if (rank >= 2) {
      int denom = 1;
      for (int j = 1; j < rank; j++) {
        denom = denom * array_dim(j-1);
        int nextd1 = (int)wh1/denom;
        wh(j) = 1 + nextd1 % array_dim(j);
      }
    }
    
    for (int k=0; k<rank; k++) {
      omat(i,k) = wh(k);
    }
    
  }
  
  return omat;

}


/*
  stopifnot(all(idx > 0 & idx <= prod(array.dim)))
  rank = length(array.dim)
  wh1 = idx-1
wh = 1 + wh1 %% array.dim[1]
wh = rep(wh, rank)
  if (rank >=2) {
    denom = 1
    for (i in 2:rank) {
      denom = denom * array.dim[i-1]
      nextd1 = wh1 %/% denom
      wh[i] = 1 + nextd1%%array.dim[i]
    }
  }
  wh
 
*/