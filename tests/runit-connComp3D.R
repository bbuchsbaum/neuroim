


test.connComp3D {
  for (i in 1:10) {
    mat = array(sample(c(0, 1), size = 10^3, replace = TRUE), dim = rep(10, 3))
    dim(mat)
    # cc = connComp3D(mat)
    mask = mat > 0
    cc = connComp3D(mask)
  }
  
  TRUE
  
}