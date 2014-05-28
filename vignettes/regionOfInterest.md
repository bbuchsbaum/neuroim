<!--
%\VignetteEngine{knitr::knitr}
%\VignetteIndexEntry{creating regions of interest (ROI)}
-->


Regions of interest
===================

## Creating a Spherical ROI

In neuroim there is basic support for creating regions of interest (ROI). To create a spherical ROI around a central point, we need an existing image volume BrainVolume or BrainSpace instance.

To create a spherical region of interest with a 5mm radius around a central voxel at x=20, y=20, z=20, we can do the following:

  ```R
      # attach MNI BrainSpace instance
      
      data("MNI_SPACE_1MM")

      sphere <- RegionSphere(MNI_SPACE_1MM, c(20,20,20), radius=5, fill=100)
      
      # to extract the voxel coordinates of the sphere:
      
      vox <- coords(sphere)
      
      # to get the udnerlying values
      
      vals <- values(sphere)
      all.equal(vals, rep(100, length(vals)))   
  ```
      
## Creating a Spherical ROI around an MNI coordinate

To create a spherical ROI centered around an MNI coordinate, we need tofirst convert the real-valued coordinate to a grid coordinate.

```R
    ## our MNI coordinate is in auditory cortex at -50, -28, 10
    
    coord <- c(-50,-28,10)
    
    # Because RegionSphere takes a coordinate in grid units, we need to convert to the MNI coordinate to grid coordinates.
    
    vox <- coordToGrid(MNI_SPACE_1MM, coord)
    sphere <- RegionSphere(MNI_SPACE_1MM, vox, radius=10, fill=1)
    
    # convert back to MNI coordinates
    
    mnicoords <- indexToCoord(MNI_SPACE_1MM, indices(sphere))
    
    ## compute center of mass of MNI coords in ROI (should be close to original coordinate)
    centerOfMass <- colMeans(mnicoords)
    
    ## -50.5 -27.5  10.5
```
    
## Converting an region of interest to a SparseBrainVolume

We may want to convert a region of interest to a BrainVolume instance. But we don't want to store every value in dense grid. Here we can make use of the "SparseBrainVolume" class.

  ```R
    
    sphere <- RegionSphere(MNI_SPACE_1MM, c(50,50,50), radius=10, fill=1)
    sparsevol <- SparseBrainVolume(values(sphere),MNI_SPACE_1MM,indices=indices(sphere))
    
    sum(sparsevol) == sum(values(sphere))
    
  ```






    
