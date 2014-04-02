library(stringr)
library(abind)


wd <- getwd()

if (basename(wd) == "R") {
	prefix = ""
} else if (basename(wd) == "pkg") {
	prefix = "R"	
} else {
	stop(paste("non-standard directory ", wd))
}

fnames = c("AllGeneric.R",
  "AllClass.R",
  "common.R",
  "BrainSpace.R",
  "BrainData.R",
  "BrainSlice.R",
  "BrainVolume.R",
  "BrainRegion3D.R",
  "BrainVector.R",
  "SparseBrainVector.R",
  "conncomp.R",
  "Axis.R",
  "NIFTI_IO.R",
  "AFNI_IO.R",
  "BinaryIO.R",
  "BrainFileDescriptor.R",
  "BrainMetaInfo.R",
  "IndexLookupVolume.R",
  "Ops.R")

fnames <- paste(prefix, "/", fnames, sep="")


lapply(fnames, source)
