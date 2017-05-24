parse_niml_element <- function(el) {
  items <- strsplit(el, " ")[[1]]
  if (length(items) > 1) {
    items <- items[items != "" & items != ">"] 
    label <- items[1]
    els <- lapply(items[2:length(items)], function(it) {
      keyval <- strsplit(it, "=")[[1]]
      c(key=keyval[1], val=keyval[2])
    })
    keys <- sapply(els, "[[", "key")
    vals <- lapply(els, "[[", "val")
    names(vals) <- keys
    list(label=label, attr=vals)
  } else if (length(items) == 1) {
    list(label=items[[1]], attr=NULL)
  }
}


read_niml_data <- function(fconn, meta) {
  
  dtype <- meta$ni_type
  
  ## fails when data_type is Node_Bucket_node_indices
  dtype <- strsplit(as.character(dtype), "\\*")[[1]]
  
  if (length(dtype) == 2) {
    nvols <- as.integer(dtype[1])
    type <- dtype[2]
  } else {
    nvols <- 1
    type <- dtype[1]
  }
  
  type <- switch (type,
                  int="integer",
                  double="double",
                  float="double")
  
  
  nels <- as.integer(meta$ni_dimen)
  
  if (!is.null(meta$ni_form) && meta$ni_form == "binary.lsbfirst") {
    allvals <- readBin(fconn, what=type, size=4, n=nels*nvols)
    mat <- matrix(allvals, nvols, nels)
  } else {
    ret <- readLines(fconn, n=nels*nvols+1)[-1]
    if (type == "integer") {
      matrix(as.integer(stringr::str_trim(ret)), nvols,nels)
    } else if (type == "double") {
      matrix(as.numeric(stringr::str_trim(ret)), nvols, nels)
    } else {
      stop(paste("unrecognized type: ", type))
    }
  }
}

parse_niml_header <- function(fconn) {
  out <- c()
  STATE <- "BEGIN"
  while(TRUE ) {
    ch <- readChar(fconn,1)
    
    if (length(ch) == 0) {
      break
    } else if (ch == "<" && STATE == "BEGIN") {
      ## open header
      STATE <- "HEADER"
    } else if (ch == ">" && STATE == "HEADER") {
      STATE <- "END"
      break
    } else {
      out <- c(out, ch)
    }
    
    
  }
  
  out <- paste(out, collapse="")
  out <- gsub("\n", "", out)
  out <- gsub("\"", "", out)
  out <- gsub("/", "", out)
  
  ret <- parse_niml_element(str_trim(out))
  ret
}

parse_niml_next <- function(fconn) {
  header <- parse_niml_header(fconn)
  if (!is.null(header$attr) && (header$label == "SPARSE_DATA" || header$label == "INDEX_LIST")) {
    print(str(header$attr))
    header$data <- read_niml_data(fconn, header$attr)
    #while (readChar(fconn,1) != ">") { next }
  }
  out <- c()
  STATE <- "BEGIN"
  lastch <- ""
  while(TRUE ) {
    ch <- readChar(fconn,1)
    
    if (length(ch) == 0) {
      break
    } else if (ch == "<" && STATE == "BEGIN") {
      STATE <- "CLOSE_TAG"
    } else if (ch == ">" && STATE == "CLOSE_TAG") {
      ## open header
      STATE <- "END"
      break
    } 
  }
  
  header
  
}

parse_niml_file <- function(fname, maxels=10000) {
  fconn <- file(fname)
  open(fconn, open="rb")
  fsize <- file.info(fname)$size
  out <- list()
  elcount <- 1
  out[[elcount]] <- parse_niml_header(fconn)
  while (seek(fconn, where=NA) < fsize && elcount < maxels) {
    elcount <- elcount + 1
    el <- parse_niml_next(fconn)
    out[[elcount]] <- el
  }
  
  close(fconn)
  
  out
}
