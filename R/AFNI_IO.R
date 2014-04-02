
#' @keywords internal
parseIntAttribute <- function(line){ 
	as.integer(str_split(str_trim(paste(line, collapse=" ")), "\\s+")[[1]])
}

#' @keywords internal
parseFloatAttribute <- function(line){ 
	as.numeric(str_split(str_trim(paste(line, collapse=" ")), "\\s+")[[1]])
}
	
#' @keywords internal
parseStringAttribute <- function(line) {
	
	res <- str_split(line, "~")[[1]]
	if (length(res) > 1) {
		res[1] <- str_sub(res[1], 2)
		res[1:(length(res)-1)]
	} else {
		str_sub(res[1:(length(res)-1)], 2)	
	}
}
	
#' @keywords internal
parseElement <- function(inputLines) {
	atype <- str_trim(str_split(inputLines[[1]], "=")[[1]])[2]	
	name <- str_trim(str_split(inputLines[[2]], "=")[[1]])[2]	
	count <- str_trim(str_split(inputLines[[3]], "=")[[1]])[2]	
		
	content <- if (atype == "string-attribute") {
		parseStringAttribute(inputLines[4:length(inputLines)])
	} else if (atype == "integer-attribute") {
		parseIntAttribute(inputLines[4:length(inputLines)])
	} else if (atype == "float-attribute") {
		parseFloatAttribute(inputLines[4:length(inputLines)])
	} else {
		stop("unrecognized attribute type ", atype)
	}
	list(type=atype, name=name, count=count, content=content)
	
}

readAFNIHeader <- function(fileName) {
	inputLines <- scan(fileName, what=character(), sep="\n", blank.lines.skip=FALSE)
	idx <- which(unlist(lapply(inputLines, function(lin) lin == ""))) + 1
	lastIdx <- length(inputLines)
	attlen <- diff(c(idx, lastIdx+1))
	attpos <- cbind(idx, attlen)
	
	header <- apply(attpos, 1, function(row) {
		a <- row[[1]]
		b <- row[[2]]
		parseElement(inputLines[a:(a+b-1)])
	})

	names(header) <- unlist(lapply(header, "[[", "name"))
	header
	
}

