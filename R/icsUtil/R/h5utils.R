library(h5)
library(plyr)
library(abind)

panel.write <- function(data, filename, key = "ndarray", verbose=TRUE, 
                       not=NULL, overwrite=FALSE){
  if (!is.array(data)) stop("ONly support array data type")
  if (verbose) print( paste("Writing ",filename))
  file <- h5file(filename,ifelse(overwrite,'w','w-'))
  file[key]=data
  h5attr(file[key],"dim")=dim(data)
  
  dimlabels = names(dimnames(data))
  h5attr(file[key],"dimlabels")=if(is.null(dimlabels)){paste("dim",0:(length(dim(data))-1),sep="_")}else{dimlabels}
  for ( i in 1:length(dim(data))){
    tryCatch(
      h5attr( file[key], paste("dimnames", i,sep="_")) <- dimnames(data)[[i]]
      ,error=function(e){
        file[paste("dimnames",i,sep="_")]<- dimnames(data)[[i]]
      }
    )
  }
  h5close(file)
}

panel.read <- function(filename, key="ndarray",verbose=TRUE, names.only=FALSE,V.name=NULL,V.idx=4){
  if(verbose) print( paste("Reading ", filename))
  file <- h5file( filename,"r")
  labels <- tryCatch( h5attr(file[key],"dimlabels"), error=function(e){paste("dim",0:(length(dnames)-1),sep="_")})
  dnames <- lapply((1:length( h5attr(file[key],"dim"))), function(i) tryCatch( h5attr( file[key], paste("dimnames",i,sep="_")),error=function(e){NULL}))
  
  notfoundidx <- which(unlist(llply(dnames, is.null)))
  if( length(notfoundidx) > 0){
    for ( i in notfoundidx){
      dnames[[i]] = tryCatch( file[paste("dimnames", i, sep="_")][], error=function(e){NULL})
      }
  }
  
  tryCatch(names(dnames) <- labels, error=function(e){print(e)})
  if(names.only){
    data <- dnames
    h5close(file)
  }else{
    if(is.null(V.name)){
      data<-file[key][]
    }else{
      stopifnot(all(V.name %in% dnames[[V.idx]]))
      stopifnot(length(dnames)==4)
      name.idx = which(dnames[[V.idx]]%in%V.name)
      data <- file[key][1:length(dnames[[1]]),1:length(dnames[[2]]),1:length(dnames[[3]]),name.idx]
      dnames[[V.idx]]=dnames[[V.idx]][name.idx]
    }
    dimnames(data) <- dnames
    h5close(file)
  }
  data
}
