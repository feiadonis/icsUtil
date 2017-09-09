library(plyr)
library(abind)
library(stringr)

readicsdata <- function(daterange,dtype="md",mkt="CHINA_STOCK",freq="DAILY",vers="wind",
                        paths=NULL,verbose=FALSE,allowempty=FALSE,V.name=NULL){
  if(!is.null(vers)){
    tp = getTaskPath()
    data.dir=tp$rdatapath
    mdlist1=llply(vers,function(v){
      filePath=file.path(data.dir,dtype,mkt,freq,v,"YYYYMMDD.h5")
      mdlist=llply(daterange,function(td){
        fileName=sub("YYYYMMDD",td,filePath)
        if(allowempty && !file.exists(fileName)){
          md = NULL
        }else{
          md = panel.read(fileName,verbose=verbose,V.name=V.name)
        }
        md
      })
      md=panel.combine(mdlist)
    })} else{mdlist1=NULL}
  
  if(!is.null(paths)){
    mdlist2=llply(paths,function(filePath){
      mdlist=llply(daterange,function(td){
        fileName=sub("YYYYMMDD",td,filePath)
        if(allowempty && !file.exists(fileName)){
          md = NULL
        }else{
          md = panel.read(fileName,verbose=verbose,V.name=V.name)
        }
        md
      })
      md=panel.combine(mdlist)
    })} else{mdlist2=NULL}
   
  mdlist = c(mdlist1,mdlist2)
  return(panel.combine(mdlist))
}

name2value <- function(n){
  s = sub("(.*)\\.(.*)\\.(.*)\\.(.*)","\\1",n)
  var = sub("(.*)\\.(.*)\\.(.*)\\.(.*)","\\2",n)
  freq = sub("(.*)\\.(.*)\\.(.*)\\.(.*)","\\3",n)
  p = as.integer(sub("(.*)\\.(.*)\\.(.*)\\.(.*)","\\4",n))
  if ( s == 'bwd') p = -p
  return(list(s = s, var = var, freq = freq, v = p))
}

value2name <- function(p, var = "Ret", freq ="DAILY"){
  if(p > 0) a = paste("fwd", var, freq, p, sep=".")
  else a = paste("bwd", var, freq, -p, sep=".")
  return(a)
}

readFwd <- function(tradingDays,periods,mkt="CHINA_STOCK",freq="DAILY"){
  mdlist = llply(periods, function(p){
    if ( p > 0) v = paste('fwd',p,sep="_")
    else if(p<0) v= paste('bwd',-p,sep="_")
    md = readicsdata(tradingDays,dtype="fwdbwd",mkt=mkt,freq=freq,vers = v)
  })
  return(panel.combine(mdlist))
}