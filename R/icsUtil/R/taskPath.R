# this function provides the function get taskPath

getTaskPath <- function(){
  matdatapath = "D:/work/data"
  rdatapath = "D:/proj/data"
  tp = list(matdatapath,rdatapath)
  names(tp)=c('matdatapath','rdatapath')
  return(tp)
}