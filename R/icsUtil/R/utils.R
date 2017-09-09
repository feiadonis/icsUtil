head1 <- function(x) head(x,1)
tail1 <- function(x) tail(x,1)
max.na <- function(x) { xna = is.na(x); if(all(xna)){NA} else {max(x,na.rm=TRUE)}}
mean.na <- function(x) {xna = is.na(x); if(all(xna)){NA} else {mean(x,na.rm=TRUE)}}
min.na <- function(x){ xna = is.na(x); if(all(xna)){NA} else {min(x, na.rm = TRUE)}}
null.replace <- function(x,v) if(is.null(x)){v}else{x}

rep.row <- function(x,n) matrix(rep(x,each=n),nrow=n)
rep.col <- function(x,n) matrix(rep(x,each=n),ncol=n,byrow=TRUE)

getMktCfg <- function( mkt = "CHINA_STOCK"){
  if(mkt %in% c("CHINA_STOCK","CHINA_FUTURE")){
    tdpath = file.path("D:/proj/data/tradingDays","CHINA_STOCK")
  }else{
    tdpath = file.path("D:/proj/data/tradingDays",mkt)
  }
  tdfilename = file.path(tdpath,'tradingDays.rds')
  td = readRDS(tdfilename)
  #load("D:/proj/ics/icsUtils/.RData")
  CHINA_STOCK <- list(
    tradingDays=td$D,
    tradingWeeks=td$W,
    tradingMonths=td$M,
    tradingYears=td$Y,
    fre = "DAILY",
    zero.valid=FALSE
  )
  CHINA_FUTURE <- list(
    tradingDays=td$SSE_TRADING_DAYS,
    tradingWeeks=td$SSE_TRADING_WEEKS,
    tradingMonths=td$SSE_TRADING_MONTHS,
    tradingYears=td$SSE_TRADING_YEARS,
    freq = "DAILY",
    zero.valid = FALSE
  )
  switch(mkt,CHINA_STOCK=CHINA_STOCK, CHINA_FUTURE=CHINA_FUTURE,stop())
}

getTradingDay <- function(d, offsets=0, mod = "D",mkt="CHINA_STOCK"){
  tmp = getMktCfg(mkt=mkt)
  mod = toupper(mod)
  if( mod == "D") td <- tmp$tradingDays
  else if( mod == 'W') td <- tmp$tradingWeeks
  else if( mod == 'M') td <- tmp$tradingMonths
  else if( mod == 'Y') td <- tmp$tradingYears
  else stop('error with mod!')
  idx <- tail1(which(td <= d))
  td[idx+offsets]
}

getTradingDayRange <- function(sd,ed,mod = "D",mkt="CHINA_STOCK"){
  tmp = getMktCfg(mkt=mkt)
  mod = toupper(mod)
  if( mod == "D") td <- tmp$tradingDays
  else if( mod == 'W') td <- tmp$tradingWeeks
  else if( mod == 'M') td <- tmp$tradingMonths
  else if( mod == 'Y') td <- tmp$tradingYears
  else stop('error with mod!')
  td[ which(td >= sd & td <= ed)]
}

panel.add.dim <- function(panel,label,name){
  dnames = dimnames(panel)
  labels = names(dnames)
  dims = dim(panel)
  dim(panle) = c(dims,1)
  dnames[[length(dnames)+1]] = name
  if(!is.null(labels))names(dnames) = c(labels,label)
  dimnames(panel)=dnames
  panel
}


panel.combine <- function(p.list, default=NA){
  library(data.table)
  p.list=p.list[!unlist(laply(p.list,is.null))]
  if (length(p.list)==1) return(p.list[[1]])
  new.dimnames = llply(1:length(dim(p.list[[1]])),
                       function(d) unique(abind(llply(p.list,function(x) dimnames(x)[[d]]),along=1)))
  panel = array(default,dim=laply(new.dimnames,length),dimnames=new.dimnames)
  for(pi in p.list){
    if(length(dimnames(pi))==4){
      panel[dimnames(pi)[[1]],dimnames(pi)[[2]],dimnames(pi)[[3]],dimnames(pi)[[4]]]=pi
    }else{
      idx = as.matrix(do.call(data.table::CJ,dimnames(pi)))
      panel[idx]=pi[idx]
      }
  }
  labels = names(dimnames(p.list[[1]]))
  names(dimnames(panel)) = labels
  panel
}

getfdDateRange <- function(stDate,edDate){
  y1 = as.integer(format(as.Date(as.character(stDate),"%Y%m%d"),"%Y"))
  m1 = as.integer(format(as.Date(as.character(stDate),"%Y%m%d"),"%m"))
  d1 = as.integer(format(as.Date(as.character(stDate),"%Y%m%d"),"%d"))
  
  y2 = as.integer(format(as.Date(as.character(edDate),"%Y%m%d"),"%Y"))
  m2 = as.integer(format(as.Date(as.character(edDate),"%Y%m%d"),"%m"))
  d2 = as.integer(format(as.Date(as.character(edDate),"%Y%m%d"),"%d"))
  
  if(y1 > y2 || (y1 <= y2 && m1 > m2) || (y1 <=y2 && m1 <= m2 && d1 > d2))return(NULL)
  if (m1 >=10) q1 = c("1231")
  else if(m1 >= 7) q1 = c("0930","1231")
  else if(m1 >= 4) q1 = c("0630","0930","1231")
  else q1 = c("0331","0630","0930","1231")
  
  if(m2 < 4) q2 = NULL
  else if(m2 < 7) q2 = c("0331")
  else if(m2 < 10) q2 = c("0331","0630")
  else q2 = c("0331","0630","0930")
  
  q3 = c("0331","0630","0930","1231")
  
  qy1 = paste(y1,q1,sep = "")
  if (is.null(q2)) qy2 = NULL
  else qy2 = paste(y2,q2,sep = "")
  
  if (y1 == y2) return(intersect(qy1,qy2))
  else if(y2 == y1 + 1) return(union(qy1,qy2))
  else{
    data = qy1
    for ( y in (y1 +1):(y2-1)){
      data = c(data,paste(y,q3,sep=""))
    }
    data = c(data,qy2)
    return(data)
  }
}


