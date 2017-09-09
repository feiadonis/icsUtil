library(R.matlab)

# now we trans date range data
dataNames = c("md","fdd","industry","csi300","sz50","zz500","zz800andsmb","zz800",
              "zz500andsmb","stComponent")

mat2h5.drange <- function(tradingDays,dnames = dataNames, verbose=TRUE,overwrite=TRUE){
  for( dataName in dnames){
    for( td in tradingDays){
      mat2h5(dataName,td,verbose=verbose,overwrite = overwrite)
    }
  }

  #gen.fwd.value(tradingDays, c(-20,-10,-5:-1,1:5,10,15,20,40,60))
}



innerCode2secuCode <- function(innerCodes, mkt="CHINA_STOCK"){
  # here innerCodes are list of ints
  secuCodes = NULL
  if(mkt=="CHINA_STOCK"){
    for ( i in innerCodes){
      tmp = as.character(i)
      if (substr(tmp,1,1)=="1") mktflag="SH"
      else if (substr(tmp,1,1)=="2") mktflag="SZ"
      else (stop("Somthing wrong with innerCode!"))

      secuCode = paste(substr(tmp,2,7),mktflag,sep=".")
      secuCodes = c(secuCodes,secuCode)
    }
  }
  secuCodes
}

mat2h5 <- function(windName, marketDate,verbose=TRUE,overwrite=TRUE){
  # transform mat data to h5 data
  dataName = windName
  mkt="CHINA_STOCK"
  freq="DAILY"
  vers="wind"

  if (windName == 'md' && mkt == "CHINA_STOCK"){
    dfolder = 'dailyData'
    factorNames = c('openPrice','highPrice','lowPrice','closePrice',
                     'preclosePrice','volume','amount','pctchange','turnover',
                     'adjFactor','cumsusdays','freefloatshares','totalshares',
                     'totalmktcap','freefloatmktcap','csi300weights','zz500weights',
                     'sz50weights','zz800weights')

  } else if( windName == 'fdd'){
    dfolder = 'dailyData'
    factorNames = c('pettm','pblf','pcfttm','psttm','dividendYieldTTM')

  } else if( windName == 'industry'){
    dfolder = "industryData"
    factorNames = "industryClass"
  } else if( windName == 'csi300'){
    dataName = 'univ'
    dfolder = "sectorComponent"
    factorNames = 'csi300'
    vers = windName
  } else if( windName == 'sz50'){
    dataName = 'univ'
    dfolder = 'sectorComponent'
    factorNames = 'sz50'
    vers = windName
  } else if( windName == 'zz500'){
    dataName = 'univ'
    dfolder = 'sectorComponent'
    factorNames = 'zz500'
    vers = windName
  } else if( windName == 'zz800'){
    dataName = 'univ'
    dfolder = 'sectorComponent'
    factorNames = 'zz800'
    vers = windName
  } else if ( windName == 'zz500andsmb'){
    dataName = 'univ'
    dfolder = 'sectorComponent'
    factorNames = 'zz500andsmb'
    vers = windName
  } else if( windName == 'stComponent'){
    dataName = "univ"
    dfolder = "sectorComponent"
    factorNames = 'stComponent'
    vers = windName
  } else if( windName == 'zz800andsmb'){
    dataName = 'univ'
    dfolder = 'sectorComponent'
    factorNames = 'zz800andsmb'
    vers = windName
  } else if (windName == "fdRptAnalysis"){
    dataName = "fdd"
    dfolder = 'financialData'
    freq = "Qtrly"
    vers = "rptAnalysis"
    factorNames = c('roe','roa','roic','netProfitMargin','grossProfitMargin','debtToAssets',
                    'assetsToEquity','currentRatio','quickRatio','totalRevenueYOY',
                    'netProfitYOY','ocfToSales','ocfYOY','roeYOY')
  } else if (windName == "fdRpt"){

  }

  tp = getTaskPath()
  dlist = llply(factorNames, function(factorName){
    if (dfolder == "industryData"){
      fileName = file.path(tp$matdatapath,dfolder,factorName,paste(getTradingDay(marketDate,0,'M'),'mat',sep="."))
    }else{
      fileName = file.path(tp$matdatapath, dfolder, factorName, paste(marketDate,'mat',sep="."))
    }

    data = readMat(fileName)
    if (dataName == 'univ'){
      tmpdata = data[['innerCodes']]
      secuCodes = innerCode2secuCode(tmpdata)
      data = array(1,dim = c(length(tmpdata),1,1,1),dimnames = list(secuCodes,as.character(marketDate),"15:00:00.000",factorName))
    } else{
        tmpdata = data[[factorName]]
        secuCodes = innerCode2secuCode(tmpdata[,1])
        data = array(tmpdata[,2],dim = c(length(tmpdata[,1]),1,1,1),dimnames = list(secuCodes,as.character(marketDate),"15:00:00.000",factorName))
      }
    data
  })
  data = panel.combine(dlist)
  names(dimnames(data)) = c('K','D','T','V')
  resultPath = file.path(tp$rdatapath,dataName,mkt,freq,vers)
  if (!dir.exists(resultPath)) dir.create(resultPath,recursive = TRUE)
  resultName = file.path(resultPath, paste(marketDate,'h5',sep="."))
  panel.write(data,resultName,overwrite = overwrite,verbose=verbose)
  gc()
}



gen.fwd.value <-function(tradingDays,periods,mkt="CHINA_STOCK",var = "Ret", freq="DAILY"){
  data = readicsdata(tradingDays,V.name=c('closePrice','adjFactor'))
  tp = getTaskPath()
  for ( p in periods){
    if ( p > 0){
      resultPath = file.path(tp$rdatapath,'fwdbwd',mkt,freq,paste('fwd',p,sep="_"))
      if(!dir.exists(resultPath))dir.create(resultPath,recursive = TRUE)
      for ( t in 1:(length(tradingDays)-p)){
        tmp = (data[,t+p,1,'closePrice',drop=FALSE]*data[,t+p,1,'adjFactor',drop=FALSE])/(data[,t,1,'closePrice',drop=FALSE]*data[,t,1,'adjFactor',drop=FALSE])-1
        dimnames(tmp)[['V']] = value2name(p,var = var, freq=freq)
        dimnames(tmp)[['D']] = as.character(tradingDays[t])
        resultName=file.path(resultPath,paste(as.character(tradingDays[t]),'h5',sep="."))
        panel.write(tmp,resultName,overwrite = TRUE)
      }
    }else{
      resultPath = file.path(tp$rdatapath,'fwdbwd',mkt,freq,paste('bwd',-p,sep="_"))
      if(!dir.exists(resultPath))dir.create(resultPath,recursive = TRUE)
      for ( t in (-p+1):length(tradingDays)){
        tmp = (data[,t,1,'closePrice',drop=FALSE]*data[,t,1,'adjFactor',drop=FALSE])/(data[,t+p,1,'closePrice',drop=FALSE]*data[,t+p,1,'adjFactor',drop=FALSE])-1
        dimnames(tmp)[['V']] = value2name(p,var = var, freq=freq)
        dimnames(tmp)[['D']] = as.character(tradingDays[t])
        resultName=file.path(resultPath,paste(as.character(tradingDays[t]),'h5',sep="."))
        panel.write(tmp,resultName,overwrite = TRUE)
      }

    }
  }
  gc()

}
