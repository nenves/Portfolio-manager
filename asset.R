library(RODBC)
library(zoo)
library(PerformanceAnalytics)
library(tseries)
library(quadprog)

#'Class: Asset
setClass(Class="Asset",
		representation=representation(id = "ANY",
                                  prices = "matrix",
                                  ccReturns = "ANY",
                                  mean="ANY",
                                  stddev="ANY")
	)

#'Method: import
#'Description: Imports prices from a given provider
setGeneric("import", def = function(object, provider, startDate, endDate, resolution) standardGeneric("import"))
setMethod(f="import", signature="Asset",definition=
function(object, provider, startDate, endDate, resolution){

  if (provider == "yahoo"){
    tempImport<-coredata(get.hist.quote(instrument=object@id, start=startDate,
                               end=endDate, quote="AdjClose",
                               provider=provider, origin="1970-01-01",
                               compression=resolution, retclass="zoo"))
  }
  else if (substr(provider,1,2) == "DB"){
    ch<-odbcConnect("localSQLServer", uid = "YOUR_USERNAME", pwd = "YOUR_PASSWORD")

    tempImport<-coredata(sqlQuery(ch,
                                  paste(
                                    "select [",
                                    object@id,
                                    "] from AssetValues",
                                    " where Date < ",
                                    "'",endDate,"'",
                                    " and Date > ",
                                    "'",startDate,"'",
                                    " order by Date DESC",
                                    sep = ""
                                  )
                        )
                )
    tempImport
    odbcClose(ch)
  }
  else{
    tempImportAll<-coredata(read.csv(file = provider, header = TRUE, sep=";"))
    gsub("-",".",object@id)
    tempImport<-tempImportAll[gsub("-",".",object@id)]
  }

  object@prices<-as.matrix(tempImport[dim(tempImport)[1]:1,])
  object@ccReturns<-diff(log(object@prices))
  object@mean<-apply(object@ccReturns,2,mean)
  object@stddev<-apply(object@ccReturns,2,sd)

	return(object)
}
)
