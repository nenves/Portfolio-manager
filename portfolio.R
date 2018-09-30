library(RODBC)
library(zoo)
library(PerformanceAnalytics)
library(tseries)
library(quadprog)

#' Class: Portfolio
setClass(Class="Portfolio",
         representation=representation(id = "ANY",
                                       assets = "ANY",
                                       weights = "ANY",
                                       mean = "ANY",
                                       stddev = "ANY",
                                       means = "ANY",
                                       cov = "ANY",
                                       cor = "ANY",
                                       prices = "ANY",
                                       assetNames = "ANY"
         )
)

#' Method: import
#' Description: Imports prices from a given provider
#setGeneric("import", def = function(object, provider, startDate, endDate, resolution) standardGeneric("importPortfolio"))
setMethod(f="import", signature="Portfolio",definition=
            function(object, provider, startDate, endDate, resolution){
              for (i in 1:length(object@assets)){
                object@assets[[i]]<-import(object@assets[[i]], provider, startDate, endDate, resolution)
                object@prices<-cbind(object@prices,object@assets[[i]]@prices)
                object@assetNames[[i]]<-object@assets[[i]]@id
              }
              return(object)
            }
)

#' Method: calculateStatistics
#' Description: calculates portfolio statistics
setGeneric("calculateStatistics", def = function(object) standardGeneric("calculateStatistics"))
setMethod(f="calculateStatistics", signature="Portfolio",definition=
            function(object){
              priceMatrix = matrix(object@assets[[1]]@ccReturns,length(object@assets[[1]]@ccReturns),1)
              for (i in 2:length(object@assets)){
                priceMatrix<-cbind(priceMatrix,matrix(object@assets[[i]]@ccReturns,length(object@assets[[i]]@ccReturns),1))
              }
              object@means<-mean(priceMatrix,na.rm=TRUE)
              object@cov<-cov(priceMatrix,use="complete.obs")
              object@cor<-cor(priceMatrix,use="complete.obs")
              return(object)
            }
)

#' Method: calculateMinVarianceGlobal
#' Description: calculates global minimum variance portfolio
setGeneric("getMinVarianceGlobal", def = function(object, shortSalesAllowed) standardGeneric("getMinVarianceGlobal"))
setMethod(f="getMinVarianceGlobal", signature="Portfolio",definition=
            function(object,shortSalesAllowed){

              # solve for min w'*Sigma*w
              numberOfAssets<-length(object@assets)
              D<-2*object@cov
              d<-matrix(rep(0,numberOfAssets),numberOfAssets,1)
              Aeq<-matrix(rep(1,numberOfAssets),numberOfAssets,1)
              beq<-1
              Aneq<-diag(numberOfAssets)
              bneq<-matrix(rep(0,numberOfAssets), numberOfAssets,1)

              if (shortSalesAllowed){
                A = Aeq
                b = beq
              }else{
                A = cbind(Aeq,Aneq)
                b = rbind(beq,bneq)
              }

              optimResult<-solve.QP(D,d,A,b,1)
              object@weights<-optimResult$solution
              object@mean<-object@weights%*%object@means
              object@stddev<-sqrt(t(object@weights)%*%object@cov%*%(object@weights))
              object@id<-"GMVP"
              return(object)
            }
)

#' Method: getMinVarianceForTarget
#' Description: calculates minimum variance portfolio for target return
setGeneric("getMinVarianceForTarget", def = function(object, shortSalesAllowed, targetReturn) standardGeneric("getMinVarianceForTarget"))
setMethod(f="getMinVarianceForTarget", signature="Portfolio",definition=
            function(object,shortSalesAllowed, targetReturn){

              # solve for min w'*Sigma*w
              numberOfAssets<-length(object@assets)
              D<-2*object@cov
              d<-matrix(rep(0,numberOfAssets),numberOfAssets,1)
              Aeq<-cbind(object@means, matrix(rep(1,numberOfAssets),numberOfAssets,1))
              beq<-rbind(targetReturn, 1)
              Aneq<-diag(numberOfAssets)
              bneq<-matrix(rep(0,numberOfAssets), numberOfAssets,1)

              if (shortSalesAllowed){
                A = Aeq
                b = beq
              }else{
                A = cbind(Aeq,Aneq)
                b = rbind(beq,bneq)
              }

              optimResult<-solve.QP(D,d,A,b,2)
              object@weights<-optimResult$solution
              object@mean<-object@weights%*%object@means
              object@stddev<-sqrt(t(object@weights)%*%object@cov%*%(object@weights))
              object@id<-"MIN RISK TARGET GAIN"
              return(object)
            }
)


#' Method: showAll
#' Description: plots all assets
setGeneric("showAll", def = function(object) standardGeneric("showAll"))
setMethod(f="showAll", signature="Portfolio",definition=
            function(object){
              numberOfAssets<-length(object@assets)
              ids<-object@assets[[1]]@id
              for (i in 2:numberOfAssets){
                ids<-cbind(ids,object@assets[[i]]@id)
              }
              x = rbind(matrix(sqrt(diag(object@cov)),numberOfAssets,1),matrix(object@stddev,1,1))
              y = rbind(100*matrix(object@means,numberOfAssets,1),100*matrix(object@mean,1,1))
              plot(x, y, col = "blue", main = "Expected daily return in % vs. Risk", xlab = "Risk", ylab = "Expected daily return in %", xlim = c(-0.002,max(x)), ylim = c(-0.01,max(y)))
              text(x, y, cbind(ids,object@id), pos = 2)

            }
)

#' Method: getTangencyPortfolio
#' Description: finds tangency portfolio
setGeneric("getTangencyPortfolio", def = function(object, shortSalesAllowed, riskFreeReturn) standardGeneric("getTangencyPortfolio"))
setMethod(f="getTangencyPortfolio", signature="Portfolio",definition=
            function(object, shortSalesAllowed, riskFreeReturn){
              maxSharp<-0
              for (targetReturn in seq(0.01,0.2,0.001)){

                # solve for min w'*Sigma*w
                numberOfAssets<-length(object@assets)
                D<-2*object@cov
                d<-matrix(rep(0,numberOfAssets),numberOfAssets,1)
                Aeq<-cbind(object@means, matrix(rep(1,numberOfAssets),numberOfAssets,1))
                beq<-rbind(targetReturn, 1)
                Aneq<-diag(numberOfAssets)
                bneq<-matrix(rep(0,numberOfAssets), numberOfAssets,1)

                if (shortSalesAllowed){
                  A = Aeq
                  b = beq
                }else{
                  A = cbind(Aeq,Aneq)
                  b = rbind(beq,bneq)
                }

                optimResult<-solve.QP(D,d,A,b,2)
                curWeights<-optimResult$solution
                curMean<-curWeights%*%object@means
                curStddev<-sqrt(t(curWeights)%*%object@cov%*%(curWeights))
                if ((abs(curMean-riskFreeReturn)/curStddev)>maxSharp){
                  maxSharp<-abs(curMean-riskFreeReturn)/curStddev
                  maxWeights<-curWeights
                  maxMean<-curMean
                  maxStddev<-curStddev
                }
              }

              object@weights<-maxWeights
              object@mean<-maxMean
              object@stddev<-maxStddev
              object@id<-"Tangency Portfolio"
              return(object)
            }
)
