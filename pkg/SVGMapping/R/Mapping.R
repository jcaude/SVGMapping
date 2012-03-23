## Copyright (c) 2011, CEA DSV/iBiTecS
## All rights reserved.
## 
## Redistribution and use in source and binary forms, with or without modification,
## are permitted provided that the following conditions are met:
## 
## * Redistributions of source code must retain the above copyright notice, this list
##   of conditions and the following disclaimer.
## 
## * Redistributions in binary form must reproduce the above copyright notice, this
##   list of conditions and the following disclaimer in the documentation and/or
##   other materials provided with the distribution.
## 
## THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
## ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
## WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
## DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR
## ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
## (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
## LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
## ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
## (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
## SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

## M A P P I N G  (virtual)
## --------------------------------------------------
library(methods)

setGenericVerif <- function(name,y){if(!isGeneric(name)){setGeneric(name,y)}else{}}

MAPPING.Identity <- function(x,p) { return(x) }
MAPPING.Linear <- function(x,p) { return(p$a*x+p$b) }
MAPPING.RangeLinear <- function(x,p) {return(min(p$max, max(p$min,p$a*x+p$b))) } 
MAPPING.Logistic <- function(x,p) { return(p$K/(1+p$a*exp(-p$r*x))) }
MAPPING.Sigmoid <- function(x,p) { return(1/(1+exp(-p$r*x))) }

setClass("Mapping",
         representation(data.source="ANY",
                        data.col.id="ANY",
                        fn="ANY",
                        fn.parameters="list",
                        animation="logical",
                        "VIRTUAL"
                        )
         )

setGenericVerif(name="dataSource", function(object) { standardGeneric("dataSource") })
setGenericVerif(name="dataSource<-", function(.Object,value) { standardGeneric("dataSource<-") })
setGenericVerif(name="dataColumnID", function(object) { standardGeneric("dataColumnID") })
setGenericVerif(name="dataColumnID<-", function(.Object,value) { standardGeneric("dataColumnID<-") })
setGenericVerif(name="getFunction", function(object) { standardGeneric("getFunction") })
setGenericVerif(name="getFunctionParams", function(object) { standardGeneric("getFunctionParams") })
setGenericVerif(name="fnIdentity", function(.Object) { standardGeneric("fnIdentity") })
setGenericVerif(name="fnLinear", function(.Object,a,b) { standardGeneric("fnLinear") })
setGenericVerif(name="fnRangeLinear", function(.Object,a,b,min,max) { standardGeneric("fnRangeLinear") })
setGenericVerif(name="fnLogistic", function(.Object,K,a,r) { standardGeneric("fnLogistic") })
setGenericVerif(name="fnSigmoid", function(.Object,r) { standardGeneric("fnSigmoid") })
setGenericVerif(name="fnUser", function(.Object,fn,fn.params) { standardGeneric("fnUser") })


setMethod(f="initialize", signature="Mapping",
          definition=function(.Object,...)
          {
            ## init.
            .Object@data.source <- NULL
            .Object@data.col.id <- 1
            .Object@fn <- NULL
            .Object@fn.parameters <- list()
            .Object@animation <- FALSE

            ## eop
            return(.Object)
          }
          )

setMethod(f="dataSource", signature="Mapping",
          definition=function(object)
          {
            return(object@data.source)
          }
          )
          
setReplaceMethod(f="dataSource", signature="Mapping",
                 definition=function(.Object,value)
                 {
                   ## !!!!! CHECKING ??
                   ## eop
                   .Object@data.source <- value
                   return(.Object)
                 }
                 )

setMethod(f="dataColumnID", signature="Mapping",
          definition=function(object)
          {
            return(object@data.col.id)
          }
          )
          
setReplaceMethod(f="dataColumnID", signature="Mapping",
                 definition=function(.Object,value)
                 {
                   ## !!!!! CHECKING ??
                   ## eop
                   .Object@data.col.id <- value
                   return(.Object)
                 }
                 )

setMethod(f="getFunction", signature="Mapping",
          definition=function(object)
          {
            return(object@fn)
          }
          )
          
setMethod(f="getFunctionParams", signature="Mapping",
          definition=function(object)
          {
            return(object@fn.parameters)
          }
          )

setMethod(f="fnIdentity", signature="Mapping",
          definition=function(.Object)
          {
            ## init.
            namedOjbect <- deparse(substitute(.Object))

            ## update
            .Object@fn <- MAPPING.Identity
            .Object@fn.parameters <- list()
            
            ## eop
            assign(namedOjbect, .Object, envir=parent.frame())
          }
          )
          
setMethod(f="fnLinear", signature="Mapping",
          definition=function(.Object,a,b)
          {
            ## init.
            namedOjbect <- deparse(substitute(.Object))
            a <- if(missing(a)) 1 else a
            b <- if(missing(b)) 0 else b

            ## check
            if(!is.numeric(a) || !is.numeric(b))
              stop("linear (a*x+b) parameters 'a' and 'b' must be numeric") 

            ## update
            .Object@fn <- MAPPING.Linear
            .Object@fn.parameters <- list(a=a,b=b)
            
            ## eop
            assign(namedOjbect, .Object, envir=parent.frame())
          }
          )

setMethod(f="fnRangeLinear", signature="Mapping",
          definition=function(.Object,a,b,min,max)
          {
            ## init.
            namedOjbect <- deparse(substitute(.Object))
            a <- if(missing(a)) 1 else a
            b <- if(missing(b)) 0 else b
            min <- if(missing(min)) -Inf else min
            max <- if(missing(max)) Inf else max

            ## check
            if(!is.numeric(a) || !is.numeric(b) || !is.numeric(min) || !is.numeric(max))
              stop("Range linear [min,(a*x+b),max] parameters 'a','b','min' and 'max' must be numeric") 

            ## update
            .Object@fn <- MAPPING.Linear
            .Object@fn.parameters <- list(a=a,b=b,min=min,max=max)
            
            ## eop
            assign(namedOjbect, .Object, envir=parent.frame())
          }
          )

setMethod(f="fnLogistic", signature="Mapping",
          definition=function(.Object,K,a,r)
          {
            ## init.
            namedOjbect <- deparse(substitute(.Object))
            K <- if(missing(K)) 1 else K
            a <- if(missing(a)) 1 else a
            r <- if(missing(r)) 1 else r

            ## check
            if(!is.numeric(K) || !is.numeric(a) || !is.numeric(r))
              stop("logistic (K/(1+a*exp(-rx)) parameters 'K','a' and 'r' must be numeric") 

            ## update
            .Object@fn <- MAPPING.Logistic
            .Object@fn.parameters <- list(K=K,a=a,r=r)
            
            ## eop
            assign(namedOjbect, .Object, envir=parent.frame())
          }
          )

setMethod(f="fnSigmoid", signature="Mapping",
          definition=function(.Object,r)
          {
            ## init.
            namedOjbect <- deparse(substitute(.Object))
            r <- if(missing(r)) 1 else r

            ## check
            if(!is.numeric(r))
              stop("Sigmoid (1/(1+exp(-rx)) parameter 'r' must be numeric") 

            ## update
            .Object@fn <- MAPPING.Sigmoid
            .Object@fn.parameters <- list(r=r)
            
            ## eop
            assign(namedOjbect, .Object, envir=parent.frame())
          }
          )

setMethod(f="fnUser", signature="Mapping",
          definition=function(.Object,fn,fn.params)
          {
            ## check
            if(missing(fn) || !is.function(fn))
              stop("You must provide a valid function 'fn'")           
            if(missing(fn.params) || !is.list(fn.params))
              stop("You must provide a valid list of parameters") 
            
            ## init.
            namedOjbect <- deparse(substitute(.Object))          

            ## update
            .Object@fn <- fn
            .Object@fn.parameters <- fn.params
            
            ## eop
            assign(namedOjbect, .Object, envir=parent.frame())
          }
          )
