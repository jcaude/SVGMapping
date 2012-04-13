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

MAPPING.Random <- function(x,p) { return(runif(1,min=p$min, max=p$max)) }
MAPPING.Identity <- function(x,p) { return(x) }
MAPPING.Linear <- function(x,p) { return(p$a*x+p$b) }
MAPPING.RangeLinear <- function(x,p) {return(min(p$max, max(p$min,p$a*x+p$b))) } 
MAPPING.Logistic <- function(x,p) { return(p$K/(1+p$a*exp(-p$r*x))) }
MAPPING.Sigmoid <- function(x,p) { return(1/(1+exp(-p$r*x))) }
MAPPING.Log2FC <- function(x,p) { return(ifelse(x>=0,exp(x*log(2)),-1/exp(x*log(2)))) }

setClass("Mapping",
         representation(targets="ANY",
                        values="ANY",
                        fn="ANY",
                        fn.parameters="list",
                        animation="logical",
                        .values="ANY",
                        "VIRTUAL"
                        )
         )

setGenericVerif(name="targets", function(object) { standardGeneric("targets") })
setGenericVerif(name="targets<-", function(.Object,value) { standardGeneric("targets<-") })
setGenericVerif(name="values", function(object) { standardGeneric("values") })
setGenericVerif(name="values<-", function(.Object,value) { standardGeneric("values<-") })
setGenericVerif(name="getFunction", function(object) { standardGeneric("getFunction") })
setGenericVerif(name="getFunctionParams", function(object) { standardGeneric("getFunctionParams") })
setGenericVerif(name="setFunction", function(.Object,fn,fn.params) { standardGeneric("setFunction") })
setGenericVerif(name="fnRandom", function(.Object,min,max) { standardGeneric("fnRandom") })
setGenericVerif(name="fnIdentity", function(.Object) { standardGeneric("fnIdentity") })
setGenericVerif(name="fnLinear", function(.Object,a,b) { standardGeneric("fnLinear") })
setGenericVerif(name="fnRangeLinear", function(.Object,a,b,min,max) { standardGeneric("fnRangeLinear") })
setGenericVerif(name="fnLogistic", function(.Object,K,a,r) { standardGeneric("fnLogistic") })
setGenericVerif(name="fnSigmoid", function(.Object,r) { standardGeneric("fnSigmoid") })
setGenericVerif(name="fnLog2FC", function(.Object) { standardGeneric("fnLog2FC") })
setGenericVerif(name="fnUser", function(.Object,fn,fn.params) { standardGeneric("fnUser") })
setGenericVerif(name="fnNone", function(.Object) { standardGeneric("fnNone") })
setGenericVerif(name="exec", function(.Object,svg) { standardGeneric("exec") })

setMethod(f="initialize", signature="Mapping",
          definition=function(.Object,...)
          {
            ## default init.
            .Object@targets <- NULL
            .Object@values <- NULL
            .Object@fn <- NULL
            .Object@fn.parameters <- list()
            .Object@animation <- logical()
            
            ## eop
            return(.Object)
          }
          )

setMethod(f="targets", signature="Mapping",
          definition=function(object)
          {
            return(object@targets)
          }
          )
          
setReplaceMethod(f="targets", signature="Mapping",
                 definition=function(.Object,value)
                 {
                   ## !!!!! CHECKING ??
                   ## eop
                   .Object@targets <- value
                   return(.Object)
                 }
                 )

setMethod(f="values", signature="Mapping",
          definition=function(object)
          {
            return(object@values)
          }
          )
          
setReplaceMethod(f="values", signature="Mapping",
                 definition=function(.Object,value)
                 {
                   ## transform
                   if(is.list(value)) value <- data.frame(value)
                   
                   ## eop
                   .Object@values <- value
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

setMethod(f="setFunction", signature="Mapping",
          definition=function(.Object,fn,fn.params)
          {
            ## init.
            namedOjbect <- deparse(substitute(.Object))
            
            ## select transform function
            if(missing(fn)) {
              fnIdentity(.Object)
            }
            else {
              ## check
              if(!(is.character(fn) || is.function(fn))) {
                stop("Invalid 'fn' argument, must be 'character' or 'function'")
              }
              if(!missing(fn.params) && !is.list(fn.params)) {
                stop("Invalid 'fn.params' argument, must be a list")
              }

              ## fill
              if(is.function(fn)) {
                fnUser(mapc,fn,fn.params)
              }
              else {
                fn <- tolower(fn)
                if(fn=="none") {
                  fnNone(.Object)
                }
                else if(fn=="random") {
                  if(missing(fn.params)) fn.params <- list(min=0,max=1)
                  fnRandom(.Object,fn.params$min, fn.params$max)
                }
                else if(fn=="identity") fnIdentity(mapO)
                else if(fn=="linear") {
                  if(missing(fn.params)) fn.params <- list(a=1,b=0)
                  fnLinear(.Object, fn.params$a, fn.params$b)
                }
                else if(fn=="rangelinear") {
                  if(missing(fn.params)) fn.params <- list(a=1,b=0,min=0,max=1)
                  fnRangeLinear(.Object,
                                fn.params$a, fn.params$b,
                                fn.params$min, fn.params$max)
                }
                else if(fn=="logistic") {
                  if(missing(fn.params)) fn.params <- list(K=1,a=1,r=1)
                  fnLogistic(.Object, fn.params$K, fn.params$a, fn.params$r)
                }
                else if(fn=="sigmoid") {
                  if(missing(fn.params)) fn.params <- list(r=1)
                  fnLogistic(.Object, fn.params$r)
                }
                else if(fn=="log2fc") {
                  fnLog2FC(.Object)
                }
                else 
                  stop("Invalid 'fn' name=",fn,", either: None, Random, Identity, ",
                       "Linear, RangeLinear, Logistic, Sigmoid or Log2FC")
              }
            }

            ## eop
            assign(namedOjbect, .Object, envir=parent.frame())
            return(invisible(.Object))
          }
          )

setMethod(f="fnRandom", signature="Mapping",
          definition=function(.Object,min,max)
          {
            ## init.
            namedOjbect <- deparse(substitute(.Object))
            min <- if(missing(min)) 0 else as.numeric(min)
            max <- if(missing(max)) 1 else as.numeric(max)

            ## check
            if(!is.numeric(min) || !is.numeric(max))
              stop("Random generator, parameters 'min' and 'max' must be numeric") 
            
            ## update
            .Object@fn <- MAPPING.Random
            .Object@fn.parameters <- list(min=min, max=max)
            
            ## eop
            assign(namedOjbect, .Object, envir=parent.frame())
            return(invisible(.Object))
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
            return(invisible(.Object))
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
            return(invisible(.Object))
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
            return(invisible(.Object))
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
            return(invisible(.Object))
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
            return(invisible(.Object))
          }
          )

setMethod(f="fnLog2FC", signature="Mapping",
          definition=function(.Object)
          {
            ## init.
            namedOjbect <- deparse(substitute(.Object))

            ## update
            .Object@fn <- MAPPING.Log2FC
            .Object@fn.parameters <- list()
            
            ## eop
            assign(namedOjbect, .Object, envir=parent.frame())
            return(invisible(.Object))
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
            return(invisible(.Object))
          }
          )

setMethod(f="fnNone", signature="Mapping",
          definition=function(.Object)
          {
            ## init.
            namedOjbect <- deparse(substitute(.Object))

            ## update
            .Object@fn <- NULL
            .Object@fn.parameters <- list()
            
            ## eop
            assign(namedOjbect, .Object, envir=parent.frame())
            return(invisible(.Object))            
          }
          )

setMethod(f="exec", signature="Mapping",
          definition=function(.Object,svg)
          {
            ## init.
            namedOjbect <- deparse(substitute(.Object))          

            ## 'none' case
            if(is.null(.Object@fn))
              .Object@.values <- .Object@values

            ## apply function to values and update object.
            else {
              if(is.list(.Object@values) || is.vector(.Object@values)) 
                .Object@.values <- sapply(.Object@values,
                                          function(x) { sapply(x, .Object@fn, .Object@fn.parameters) }
                                          )
              else
                .Object@.values <- apply(.Object@values, c(1,2),
                                         function(x) { sapply(x, .Object@fn, .Object@fn.parameters) }
                                         )
            }

            ## eop
            assign(namedOjbect, .Object, envir=parent.frame())
            return(invisible(.Object))
          }
          )

  
