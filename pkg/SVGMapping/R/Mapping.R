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
MAPPING.Logistic <- function(x,p) { return(p$K/(1+p$a*exp(-p$r*x))) }

setClass("Mapping",
         representation(data.source="ANY",
                        map.fn="ANY",
                        map.fn.parameters="list",
                        animation="logical",
                        "VIRTUAL"
                        )
         )

setGenericVerif(name="dataSource", function(object) { standardGeneric("dataSource") })
setGenericVerif(name="dataSource<-", function(.Object,value) { standardGeneric("dataSource<-") })
setGenericVerif(name="mapFunction", function(object) { standardGeneric("mapFunction") })
setGenericVerif(name="mapFunction<-", function(.Object,value) { standardGeneric("mapFunction<-") })

setMethod(f="initialize", signature="Mapping",
          definition=function(.Object,value)
          {
            ## check.
            if(!is(value,"SVG")) stop("'value' must be an SVG object")

            ## init.
            .Object@svg <- value
            .Object@data.source <- NULL
            .Object@map.fn <- NULL
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

setMethod(f="mapFunction", signature="Mapping",
          definition=function(object)
          {
            return(object@map.fn)
          }
          )
          
setReplaceMethod(f="mapFunction", signature="Mapping",
                 definition=function(.Object,value)
                 {
                   ## !!!!! CHECKING ??
                   ## eop
                   .Object@map.fn <- value
                   return(.Object)
                 }
                 )
