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

## M A P P I N G   -   C O L O R S
## --------------------------------------------------

setGenericVerif <- function(name,y){if(!isGeneric(name)){setGeneric(name,y)}else{}}

setClass("MappingColors",
         representation(target.attribute="character",
                        map.colors="vector",
                        colrange.min="numeric",
                        colrange.max="numeric"
                        ),
         contains="Mapping"
         )

setGenericVerif(name="targetAttribute", function(object) { standardGeneric("targetAttribute") })
setGenericVerif(name="targetAttribute<-", function(.Object,value) { standardGeneric("targetAttribute<-") })
setGenericVerif(name="mapColors", function(object) { standardGeneric("mapColors") })
setGenericVerif(name="mapColors<-", function(.Object,value) { standardGeneric("mapColors<-") })
setGenericVerif(name="colrange.min", function(object) { standardGeneric("colrange.min") })
setGenericVerif(name="colrange.min<-", function(.Object,value) { standardGeneric("colrange.min<-") })
setGenericVerif(name="colrange.max", function(object) { standardGeneric("colrange.max") })
setGenericVerif(name="colrange.max<-", function(.Object,value) { standardGeneric("colrange.max<-") })
setGenericVerif(name="colrange", function(object) { standardGeneric("colrange") })
setGenericVerif(name="colrange<-", function(.Object,value) { standardGeneric("colrange<-") })
setGenericVerif(name="exec", function(.Object,svg) { standardGeneric("exec") })

setMethod(f="initialize", signature="MappingColors",
          definition=function(.Object,...)
          {            
            ## super
            .Object <- callNextMethod()

            ## detault init.
            .Object@map.colors <- vector()
            .Object@colrange.min <- 0
            .Object@colrange.max <- 1
            .Object@target.attribute <- character()

            ## eop
            return(.Object)
          }
          )

setMethod(f="targetAttribute", signature="MappingColors",
          definition=function(object)
          {
            return(object@target.attribute)
          }
          )

setReplaceMethod(f="targetAttribute", signature="MappingColors",
                 definition=function(.Object,value)
                 {
                   ## check
                   if(!is.character(value))
                     stop("Target attribute 'value' must be a string")

                   ## init.
                   .Object@target.attribute <- value
                   return(.Object)
                 }
                 )

setMethod(f="mapColors", signature="MappingColors",
          definition=function(object)
          {
            return(object@map.colors)
          }
          )

setReplaceMethod(f="mapColors", signature="MappingColors",
                 definition=function(.Object,value)
                 {
                   ## check
                   if(!is.vector(value))
                     stop("Colors 'value' must be a vector")

                   ## init.
                   .Object@map.colors <- unlist(value)
                   return(.Object)
                 }
                 )

setMethod(f="colrange.min", signature="MappingColors",
          definition=function(object)
          {
            return(object@colrange.min)
          }
          )

setReplaceMethod(f="colrange.min", signature="MappingColors",
                 definition=function(.Object,value)
                 {
                   ## check
                   if(!is.numeric(value))
                     stop("Minimum Color Range 'value' must be numeric")

                   ## init.
                   .Object@colrange.min <- value
                   return(.Object)
                 }
                 )

setMethod(f="colrange.max", signature="MappingColors",
          definition=function(object)
          {
            return(object@colrange.max)
          }
          )

setReplaceMethod(f="colrange.max", signature="MappingColors",
                 definition=function(.Object,value)
                 {
                   ## check
                   if(!is.numeric(value))
                     stop("Maximum Color Range 'value' must be numeric")

                   ## init.
                   .Object@colrange.max <- value
                   return(.Object)
                 }
                 )

setMethod(f="colrange", signature="MappingColors",
          definition=function(object)
          {
            return(c(object@colrange.min,object@colrange.max))
          }
          )

setReplaceMethod(f="colrange", signature="MappingColors",
                 definition=function(.Object,value)
                 {
                   ## check
                   if(!is.vector(value) && (length(value) != 2))
                     stop("Colors Range 'value' must be a vector such as: c(min,max)")

                   ## init.
                   value <- unlist(value)
                   .Object@colrange.min <- value[[1]]
                   .Object@colrange.max <- value[[2]]
                   return(.Object)
                 }
                 )

setMethod(f="exec", signature="MappingColors",
          definition=function(.Object, svg)
          {
            ## local
            .v2col <- function(v,colors,s,l,m) {
              v <- (v-m)*s+1
              v <- if(v < 1) 1 else v
              v <- if(v > l) l else v
              return(colors[as.integer(round(v))])
            }
            
            ## call super
            callNextMethod()
            
            ## init.
            namedOjbect <- deparse(substitute(.Object))          

            ## transform fn(values) -> colors
            l <- length(.Object@map.colors)
            s <- (l-1) / (.Object@colrange.max-.Object@colrange.min)
            .Object@.values <- sapply(.Object@.values,
                                      .v2col,
                                      .Object@map.colors, s, l, .Object@colrange.min)

            ## map colors on the template attribute
            svg[.Object@targets,.Object@target.attribute] <- .Object@.values
            
            ## eop
            assign(namedOjbect, .Object, envir=parent.frame())
            return(invisible(.Object))
          }
          )
