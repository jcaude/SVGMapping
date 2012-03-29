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

## G R A D I E N T S
## --------------------------------------------------

setGenericVerif <- function(name,y){if(!isGeneric(name)){setGeneric(name,y)}else{}}

setClass("Gradient",
         representation(id="character",
                        units="character",
                        transform="character",
                        spread.method="character",
                        xlink.href="character",
                        stops="list",
                        "VIRTUAL"
                        )
         )

setGenericVerif(name="id", function(object) { standardGeneric("id") })
setGenericVerif(name="id<-", function(.Object, value) { standardGeneric("id<-") })
setGenericVerif(name="units", function(object) { standardGeneric("units") })
setGenericVerif(name="units<-", function(.Object, value) { standardGeneric("units<-") })
setGenericVerif(name="transform", function(object) { standardGeneric("transform") })
setGenericVerif(name="transform<-", function(.Object, value) { standardGeneric("transform<-") })
setGenericVerif(name="spreadMethod", function(object) { standardGeneric("spreadMethod") })
setGenericVerif(name="spreadMethod<-", function(.Object, value) { standardGeneric("spreadMethod<-") })
setGenericVerif(name="xlinkHref", function(object) { standardGeneric("xlinkHref") })
setGenericVerif(name="xlinkHref<-", function(.Object, value) { standardGeneric("xlinkHref<-") })
setGenericVerif(name="stops", function(object) { standardGeneric("stops") })
setGenericVerif(name="stops<-", function(.Object, value) { standardGeneric("stops<-") })
setGenericVerif(name="SVG", function(object) { standardGeneric("SVG") })

setMethod(f="initialize", signature="Gradient",
          definition=function(.Object,...)
          {
            ## - locals
            .arg <- function(name,default.value) {
              if(sum(grepl(paste("^",name,"$",sep=""), args.names)) > 0) {
                v <- args[[grep(paste("^",name,"$",sep=""),args.names)]]
                return(v)
              }
              else {
                return(default.value)
              }
            }

            ## get args
            args = list(...)
            args.names = names(args)
            if(is.null(args.names)) args.names <- list()
            
            ## default init.
            .Object@id <- .arg("id",character(0))
            .Object@units <- .arg("units","objectBoundingBox")
            .Object@transform <- .arg("transform",character(0))
            .Object@spread.method <- .arg("spread.method","pad")
            .Object@xlink.href <- .arg("xlink.href",character(0))
            .Object@stops <- .arg("stops",list())

            ## eop
            return(.Object)
          }
          )

setMethod(f="show", signature="Gradient",
          definition=function(object)
          {
            s <- character(0)
            if(length(object@id) > 0) s <- paste(s," id=",object@id,sep="")
            if(object@units != "objectBoundingBox") s <- paste(s," units=",object@units,sep="")
            if(length(object@transform) > 0) s <- paste(s," transform=",object@transform,sep="")
            if(object@spread.method != "pad") s <- paste(s," spread.method=",object@spread.method,sep="")
            if(length(object@xlink.href) > 0) s <- paste(s," xlink.href=",object@xlink.href,sep="")
            cat(s)
          }
          )

setMethod(f="id", signature="Gradient",
          definition=function(object)
          {
            return(object@id)
          }
          )

setReplaceMethod(f="id", signature="Gradient",
                 definition=function(.Object,value)
                 {
                   ## check
                   if(!is.character(value))
                     stop("'value' must be a character string")
                     
                   ## eop
                   .Object@id <- as.character(value)
                   return(.Object)
                 }
                 )

setMethod(f="units", signature="Gradient",
          definition=function(object)
          {
            return(object@units)
          }
          )

setReplaceMethod(f="units", signature="Gradient",
                 definition=function(.Object, value)
                 {
                   ## check
                   if(!is.character(value))
                     stop("'value' must be a character string")
                   if(!(value %in% list("userSpaceOnUse", "objectBoundingBox")))
                     stop("'value' must be either 'userSpaceOnUse' or 'objectBoundingBox'")

                   ## assign & eop
                   .Object@units <- value
                   return(.Object)
                 }
                 )

setMethod(f="transform", signature="Gradient",
          definition=function(object)
          {
            return(object@transform)
          }
          )

setReplaceMethod(f="transform", signature="Gradient",
                 definition=function(.Object, value)
                 {
                   ## check
                   if(!is.character(value))
                     stop("'value' must be a character string")

                   ## assign & eop
                   .Object@transform <- value
                   return(.Object)
                 }
                 )

setMethod(f="spreadMethod", signature="Gradient",
          definition=function(object)
          {
            return(object@spread.method)
          }
          )

setReplaceMethod(f="spreadMethod", signature="Gradient",
                 definition=function(.Object, value)
                 {
                   ## check
                   if(!is.character(value))
                     stop("'value' must be a character string")
                   value <- tolower(value)
                   if(!(value %in% list("pad", "reflect", "repeat")))
                     stop("'value' must be either: pad, reflect or repeat")

                   ## assign & eop
                   .Object@spread.method <- value
                   return(.Object)
                 }
                 )

setMethod(f="xlinkHref", signature="Gradient",
          definition=function(object)
          {
            return(object@xlink.href)
          }
          )

setReplaceMethod(f="xlinkHref", signature="Gradient",
                 definition=function(.Object, value)
                 {
                   ## check
                   if(!is.character(value))
                     stop("'value' must be a character string")

                   ## assign & eop
                   .Object@xlink.href <- value
                   return(.Object)
                 }
                 )

setMethod(f="stops", signature="Gradient",
          definition=function(object)
          {
            return(object@stops)
          }
          )

setReplaceMethod(f="stops", signature="Gradient",
                 definition=function(.Object, value)
                 {
                   ## check
                   if(!is.list(stops))
                     stop("'value' must be a list of 'GradientStop' objects")

                   ## assign & eop
                   .Object@stops <- value
                   return(.Object)
                 }
                 )

setMethod(f="SVG", signature="Gradient",
          definition=function(object)
          {
            ## return a list of core attributes
            attr <- list(id=object@id)
            if(object@units != "objectBoundingBox")
              attr <- c(attr, gradientUnits=object@units)
            if(length(object@transform) > 0)
              attr <- c(attr, transform=object@transform)
            if(object@spread.method != "pad")
              attr <- c(attr, spreadMethod=object@spread.method)
            if(length(object@xlink.href) > 0)
              attr <- c(attr, "xlink:href"=object@xlink.href)
            return(attr)
          }
          )
