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
         representation(svg_units="character",
                        svg_transform="character",
                        spread.method="character",
                        xlink.href="character",
                        stops="list",
                        "VIRTUAL"
                        ),
         contains="SVGNode"
         )

setGenericVerif(name="svgUnits", function(object) { standardGeneric("svgUnits") })
setGenericVerif(name="svgUnits<-", function(.Object, value) { standardGeneric("svgUnits<-") })
setGenericVerif(name="svgTransform", function(object) { standardGeneric("svgTransform") })
setGenericVerif(name="svgTransform<-", function(.Object, value) { standardGeneric("svgTransform<-") })
setGenericVerif(name="spreadMethod", function(object) { standardGeneric("spreadMethod") })
setGenericVerif(name="spreadMethod<-", function(.Object, value) { standardGeneric("spreadMethod<-") })
setGenericVerif(name="xlinkHref", function(object) { standardGeneric("xlinkHref") })
setGenericVerif(name="xlinkHref<-", function(.Object, value) { standardGeneric("xlinkHref<-") })
setGenericVerif(name="stops", function(object) { standardGeneric("stops") })
setGenericVerif(name="stops<-", function(.Object, value) { standardGeneric("stops<-") })
setGenericVerif(name=".xml", function(object) { standardGeneric(".xml") })
setGenericVerif(name="URL", function(object) { standardGeneric("URL") })

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

            ## super
            .Object <- callNextMethod(.Object,...)

            ## get args
            args = list(...)
            args.names = names(args)
            if(is.null(args.names)) args.names <- list()
            
            ## default init.
            .Object@svg_units <- .arg("units","objectBoundingBox")
            .Object@svg_transform <- .arg("transform",character(0))
            .Object@spread.method <- .arg("spread.method","pad")
            .Object@xlink.href <- .arg("xlink.href",character(0))
            .Object@stops <- .arg("stops",list())

            ## eop
            return(.Object)
          }
          )

setMethod(f="svgUnits", signature="Gradient",
          definition=function(object)
          {
            return(object@svg_units)
          }
          )

setReplaceMethod(f="svgUnits", signature="Gradient",
                 definition=function(.Object, value)
                 {
                   ## check
                   if(!is.character(value))
                     stop("'value' must be a character string")
                   if(!(value %in% list("userSpaceOnUse", "objectBoundingBox")))
                     stop("'value' must be either 'userSpaceOnUse' or 'objectBoundingBox'")

                   ## assign & eop
                   .Object@svg_units <- value
                   return(.Object)
                 }
                 )

setMethod(f="svgTransform", signature="Gradient",
          definition=function(object)
          {
            return(object@svg_transform)
          }
          )

setReplaceMethod(f="svgTransform", signature="Gradient",
                 definition=function(.Object, value)
                 {
                   ## check
                   if(!is.character(value))
                     stop("'value' must be a character string")

                   ## assign & eop
                   .Object@svg_transform <- value
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
                   ## case 1 - list()
                   if(is.list(value)) {
                     if(length(value) > 0) {
                       check <- sapply(value,function(x) {return(is.object(x) && is(x,"GradientStop"))})
                       value <- value[check]
                       .Object@stops <- value
                     }
                   }

                   ## case 2 - object
                   else if(is.object(value) && is(value, "GradientStop")) 
                     .Object@stops <- c(.Object@stops, value)

                   ## error
                   else 
                     stop("'value' must be either a list of GradientStop or a GradientStop")
                   
                   ## eop
                   return(.Object)
                 }
                 )

setMethod(f=".xml", signature="Gradient",
          definition=function(object)
          {
            ## super
            attr <- callNextMethod(object)
            
            ## return a list of core attributes
            if(object@svg_units != "objectBoundingBox")
              attr <- c(attr, gradientUnits=object@svg_units)
            if(length(object@svg_transform) > 0)
              attr <- c(attr, transform=object@svg_transform)
            if(object@spread.method != "pad")
              attr <- c(attr, spreadMethod=object@spread.method)
            if(length(object@xlink.href) > 0)
              attr <- c(attr, "xlink:href"=object@xlink.href)
            return(attr)
          }
          )

setMethod(f="URL", signature="Gradient",
          definition=function(object)
          {
            if(length(object@id) == 0)
              return(object@id)
            else
              return(paste("url(#",object@id,")",sep=""))
          }
          )
