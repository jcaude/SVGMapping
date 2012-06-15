## Copyright (c) 2012, CEA DSV/iBiTecS
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

## L A Y O U T
## --------------------------------------------------

setGenericVerif <- function(name,y){if(!isGeneric(name)){setGeneric(name,y)}else{}}

setClass("Layout",
         representation(opacity="numeric",
                        x="numeric",
                        y="numeric",
                        width="numeric",
                        height="numeric",
                        transform="character",
                        "VIRTUAL"),
         contains="SVGNode"
         )

setGenericVerif(name="opacity", function(object) { standardGeneric("opacity") })
setGenericVerif(name="opacity<-", function(.Object,value) { standardGeneric("opacity<-") })
setGenericVerif(name="dimensions", function(object) { standardGeneric("dimensions") })
setGenericVerif(name="dimensions<-", function(.Object,value) { standardGeneric("dimensions<-") })
setGenericVerif(name="x", function(object) { standardGeneric("x") })
setGenericVerif(name="x<-", function(.Object,value) { standardGeneric("x<-") })
setGenericVerif(name="y", function(object) { standardGeneric("y") })
setGenericVerif(name="y<-", function(.Object,value) { standardGeneric("y<-") })
setGenericVerif(name="width", function(object) { standardGeneric("width") })
setGenericVerif(name="width<-", function(.Object,value) { standardGeneric("width<-") })
setGenericVerif(name="height", function(object) { standardGeneric("height") })
setGenericVerif(name="height<-", function(.Object,value) { standardGeneric("height<-") })
setGenericVerif(name="svg.transform", function(object) { standardGeneric("svg.transform") })
setGenericVerif(name="svg.transform<-", function(.Object,value) { standardGeneric("svg.transform<-") })

setMethod(f="initialize", signature="Layout",
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
            opacity(.Object) <- .arg("opacity",0.0)
            dimensions(.Object) <- .arg("dimensions",list(x=0,y=0,width=100,height=100))
            svg.transform(.Object) <- .arg("transform",character(0))

            ## eop
            return(.Object)         
          }
          )

setMethod(f="opacity", signature="Layout",
          definition=function(object)
          {
            return(object@opacity)
          }
          )

setReplaceMethod(f="opacity", signature="Layout",
                 definition=function(.Object,value)
                 {
                   ## check
                   if(!is.atomic(value) && !is.numeric(value))
                     stop("'value' must be atomic and numeric")

                   ## assign & eop
                   .Object@opacity <- value
                   return(.Object)
                 }
                 )

setMethod(f="dimensions", signature="Layout",
          definition=function(object)
          {
            return(list(x=x(object),y=y(object),
                        width=width(object),height=height(object)))
          }
          )

setReplaceMethod(f="dimensions", signature="Layout",
                 definition=function(.Object,value)
                 {
                   ## check
                   if(!is.list(value))
                     stop("'value' must be a 'list()'")
                   check <- names(value) %in% list("x","y","width","height")
                   if(length(check[check == TRUE]) != 4)
                     stop("'value' must at least contains 'x,y,width,height'")

                   ## assign & eop
                   x(.Object) <- as.numeric(value[["x"]])
                   y(.Object) <- as.numeric(value[["y"]])
                   width(.Object) <- as.numeric(value[["width"]])
                   height(.Object) <- as.numeric(value[["height"]])
                   return(.Object)
                 }
                 )

setMethod(f="x", signature="Layout",
          definition=function(object)
          {
            return(object@x)
          }
          )

setReplaceMethod(f="x", signature="Layout",
                 definition=function(.Object,value)
                 {
                   ## check
                   if(!is.atomic(value) && !is.numeric(value))
                     stop("'value' must be atomic and numeric")

                   ## assign & eop
                   .Object@x <- value
                   return(.Object)
                 }
                 )

setMethod(f="y", signature="Layout",
          definition=function(object)
          {
            return(object@y)
          }
          )

setReplaceMethod(f="y", signature="Layout",
                 definition=function(.Object,value)
                 {
                   ## check
                   if(!is.atomic(value) && !is.numeric(value))
                     stop("'value' must be atomic and numeric")

                   ## assign & eop
                   .Object@y <- value
                   return(.Object)
                 }
                 )

setMethod(f="width", signature="Layout",
          definition=function(object)
          {
            return(object@width)
          }
          )

setReplaceMethod(f="width", signature="Layout",
                 definition=function(.Object,value)
                 {
                   ## check
                   if(!is.atomic(value) && !is.numeric(value))
                     stop("'value' must be atomic and numeric")

                   ## assign & eop
                   .Object@width <- value
                   return(.Object)
                 }
                 )

setMethod(f="height", signature="Layout",
          definition=function(object)
          {
            return(object@height)
          }
          )

setReplaceMethod(f="height", signature="Layout",
                 definition=function(.Object,value)
                 {
                   ## check
                   if(!is.atomic(value) && !is.numeric(value))
                     stop("'value' must be atomic and numeric")

                   ## assign & eop
                   .Object@height <- value
                   return(.Object)
                 }
                 )

setMethod(f="svg.transform", signature="Layout",
          definition=function(object)
          {
            return(object@transform)
          }
          )

setReplaceMethod(f="svg.transform", signature="Layout",
                 definition=function(.Object,value)
                 {
                   ## check
                   if(!is.atomic(value) && !is.character(value))
                     stop("'value' must be atomic and a string")

                   ## assign & eop
                   .Object@transform <- value
                   return(.Object)
                 }
                 )

