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

## M A S K
## --------------------------------------------------

setGenericVerif <- function(name,y){if(!isGeneric(name)){setGeneric(name,y)}else{}}

setClass("Mask",
         representation(maskUnits="character",
                        maskContentUnits="character",
                        x="character",
                        y="character",
                        width="character",
                        height="character",
                        content="XMLInternalNode"),
         contains="SVGNode"
         )

setGenericVerif(name="maskUnits", function(object) { standardGeneric("maskUnits") })
setGenericVerif(name="maskUnits<-", function(.Object, value) { standardGeneric("maskUnits<-") })
setGenericVerif(name="maskContentUnits", function(object) { standardGeneric("maskContentUnits") })
setGenericVerif(name="maskContentUnits<-", function(.Object, value) { standardGeneric("maskContentUnits<-") })
setGenericVerif(name="x", function(object) { standardGeneric("x") })
setGenericVerif(name="x<-", function(.Object, value) { standardGeneric("x<-") })
setGenericVerif(name="y", function(object) { standardGeneric("y") })
setGenericVerif(name="y<-", function(.Object, value) { standardGeneric("y<-") })
setGenericVerif(name="width", function(object) { standardGeneric("width") })
setGenericVerif(name="width<-", function(.Object, value) { standardGeneric("width<-") })
setGenericVerif(name="height", function(object) { standardGeneric("height") })
setGenericVerif(name="height<-", function(.Object, value) { standardGeneric("height<-") })
setGenericVerif(name="content", function(object) { standardGeneric("content") })
setGenericVerif(name="content<-", function(.Object, value) { standardGeneric("content<-") })
setGenericVerif(name=".xml", function(object) { standardGeneric(".xml") })


setMethod(f="initialize", signature="Mask",
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
            args <- list(...)
            args.names <- names(args)
            if(is.null(args.names)) args.names <- list()

            ## default init.
            .Object@maskUnits <- .arg("maskUnits","objectBoundingBox")
            .Object@maskContentUnits <- .arg("maskContentUnits","userSpaceOnUse")
            .Object@x <- .arg("x","-10%")
            .Object@y <- .arg("y","-10%")
            .Object@width <- .arg("width","120%")
            .Object@height <- .arg("height","120%")
            .Object@content <- .arg("content",NULL)

            ## eop
            return(.Object)
          }
          )

setMethod(f="maskUnits", signature="Mask",
          definition=function(object)
          {
            return(object@maskUnits)
          }
          )

setReplaceMethod(f="maskUnits", signature="Mask",
                 definition=function(.Object, value)
                 {
                   ## check
                   if(!is.character(value))
                     stop("'value' must be a character string")
                   if(!(value %in% list("userSpaceOnUse", "objectBoundingBox")))
                     stop("'value' must be either 'userSpaceOnUse' or 'objectBoundingBox'")

                   ## assign & eop
                   .Object@maskUnits <- value
                   return(.Object)
                 }
                 )

setMethod(f="maskContentUnits", signature="Mask",
          definition=function(object)
          {
            return(object@maskContentUnits)
          }
          )

setReplaceMethod(f="maskContentUnits", signature="Mask",
                 definition=function(.Object, value)
                 {
                   ## check
                   if(!is.character(value))
                     stop("'value' must be a character string")
                   if(!(value %in% list("userSpaceOnUse", "objectBoundingBox")))
                     stop("'value' must be either 'userSpaceOnUse' or 'objectBoundingBox'")

                   ## assign & eop
                   .Object@maskContentUnits <- value
                   return(.Object)
                 }
                 )
 
setMethod(f="x", signature="Mask",
          definition=function(object)
          {
            return(object@x)
          }
          )

setReplaceMethod(f="x", signature="Mask",
                 definition=function(.Object, value)
                 {
                   ## check
                   if(is.numeric(value))
                     value <- as.character(value)
                   if(!is.character(value))
                     stop("'value' must be a character string or a numeric value")

                   ## assign & eop
                   .Object@x <- value
                   return(.Object)
                 }
                 )

setMethod(f="y", signature="Mask",
          definition=function(object)
          {
            return(object@y)
          }
          )

setReplaceMethod(f="y", signature="Mask",
                 definition=function(.Object, value)
                 {
                   ## check
                   if(is.numeric(value))
                     value <- as.character(value)
                   if(!is.character(value))
                     stop("'value' must be a character string or a numeric value")

                   ## assign & eop
                   .Object@y <- value
                   return(.Object)
                 }
                 )

setMethod(f="width", signature="Mask",
          definition=function(object)
          {
            return(object@width)
          }
          )

setReplaceMethod(f="width", signature="Mask",
                 definition=function(.Object, value)
                 {
                   ## check
                   if(is.numeric(value))
                     value <- as.character(value)
                   if(!is.character(value))
                     stop("'value' must be a character string or a numeric value")

                   ## assign & eop
                   .Object@width <- value
                   return(.Object)
                 }
                 )

setMethod(f="height", signature="Mask",
          definition=function(object)
          {
            return(object@height)
          }
          )

setReplaceMethod(f="height", signature="Mask",
                 definition=function(.Object, value)
                 {
                   ## check
                   if(is.numeric(value))
                     value <- as.character(value)
                   if(!is.character(value))
                     stop("'value' must be a character string or a numeric value")

                   ## assign & eop
                   .Object@height <- value
                   return(.Object)
                 }
                 )

setMethod(f="content", signature="Mask",
          definition=function(object)
          {
            return(object@content)
          }
          )

setReplaceMethod(f="content", signature="Mask",
                 definition=function(.Object,value)
                 {
                   ## check
                   if(!is.object(value) && !is(value,"XMLInternalNode")) 
                     stop("'value' must be a XMLInternalNode object")

                   ## assign & eop
                   .Object@content <- value
                   return(.Object)
                 }
                 )

setMethod(f=".xml", signature="Mask",
          definition=function(object)
          {
            ## super
            attr <- callNextMethod(object)

            ## return a core attribute list
            if(object@maskUnits != "objectBoundingBox")
              attr <- c(attr,maskUnits=object@maskUnits)
            if(object@maskContentUnits != "userSpaceOnUse")
              attr <- c(attr,maskContentUnits=object@maskContentUnits)
            if(object@x != "-10%")
              attr <- c(attr,x=object@x)
            if(object@y != "-10%")
              attr <- c(attr,y=object@y)
            if(object@width != "120%")
              attr <- c(attr,width=object@width)
            if(object@height != "120%")
              attr <- c(attr,height=object@height)

            ## eop
            return(attr)
          }
          )

## F A C T O R Y
##----------------------------------------
Mask.factory <- function() {
  
}
