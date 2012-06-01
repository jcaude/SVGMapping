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

setClass("SVGShape",
         representation(svg.class="vector",
                        svg.style="character",
                        svg.transform="vector",
                        "VIRTUAL"),
         contains="SVGNode"
         )

setGenericVerif(name="svgClass", function(object) { standardGeneric("svgClass") })
setGenericVerif(name="svgClass<-", function(.Object,value) { standardGeneric("svgClass<-") })
setGenericVerif(name="svgStyle", function(object) { standardGeneric("svgStyle") })
setGenericVerif(name="svgStyle<-", function(.Object,value) { standardGeneric("svgStyle<-") })
setGenericVerif(name="svgTransform", function(object) { standardGeneric("svgTransform") })
setGenericVerif(name="svgTransform<-", function(.Object,value) { standardGeneric("svgTransform<-") })
setGenericVerif(name=".xml", function(object) { standardGeneric(".xml") })

setMethod(f="initialize", signature="SVGShape",
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

            .varg <- function(name,default.value,split="[ ]+") {
              if(sum(grepl(paste("^",name,"$",sep=""), args.names)) > 0) {
                v <- args[[grep(paste("^",name,"$",sep=""),args.names)]]
                return(unlist(strsplit(v,split=split)))
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
            .Object@svg.class <- .varg("class", character(0))
            .Object@svg.style <- .arg("style", character(0))
            .Object@svg.transform <- .varg("transform", character(0))

            ## eop
            return(.Object)
          }
          )

setMethod(f="svgClass", signature="SVGShape",
          definition=function(object)
          {
            return(object@svg.class)
          }
          )

setReplaceMethod(f="svgClass", signature="SVGShape",
                 definition=function(.Object,value)
                 {
                   ## check
                   if(!(is.vector(value) && !is.list(value) && is.character(value)))
                     stop("'value' must be a vector of character strings")

                   ## assign & eop
                   .Object@svg.class <- value
                   return(.Object)
                 }
                 )

setMethod(f="svgStyle", signature="SVGShape",
          definition=function(object)
          {
            return(object@svg.style)
          }
          )

setReplaceMethod(f="svgStyle", signature="SVGShape",
                 definition=function(.Object,value)
                 {
                   ## check
                   if(!(is.atomic(value) && is.character(value)))
                     stop("'value' must be an atomic string")

                   ## assign & eop
                   .Object@svg.style <- value
                   return(.Object)                   
                 }
                 )

setMethod(f="svgTransform", signature="SVGShape",
          definition=function(object)
          {
            return(object@svg.transform)
          }
          )

setReplaceMethod(f="svgTransform", signature="SVGShape",
                 definition=function(.Object,value)
                 {
                   ## check
                   if(!(is.vector(value) && !is.list(value) && is.character(value)))
                     stop("'value' must be a vector of character strings")

                   ## assign & eop
                   .Object@svg.transform <- value
                   return(.Object)
                 }
                 )

setMethod(f=".xml", signature="SVGShape",
          definition=function(object)
          {
            ## super
            attr <- callNextMethod(object)

            ## complete the list of core attributes
            if(length(object@svg.class) > 0)
              attr <- c(attr,class=paste(object@svg.class,collapse=" "))
            if(length(object@svg.style) > 0)
              attr <- c(attr,style=object@svg.style)
            if(length(object@svg.transform) > 0)
              attr <- c(attr,transform=paste(object@svg.transform,collapse=" "))
            return(attr)            
          }
          )
