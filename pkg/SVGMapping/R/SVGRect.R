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

setClass("SVGRect",
         representation(x="character",
                        y="character",
                        width="character",
                        height="character",
                        rx="character",
                        ry="character"),
         contains="SVGShape"
         )

setGenericVerif(name="coords", function(object) { standardGeneric("coords") })
setGenericVerif(name="coords<-", function(.Object,value) { standardGeneric("coords<-") })
setGenericVerif(name="x", function(object) { standardGeneric("x") })
setGenericVerif(name="x<-", function(.Object,value) { standardGeneric("x<-") })
setGenericVerif(name="y", function(object) { standardGeneric("y") })
setGenericVerif(name="y<-", function(.Object,value) { standardGeneric("y<-") })
setGenericVerif(name="width", function(object) { standardGeneric("width") })
setGenericVerif(name="width<-", function(.Object,value) { standardGeneric("width<-") })
setGenericVerif(name="height", function(object) { standardGeneric("height") })
setGenericVerif(name="height<-", function(.Object,value) { standardGeneric("height<-") })
setGenericVerif(name="rx", function(object) { standardGeneric("rx") })
setGenericVerif(name="rx<-", function(.Object,value) { standardGeneric("rx<-") })
setGenericVerif(name="ry", function(object) { standardGeneric("ry") })
setGenericVerif(name="ry<-", function(.Object,value) { standardGeneric("ry<-") })
setGenericVerif(name=".xml", function(object) { standardGeneric(".xml") })

setMethod(f="initialize", signature="SVGRect",
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
            flag <- FALSE
            coords <- list()
            if(sum(grepl("^coords$", args.names)) > 0) {
              coords <- args[["coords"]]
              if( is.list(coords) &&
                 (length(names(coords) %in% list("x","y","width","height"))==4) )
                flag <- TRUE
            }
            .Object@x <- if(flag) coords[["x"]] else .arg("x","0")
            .Object@y <- if(flag) coords[["y"]] else .arg("y","0")
            .Object@width <- if(flag) coords[["width"]] else .arg("width","0")
            .Object@height <- if(flag) coords[["height"]] else .arg("height","0")
            .Object@rx <- .arg("rx",character(0))
            .Object@ry <- .arg("ry",character(0))

            ## eop
            return(.Object)
          }
          )

setMethod(f="print", signature="SVGRect",
          definition=function(x,...)
          {
            rect.node <- .xml(x)
            return(rect.node)
          }
          )

setMethod(f="coords", signature="SVGRect",
          definition=function(object)
          {
            return(list(x=object@x,y=object@y,width=object@width,height=object@height))
          }
          )

setReplaceMethod(f="coords", signature="SVGRect",
                 definition=function(.Object,value)
                 {
                   ## check
                   if(!is.list(value))
                     stop("'value' must be a 'list()'")
                   check <- names(value) %in% list("x","y","width","height")
                   if(length(check[check == TRUE]) != 4)
                     stop("'value' must at least contains 'x,y,width,height'")

                   ## assign & eop
                   x(.Object) <- value[["x"]]
                   y(.Object) <- value[["y"]]
                   width(.Object) <- value[["width"]]
                   height(.Object) <- value[["height"]]
                   return(.Object)
                 }
                 )

setMethod(f="x", signature="SVGRect",
          definition=function(object)
          {
            return(object@x)
          }
          )

setReplaceMethod(f="x", signature="SVGRect",
                 definition=function(.Object,value)
                 {
                   ## check
                   if(!is.atomic(value) && !is.character(value))
                     stop("'value' must be an atomic string")

                   ## assign & eop
                   .Object@x <- value
                   return(.Object)
                 }
                 )

setMethod(f="y", signature="SVGRect",
          definition=function(object)
          {
            return(object@y)
          }
          )

setReplaceMethod(f="y", signature="SVGRect",
                 definition=function(.Object,value)
                 {
                   ## check
                   if(!is.atomic(value) && !is.character(value))
                     stop("'value' must be an atomic string")

                   ## assign & eop
                   .Object@y <- value
                   return(.Object)
                 }
                 )

setMethod(f="width", signature="SVGRect",
          definition=function(object)
          {
            return(object@width)
          }
          )

setReplaceMethod(f="width", signature="SVGRect",
                 definition=function(.Object,value)
                 {
                   ## check
                   if(!is.atomic(value) && !is.character(value))
                     stop("'value' must be an atomic string")

                   ## assign & eop
                   .Object@width <- value
                   return(.Object)
                 }
                 )

setMethod(f="height", signature="SVGRect",
          definition=function(object)
          {
            return(object@height)
          }
          )

setReplaceMethod(f="height", signature="SVGRect",
                 definition=function(.Object,value)
                 {
                   ## check
                   if(!is.atomic(value) && !is.character(value))
                     stop("'value' must be an atomic string")

                   ## assign & eop
                   .Object@height <- value
                   return(.Object)
                 }
                 )

setMethod(f="rx", signature="SVGRect",
          definition=function(object)
          {
            return(object@rx)
          }
          )

setReplaceMethod(f="rx", signature="SVGRect",
                 definition=function(.Object,value)
                 {
                   ## check
                   if(!is.atomic(value) && !is.character(value))
                     stop("'value' must be an atomic string")

                   ## assign & eop
                   .Object@rx <- value
                   return(.Object)
                 }
                 )

setMethod(f="ry", signature="SVGRect",
          definition=function(object)
          {
            return(object@ry)
          }
          )

setReplaceMethod(f="ry", signature="SVGRect",
                 definition=function(.Object,value)
                 {
                   ## check
                   if(!is.atomic(value) && !is.character(value))
                     stop("'value' must be an atomic string")

                   ## assign & eop
                   .Object@ry <- value
                   return(.Object)
                 }
                 )

setMethod(f=".xml", signature="SVGRect",
          definition=function(object)
          {
            ## init.
            rect <- newXMLNode("rect")

            ## core attributes
            attr <- callNextMethod(object)

            ## attributes
            if(x(object) != "0") attr <- c(attr, x=x(object))
            if(y(object) != "0") attr <- c(attr, y=y(object))
            attr <- c(attr,width=width(object))
            attr <- c(attr,height=height(object))
            if(length(rx(object)) > 0) attr <- c(attr, rx=rx(object))
            if(length(ry(object)) > 0) attr <- c(attr, ry=ry(object))
            xmlAttrs(rect) <- attr

            ## eop
            return(rect)
          }
          )
          
## F A C T O R Y
## --------------------------------------------------
SVGRect.factory <- function(coords,x,y,width,height,rx,ry,class,style,transform) {

  ## init.
  args <- list("SVGRect")
  if(!missing(coords)) args <- c(args,coords=list(coords))
  if(!missing(x)) args <- c(args,x=x)
  if(!missing(y)) args <- c(args,y=y)
  if(!missing(width)) args <- c(args,width=width)
  if(!missing(height)) args <- c(args,height=height)
  if(!missing(rx)) args <- c(args,rx=rx)
  if(!missing(ry)) args <- c(args,ry=ry)
  if(!missing(class)) args <- c(args,class=class)
  if(!missing(style)) args <- c(args,style=style)
  if(!missing(transform)) args <- c(args,transform=transform)
  rectangle <- do.call(new,args)

  ## eop
  return(rectangle)
}
 
