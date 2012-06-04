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

## S V G L I N E
## --------------------------------------------------

setGenericVerif <- function(name,y){if(!isGeneric(name)){setGeneric(name,y)}else{}}

setClass("SVGLine",
         representation(x1="character",
                        y1="character",
                        x2="character",
                        y2="character"),
         contains="SVGShape"
         )

setGenericVerif(name="coords", function(object) { standardGeneric("coords") })
setGenericVerif(name="coords<-", function(.Object,value) { standardGeneric("coords<-") })
setGenericVerif(name="x1", function(object) { standardGeneric("x1") })
setGenericVerif(name="x1<-", function(.Object,value) { standardGeneric("x1<-") })
setGenericVerif(name="y1", function(object) { standardGeneric("y1") })
setGenericVerif(name="y1<-", function(.Object,value) { standardGeneric("y1<-") })
setGenericVerif(name="x2", function(object) { standardGeneric("x2") })
setGenericVerif(name="x2<-", function(.Object,value) { standardGeneric("x2<-") })
setGenericVerif(name="y2", function(object) { standardGeneric("y2") })
setGenericVerif(name="y2<-", function(.Object,value) { standardGeneric("y2<-") })
setGenericVerif(name=".xml", function(object) { standardGeneric(".xml") })

setMethod(f="initialize", signature="SVGLine",
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
                 (length(names(coords) %in% list("x1","y1","x2","y2"))==4) )
                flag <- TRUE
            }
            if(flag) {
              coords(.Object) <- coords
            }
            else  {
              x1(.Object) <- .arg("x1","0")
              y1(.Object) <- .arg("y1","0")
              x2(.Object) <- .arg("x2","0")
              y2(.Object) <- .arg("y2","0")
            }
            
            ## eop
            return(.Object)
          }
          )

setMethod(f="print", signature="SVGLine",
          definition=function(x,...)
          {
            rect.node <- .xml(x)
            print(rect.node)
          }
          )

setMethod(f="coords", signature="SVGLine",
          definition=function(object)
          {
            return(list(x1=x1(object),y1=y1(object),x2=x2(object),y2=y2(object)))
          }
          )

setReplaceMethod(f="coords", signature="SVGLine",
                 definition=function(.Object,value)
                 {
                   ## check
                   if(!is.list(value))
                     stop("'value' must be a 'list()'")
                   check <- names(value) %in% list("x1","y1","x2","y2")
                   if(length(check[check == TRUE]) != 4)
                     stop("'value' must at least contains 'x1,y1,x2,y2'")

                   ## assign & eop
                   x1(.Object) <- value[["x1"]]
                   y1(.Object) <- value[["y1"]]
                   x2(.Object) <- value[["x2"]]
                   y2(.Object) <- value[["y2"]]
                   return(.Object)
                 }
                 )

setMethod(f="x1", signature="SVGLine",
          definition=function(object)
          {
            return(object@x1)
          }
          )

setReplaceMethod(f="x1", signature="SVGLine",
                 definition=function(.Object,value)
                 {
                   ## check
                   if(!is.atomic(value))
                     stop("'value' must be atomic")
                   if(is.numeric(value)) value <- as.character(value)
                   if(!is.character(value))
                     stop("'value' must be an atomic string")

                   ## assign & eop
                   .Object@x1 <- value
                   return(.Object)
                 }
                 )

setMethod(f="y1", signature="SVGLine",
          definition=function(object)
          {
            return(object@y1)
          }
          )

setReplaceMethod(f="y1", signature="SVGLine",
                 definition=function(.Object,value)
                 {
                   ## check
                   if(!is.atomic(value))
                     stop("'value' must be atomic")
                   if(is.numeric(value)) value <- as.character(value)
                   if(!is.character(value))
                     stop("'value' must be an atomic string")

                   ## assign & eop
                   .Object@y1 <- value
                   return(.Object)
                 }
                 )

setMethod(f="x2", signature="SVGLine",
          definition=function(object)
          {
            return(object@x2)
          }
          )

setReplaceMethod(f="x2", signature="SVGLine",
                 definition=function(.Object,value)
                 {
                   ## check
                   if(!is.atomic(value))
                     stop("'value' must be atomic")
                   if(is.numeric(value)) value <- as.character(value)
                   if(!is.character(value))
                     stop("'value' must be an atomic string")

                   ## assign & eop
                   .Object@x2 <- value
                   return(.Object)
                 }
                 )

setMethod(f="y2", signature="SVGLine",
          definition=function(object)
          {
            return(object@y2)
          }
          )

setReplaceMethod(f="y2", signature="SVGLine",
                 definition=function(.Object,value)
                 {
                   ## check
                   if(!is.atomic(value))
                     stop("'value' must be atomic")
                   if(is.numeric(value)) value <- as.character(value)
                   if(!is.character(value))
                     stop("'value' must be an atomic string")

                   ## assign & eop
                   .Object@y2 <- value
                   return(.Object)
                 }
                 )

setMethod(f=".xml", signature="SVGLine",
          definition=function(object)
          {
            ## init.
            line <- newXMLNode("line")

            ## core attributes
            attr <- callNextMethod(object)

            ## attributes
            if(x1(object) != "0") attr <- c(attr, x1=x1(object))
            if(y1(object) != "0") attr <- c(attr, y1=y1(object))
            if(x2(object) != "0") attr <- c(attr, x2=x2(object))
            if(y2(object) != "0") attr <- c(attr, y2=y2(object))
            xmlAttrs(line) <- attr

            ## eop
            return(line)
          }
          )
          
## F A C T O R Y
## --------------------------------------------------
SVGLine.factory <- function(coords,x1,y1,x2,y2,class,style,transform) {

  ## init.
  args <- list("SVGLine")
  if(!missing(coords)) args <- c(args,coords=list(coords))
  if(!missing(x1)) args <- c(args,x1=x1)
  if(!missing(y1)) args <- c(args,y1=y1)
  if(!missing(x2)) args <- c(args,x2=x2)
  if(!missing(y2)) args <- c(args,y2=y2)
  if(!missing(class)) args <- c(args,class=class)
  if(!missing(style)) args <- c(args,style=style)
  if(!missing(transform)) args <- c(args,transform=transform)
  line <- do.call(new,args)

  ## eop
  return(line)
}
