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

## R E C T A N G L E
## --------------------------------------------------

setGenericVerif <- function(name,y){if(!isGeneric(name)){setGeneric(name,y)}else{}}

#' Rectangular Definition (VIRTUAL)
#' 
#' This class is used to defined all object that have rectangular properties.
#' This class can't be directly instantiate and must be derived. 
#' 
#' @exportClass "Rectangle"
setClass("Rectangle",
         representation(x="character",
                        y="character",
                        width="character",
                        height="character",
                        "VIRTUAL"),
         prototype(x="0",y="0",width="0",height="0")
)

#' Dimensions of the Rectangular object
#' 
#' These methods are accessors to the location and dimension of a Rectangular
#' object.
#' 
#' The method \code{bbox(object)} return the bounding box (x,y,width,height) of 
#' the rectangular object.
#' 
#' Coordinates or dimensions are given as values compliant with the SVG 1.1
#' specifications
#' 
#' @name bbox
#' 
#' @param object a rectangular object
#' 
#' @return \code{bbox()}: a named list (\code{list(x=,y=,width=,height=)}) of 
#' values for the complete bounding-box given as strings.
#' 
#' @rdname rectangle.bbox-methods
#' @exportMethod bbox
#' @docType methods
setGeneric(name="bbox", function(object) { standardGeneric("bbox") })

#' <title already defined>
#' 
#' 
#' 
#' The \code{bbox(object)<-value} method can be used to set the bounding-box of
#' a rectangular object. In this case the \code{value} argument is expected 
#' to be a named list (\code{list(x=,y=,width=,heigh=)})
#' 
#' @name bbox<-
#' 
#' @return \code{bbox()<-}: the rectangular object as invisible
#' 
#' @rdname rectangle.bbox-methods
#' @exportMethod bbox<-
#' @docType methods
setGeneric(name="bbox<-", function(.Object,value) { standardGeneric("bbox<-") })

#' <title already defined>
#' 
#' The method \code{x(object)} return the location on the \emph{X-axis} of the 
#' rectangular object.
#' 
#' @name x
#' 
#' @return \code{x(),y(),width(),height()}: a string value for single 
#' coordinates and dimensions methods
#' 
#' @rdname rectangle.bbox-methods
#' @exportMethod x
#' @docType methods
setGeneric(name="x", function(object) { standardGeneric("x") })

#' <title already defined>
#' 
#' 
#' 
#' The \code{x(object)<-value} method can be used to set the \emph{X-axis}
#' location of a rectangular object.
#' 
#' @name x<-
#' 
#' @return \code{x(),y(),width(),height()<-}: the rectangular object as invisible
#' 
#' @rdname rectangle.bbox-methods
#' @exportMethod x<-
#' @docType methods
setGeneric(name="x<-", function(.Object,value) { standardGeneric("x<-") })

#' <title already defined>
#' 
#' The method \code{y(object)} return the location on the \emph{Y-axis} of 
#' the rectangular object.
#' 
#' @name y
#' @rdname rectangle.bbox-methods
#' @exportMethod y
#' @docType methods
setGeneric(name="y", function(object) { standardGeneric("y") })

#' <title already defined>
#' 
#' 
#' 
#' The \code{y(object)<-value} method can be used to set the \emph{Y-axis} 
#' location of a rectangular object.
#' 
#' @name y<-
#' @rdname rectangle.bbox-methods
#' @exportMethod y<-
#' @docType methods
setGeneric(name="y<-", function(.Object,value) { standardGeneric("y<-") })

#' <title already defined>
#' 
#' The method \code{width(object)} return the width of the rectangular object.
#' 
#' @name width
#' @rdname rectangle.bbox-methods
#' @exportMethod width
#' @docType methods
setGeneric(name="width", function(object) { standardGeneric("width") })

#' <title already defined>
#' 
#' 
#' 
#' The \code{width(object)<-value} method can be used to set the width of
#' an rectangular object.
#' 
#' @name width<-
#' @rdname rectangle.bbox-methods
#' @exportMethod width<-
#' @docType methods
setGeneric(name="width<-", function(.Object,value) { standardGeneric("width<-") })

#' <title already defined>
#' 
#' The method \code{height(object)} return the width of the rectangular object.
#' 
#' @name height
#' @rdname rectangle.bbox-methods
#' @exportMethod height
#' @docType methods
setGeneric(name="height", function(object) { standardGeneric("height") })

#' <title already defined>
#' 
#' 
#' 
#' The \code{height(object)<-value} method can be used to set the height of
#' a rectangular object.
#' 
#' @name height<-
#' @rdname rectangle.bbox-methods
#' @exportMethod height<-
#' @docType methods
setGeneric(name="height<-", function(.Object,value) { standardGeneric("height<-") })

setMethod(f="initialize", signature="Rectangle",
          definition=function(.Object,...)
          {
            ## - locals
            .arg <- function(name,default.value) {
              if(any(grepl(paste("^",name,"$",sep=""), args.names))) {
                v <- args[[grep(paste("^",name,"$",sep=""),args.names)]]
                return(v)
              }
              else {
                return(default.value)
              }
            }
            
            ## get args
            args <- list(...)
            args.names <- names(args)
            if(is.null(args.names)) args.names <- list()
            
            ## default init.
            flag <- FALSE
            bbox <- list()
            if(sum(grepl("^bbox$", args.names)) > 0) {
              bbox <- args[["bbox"]]
              if( is.list(bbox) &&
                  all(list("x","y","width","height") %in% names(bbox)) )
                flag <- TRUE
              else    
                stop("invalid 'bbox' argument")
            }
            if(flag) {
              bbox(.Object) <- bbox
            }
            else {
              x(.Object) <- .arg("x","0")
              y(.Object) <- .arg("y","0")
              width(.Object) <- .arg("width","0")
              height(.Object) <- .arg("height","0")
            }
            
            ## eop
            return(.Object)
          }
)

#' @rdname rectangle.bbox-methods
setMethod(f="bbox", signature="Rectangle",
          definition=function(object)
          {
            return(list(x=x(object),y=y(object),
                        width=width(object),height=height(object)))
          }
)

#' @name bbox<- 
#' @rdname rectangle.bbox-methods
setReplaceMethod(f="bbox", signature="Rectangle",
                 definition=function(.Object,value)
                 {
                   ## check
                   if(!is.list(value))
                     stop("'value' must be a 'list()'")
                   if(!all(c("x","y","width","height") %in% names(value)))
                     stop("'value' must contains fields 'x,y,width,height'")
                   
                   ## assign & eop
                   x(.Object) <- value[["x"]]
                   y(.Object) <- value[["y"]]
                   width(.Object) <- value[["width"]]
                   height(.Object) <- value[["height"]]
                   return(.Object)
                 }
)

#' @rdname rectangle.bbox-methods
setMethod(f="x", signature="Rectangle",
          definition=function(object)
          {
            return(object@x)
          }
)

#' @name x<-
#' @rdname rectangle.bbox-methods
setReplaceMethod(f="x", signature="Rectangle",
                 definition=function(.Object,value)
                 {
                   ## check
                   if(!is.atomic(value))
                     stop("'value' must be atomic")
                   if(is.numeric(value)) value <- as.character(value)
                   if(!is.character(value))
                     stop("'value must be a string")
                   
                   ## assign & eop
                   .Object@x <- value
                   return(.Object)
                 }
)

#' @rdname rectangle.bbox-methods
setMethod(f="y", signature="Rectangle",
          definition=function(object)
          {
            return(object@y)
          }
)

#' @name y<- 
#' @rdname rectangle.bbox-methods
setReplaceMethod(f="y", signature="Rectangle",
                 definition=function(.Object,value)
                 {
                   ## check
                   if(!is.atomic(value))
                     stop("'value' must be atomic")
                   if(is.numeric(value)) value <- as.character(value)
                   if(!is.character(value))
                     stop("'value must be a string")
                   
                   ## assign & eop
                   .Object@y <- value
                   return(.Object)
                 }
)

#' @rdname rectangle.bbox-methods
setMethod(f="width", signature="Rectangle",
          definition=function(object)
          {
            return(object@width)
          }
)

#' @name width<-
#' @rdname rectangle.bbox-methods 
setReplaceMethod(f="width", signature="Rectangle",
                 definition=function(.Object,value)
                 {
                   ## check
                   ## check
                   if(!is.atomic(value))
                     stop("'value' must be atomic")
                   if(is.numeric(value)) value <- as.character(value)
                   if(!is.character(value))
                     stop("'value must be a string")
                   
                   ## assign & eop
                   .Object@width <- value
                   return(.Object)
                 }
)

#' @rdname rectangle.bbox-methods
setMethod(f="height", signature="Rectangle",
          definition=function(object)
          {
            return(object@height)
          }
)

#' @name height<- 
#' @rdname rectangle.bbox-methods
setReplaceMethod(f="height", signature="Rectangle",
                 definition=function(.Object,value)
                 {
                   ## check
                   if(!is.atomic(value))
                     stop("'value' must be atomic")
                   if(is.numeric(value)) value <- as.character(value)
                   if(!is.character(value))
                     stop("'value must be a string")
                   
                   ## assign & eop
                   .Object@height <- value
                   return(.Object)
                 }
)

#' @aliases .xml,Rectangle-method
#' @rdname svgnode.xml-methods
setMethod(f=".xml", signature="Rectangle",
          definition=function(object)
          {
            ## super
            attr <- list()
            
            ## return a list of core attributes
            if(x(object) != "0") attr <- c(attr, x=x(object))
            if(y(object) != "0") attr <- c(attr, y=y(object))
            if(width(object) != "0") attr <- c(attr,width=width(object))
            if(height(object) != "0") attr <- c(attr,height=height(object))
            
            ## eop
            return(attr)
          }
)

