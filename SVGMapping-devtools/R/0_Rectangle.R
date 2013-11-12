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
#' @aliases Rectangle-class
setClass("Rectangle",
         representation(x="SVGCoord",
                        y="SVGCoord",
                        width="SVGLength",
                        height="SVGLength",
                        "VIRTUAL")
)

#' <title already defined> 
#' 
#' 
#' 
#' \bold{Rectangle:} the method \code{bbox(object)} return the bounding box 
#' \code{list(x,y,width,height)} of the rectangular object.
#' 
#' @name bbox
#' 
#' @rdname svgmapping.bbox-methods
#' @exportMethod bbox
#' @docType methods
NULL

#' Coordinates of the Vector object
#' 
#' These methods are accessors to the location and direction of a \code{Vector}
#' object
#' 
#' The \code{coords{object} <- value} method can be used to set the location and
#' dimensions of a rectangular object. In this case the \code{value} argument is
#' expected to be a named list (\code{list(x=,y=,width=,height=)})
#' 
#' @name coords<-
#' 
#' @rdname rectangle.coords-methods
#' @exportMethod coords<-
#' @docType methods
NULL

#' <title already defined>
#' 
#' 
#' 
#' The method \code{x(object)} return the location on the \emph{X-axis} of the 
#' rectangular object.
#' 
#' @name x
#'   
#' @return \code{x(),y()}: an SVGCoord object for single coordinates and an
#'   SVGLength object for \code{width(),height()} dimensions methods
#'   
#' @rdname rectangle.coords-methods
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
#' @rdname rectangle.coords-methods
#' @exportMethod x<-
#' @docType methods
setGeneric(name="x<-", function(.Object,value) { standardGeneric("x<-") })

#' <title already defined>
#' 
#' 
#' 
#' The method \code{y(object)} return the location on the \emph{Y-axis} of 
#' the rectangular object.
#' 
#' @name y
#' @rdname rectangle.coords-methods
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
#' @rdname rectangle.coords-methods
#' @exportMethod y<-
#' @docType methods
setGeneric(name="y<-", function(.Object,value) { standardGeneric("y<-") })

#' <title already defined>
#' 
#' 
#' 
#' The method \code{width(object)} return the width of the rectangular object.
#' 
#' @name width
#' @rdname rectangle.coords-methods
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
#' @rdname rectangle.coords-methods
#' @exportMethod width<-
#' @docType methods
setGeneric(name="width<-", function(.Object,value) { standardGeneric("width<-") })

#' <title already defined>
#' 
#' 
#' 
#' The method \code{height(object)} return the width of the rectangular object.
#' 
#' @name height
#' @rdname rectangle.coords-methods
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
#' @rdname rectangle.coords-methods
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
            if(sum(grepl("^coords$", args.names)) > 0) {
              bbox <- args[["coords"]]
              if( is.list(bbox) &&
                  all(list("x","y","width","height") %in% names(bbox)) )
                flag <- TRUE
              else    
                stop("invalid 'coords' argument")
            }
            if(flag) {
              coords(.Object) <- bbox
            }
            else {
              x(.Object) <- .arg("x",SVGCoord.factory())
              y(.Object) <- .arg("y",SVGCoord.factory())
              width(.Object) <- .arg("width",SVGLength.factory())
              height(.Object) <- .arg("height",SVGLength.factory())
            }
            
            ## eop
            return(.Object)
          }
)

#' @rdname svgmapping.coords-methods
#' @aliases bbox,Rectangle-method
setMethod(f="bbox", signature="Rectangle",
          definition=function(object)
          {
            return(list(x=x(object),y=y(object),
                        width=width(object),height=height(object)))
          }
)

#' @name coords<- 
#' @rdname rectangle.coords-methods
#' @aliases coords<-,Rectangle-method
setReplaceMethod(f="coords", signature="Rectangle",
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

#' @rdname rectangle.coords-methods
#' @aliases x,Rectangle-method
setMethod(f="x", signature="Rectangle",
          definition=function(object)
          {
            return(object@x)
          }
)

#' @name x<-
#' @rdname rectangle.coords-methods
#' @aliases x<-,Rectangle-method
setReplaceMethod(f="x", signature="Rectangle",
                 definition=function(.Object,value)
                 {
                   ## check
                   if(is.atomic(value) && 
                        (is.numeric(value) || is.character(value))) 
                     value <- SVGCoord.factory(value)
                   if(!(is.object(value) && is(value,"SVGCoord")))
                     stop("'value' must be an SVGCoord object")
                   
                   ## assign & eop
                   .Object@x <- value
                   return(.Object)
                 }
)

#' @rdname rectangle.coords-methods
#' @aliases y,Rectangle-method
setMethod(f="y", signature="Rectangle",
          definition=function(object)
          {
            return(object@y)
          }
)

#' @name y<- 
#' @rdname rectangle.coords-methods
#' @aliases y<-,Rectangle-method
setReplaceMethod(f="y", signature="Rectangle",
                 definition=function(.Object,value)
                 {
                   ## check
                   if(is.atomic(value) && 
                        (is.numeric(value) || is.character(value))) 
                     value <- SVGCoord.factory(value)
                   if(!(is.object(value) && is(value,"SVGCoord")))
                     stop("'value' must be an SVGCoord object")
                   
                   ## assign & eop
                   .Object@y <- value
                   return(.Object)
                 }
)

#' @rdname rectangle.coords-methods
#' @aliases width,Rectangle-method
setMethod(f="width", signature="Rectangle",
          definition=function(object)
          {
            return(object@width)
          }
)

#' @name width<-
#' @rdname rectangle.coords-methods
#' @aliases width<-,Rectangle-method
setReplaceMethod(f="width", signature="Rectangle",
                 definition=function(.Object,value)
                 {
                   ## check
                   if(is.atomic(value) && 
                        (is.numeric(value) || is.character(value))) 
                     value <- SVGLength.factory(value)
                   if(!(is.object(value) && is(value,"SVGLength")))
                     stop("'value' must be an SVGLength object")
                   
                   ## assign & eop
                   .Object@width <- value
                   return(.Object)
                 }
)

#' @rdname rectangle.coords-methods
#' @aliases height,Rectangle-method
setMethod(f="height", signature="Rectangle",
          definition=function(object)
          {
            return(object@height)
          }
)

#' @name height<- 
#' @rdname rectangle.coords-methods
#' @aliases height<-,Rectangle-method
setReplaceMethod(f="height", signature="Rectangle",
                 definition=function(.Object,value)
                 {
                   ## check
                   if(is.atomic(value) && 
                        (is.numeric(value) || is.character(value))) 
                     value <- SVGLength.factory(value)
                   if(!(is.object(value) && is(value,"SVGLength")))
                     stop("'value' must be an SVGLength object")
                   
                   ## assign & eop
                   .Object@height <- value
                   return(.Object)
                 }
)

#' @rdname svgcore.xml-methods
#' @aliases .xml,Rectangle-method
setMethod(f=".xml", signature="Rectangle",
          definition=function(object)
          {
            ## init.
            uzero <- SVGCoord.factory()
            lzero <- SVGLength.factory()
            
            ## super
            attr <- list()
            
            ## return a list of core attributes
            if(x(object) != uzero) attr <- c(attr, x=as.character(x(object)))
            if(y(object) != uzero) attr <- c(attr, y=as.character(y(object)))
            if(width(object) != lzero) 
              attr <- c(attr,width=as.character(width(object)))
            if(height(object) != lzero) 
              attr <- c(attr,height=as.character(height(object)))
            
            ## eop
            return(attr)
          }
)

