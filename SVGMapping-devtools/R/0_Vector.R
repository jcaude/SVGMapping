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

## V E C T O R
## --------------------------------------------------

setGenericVerif <- function(name,y){if(!isGeneric(name)){setGeneric(name,y)}else{}}

#' Vector Definition (VIRTUAL)
#' 
#' This class is used to defined all object that have vectorial properties.
#' This class can't be directly instantiate and must be derived. 
#'  
#'  @exportClass "Vector"
#'  @aliases Vector-class
setClass("Vector",
         representation(x1="SVGCoord",
                        y1="SVGCoord",
                        x2="SVGCoord",
                        y2="SVGCoord",
                        "VIRTUAL")
)

#' Named list of coordinates and dimensions 
#' 
#' \code{bbox} methods are accessors to the coordinates and dimensions of 
#' a geometrical shape.
#' 
#' \bold{Vector:} the method \code{bbox(object)} returns 
#' the bounding box \code{list(x1,y1,x2,y2)} of the vector object.
#' 
#' @note coordinates or length are given as values compliant with the SVG 1.1
#' specifications
#' 
#' @name bbox
#' 
#' @param object a geometrical object
#' 
#' @return \code{bbox()}: a named list of coordinates and dimensions 
#' 
#' @rdname svgmapping.bbox-methods
#' @exportMethod bbox
#' @docType methods
setGeneric(name="bbox", function(object) { standardGeneric("bbox") })

#' <title already defined>
#' 
#' 
#' 
#' \bold{Vector:} The \code{bbox{object} <- value} method sets the coordinates 
#' and direction of a vector object. It is expected that \code{value} is a 
#' list containing the four named values \code{list(x1,y1,x2,y2)}.
#' 
#' @name bbox<-
#' 
#'  @rdname svgmapping.bbox-methods
#'  @exportMethod bbox<-
#'  @docType methods  
setGeneric(name="bbox<-", function(.Object,value) { standardGeneric("bbox<-") })

#' Coordinates of the Vector object
#' 
#' These methods are accessors to the location and direction of a \code{Vector}
#' object
#' 
#' The \code{x1(object)} method returns the X-axis coordinate of the vector.
#' 
#' @name x1
#' 
#' @rdname vector.bbox-methods
#' @exportMethod x1
#' @docType methods
setGeneric(name="x1", function(object) { standardGeneric("x1") })

#' <title already defined>
#' 
#' 
#' 
#' The \code{x1(object) <- value} method sets the X-axis coordinate of
#' the vector.
#' 
#' @name x1<-
#' 
#'  @rdname vector.bbox-methods
#'  @exportMethod x1<-
#'  @docType methods  
setGeneric(name="x1<-", function(.Object,value) { standardGeneric("x1<-") })

#' <title already defined>
#' 
#' 
#' 
#' The \code{y1(object)} method returns the Y-axis coordinate of the vector.
#' 
#' @name y1
#' 
#' @rdname vector.bbox-methods
#' @exportMethod y1
#' @docType methods
setGeneric(name="y1", function(object) { standardGeneric("y1") })

#' <title already defined>
#' 
#' 
#' 
#' The \code{y1(object) <- value} method sets the Y-axis coordinate of
#' the vector.
#' 
#' @name y1<-
#' 
#'  @rdname vector.bbox-methods
#'  @exportMethod y1<-
#'  @docType methods  
setGeneric(name="y1<-", function(.Object,value) { standardGeneric("y1<-") })

#' <title already defined>
#' 
#' 
#' 
#' The \code{x2(object)} method returns the X-axis direction of the vector.
#' 
#' @name x2
#' 
#' @rdname vector.bbox-methods
#' @exportMethod x2
#' @docType methods
setGeneric(name="x2", function(object) { standardGeneric("x2") })

#' <title already defined>
#' 
#' 
#' 
#' The \code{x2(object) <- value} method sets the X-axis direction of
#' the vector.
#' 
#' @name x2<-
#' 
#'  @rdname vector.bbox-methods
#'  @exportMethod x2<-
#'  @docType methods  
setGeneric(name="x2<-", function(.Object,value) { standardGeneric("x2<-") })

#' <title already defined>
#' 
#' 
#' 
#' The \code{y2(object)} method returns the Y-axis direction of the vector.
#' 
#' @name y2
#' 
#' @rdname vector.bbox-methods
#' @exportMethod y2
#' @docType methods
setGeneric(name="y2", function(object) { standardGeneric("y2") })

#' <title already defined>
#' 
#' 
#' 
#' The \code{y2(object) <- value} method sets the Y-axis direction of
#' the vector.
#' 
#' @name y2<-
#' 
#'  @rdname vector.bbox-methods
#'  @exportMethod y2<-
#'  @docType methods  
setGeneric(name="y2<-", function(.Object,value) { standardGeneric("y2<-") })

setMethod(f="initialize", signature="Vector",
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
            flag <- FALSE
            bbox <- list()
            if(sum(grepl("^bbox$", args.names)) > 0) {
              bbox <- args[["bbox"]]
              if( is.list(bbox) &&
                  all(list("x1","y1","x2","y2") %in% names(bbox)) )
                flag <- TRUE
              else {
                stop("invalid 'bbox' argument")
              }
            }
            if(flag) {
              bbox(.Object) <- bbox
            }
            else  {
              x1(.Object) <- .arg("x1",SVGCoord.factory())
              y1(.Object) <- .arg("y1",SVGCoord.factory())
              x2(.Object) <- .arg("x2",SVGCoord.factory())
              y2(.Object) <- .arg("y2",SVGCoord.factory())
            }
            
            ## eop
            return(.Object)
          }
)

#' @rdname svgmapping.bbox-methods
#' @aliases bbox,Vector-method
setMethod(f="bbox", signature="Vector",
          definition=function(object)
          {
            return(list(x1=x1(object),y1=y1(object),x2=x2(object),y2=y2(object)))
          }
)

#' @name bbox<- 
#' @rdname svgmapping.bbox-methods
#' @aliases bbox<-,Vector-method
setReplaceMethod(f="bbox", signature="Vector",
                 definition=function(.Object,value)
                 {
                   ## check
                   if(!is.list(value))
                     stop("'value' must be a 'list()'")
                   if(!all(list("x1","y1","x2","y2") %in% names(value)))
                     stop("'value' must at least contains 'x1,y1,x2,y2'")
                   
                   ## assign & eop
                   x1(.Object) <- value[["x1"]]
                   y1(.Object) <- value[["y1"]]
                   x2(.Object) <- value[["x2"]]
                   y2(.Object) <- value[["y2"]]
                   return(.Object)
                 }
)

#' @rdname vector.bbox-methods
#' @aliases x1,Vector-method
setMethod(f="x1", signature="Vector",
          definition=function(object)
          {
            return(object@x1)
          }
)

#' @name x1<-
#' @rdname vector.bbox-methods
#' @aliases x1<-,Vector-method
setReplaceMethod(f="x1", signature="Vector",
                 definition=function(.Object,value)
                 {
                   ## check
                   if(is.atomic(value) && 
                        (is.numeric(value) || is.character(value))) 
                     value <- SVGCoord.factory(value)
                   if(!(is.object(value) && is(value,"SVGCoord")))
                     stop("'value' must be an SVGCoord object")
                   
                   ## assign & eop
                   .Object@x1 <- value
                   return(.Object)
                 }
)

#' @rdname vector.bbox-methods
#' @aliases y1,Vector-method
setMethod(f="y1", signature="Vector",
          definition=function(object)
          {
            return(object@y1)
          }
)

#' @name y1<-
#' @rdname vector.bbox-methods
#' @aliases y1<-,Vector-method
setReplaceMethod(f="y1", signature="Vector",
                 definition=function(.Object,value)
                 {
                   ## check
                   if(is.atomic(value) && 
                        (is.numeric(value) || is.character(value))) 
                     value <- SVGCoord.factory(value)
                   if(!(is.object(value) && is(value,"SVGCoord")))
                     stop("'value' must be an SVGCoord object")
                   
                   ## assign & eop
                   .Object@y1 <- value
                   return(.Object)
                 }
)

#' @rdname vector.bbox-methods
#' @aliases x2,Vector-method
setMethod(f="x2", signature="Vector",
          definition=function(object)
          {
            return(object@x2)
          }
)

#' @name x2<-
#' @rdname vector.bbox-methods
#' @aliases x2<-,Vector-method
setReplaceMethod(f="x2", signature="Vector",
                 definition=function(.Object,value)
                 {
                   ## check
                   if(is.atomic(value) && 
                        (is.numeric(value) || is.character(value))) 
                     value <- SVGCoord.factory(value)
                   if(!(is.object(value) && is(value,"SVGCoord")))
                     stop("'value' must be an SVGCoord object")
                   
                   ## assign & eop
                   .Object@x2 <- value
                   return(.Object)
                 }
)

#' @rdname vector.bbox-methods
#' @aliases y2,Vector-method
setMethod(f="y2", signature="Vector",
          definition=function(object)
          {
            return(object@y2)
          }
)

#' @name y2<-
#' @rdname vector.bbox-methods
#' @aliases y2<-,Vector-method
setReplaceMethod(f="y2", signature="Vector",
                 definition=function(.Object,value)
                 {
                   ## check
                   if(is.atomic(value) && 
                        (is.numeric(value) || is.character(value))) 
                     value <- SVGCoord.factory(value)
                   if(!(is.object(value) && is(value,"SVGCoord")))
                     stop("'value' must be an SVGCoord object")
                   
                   ## assign & eop
                   .Object@y2 <- value
                   return(.Object)
                 }
)

#' @rdname svgcore.xml-methods
#' @aliases .xml,Vector-method
setMethod(f=".xml", signature="Vector",
          definition=function(object)
          {
            ## super
            attr <- list()
            zero <- SVGCoord.factory()
            
            ## attributes
            if(x1(object) != zero) attr <- c(attr, x1=as.character(x1(object)))
            if(y1(object) != zero) attr <- c(attr, y1=as.character(y1(object)))
            if(x2(object) != zero) attr <- c(attr, x2=as.character(x2(object)))
            if(y2(object) != zero) attr <- c(attr, y2=as.character(y2(object)))
            
            ## eop
            return(attr)
          }
)
