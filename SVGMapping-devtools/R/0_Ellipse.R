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

## E L L I P S E
## --------------------------------------------------

setGenericVerif <- function(name,y){if(!isGeneric(name)){setGeneric(name,y)}else{}}

#' Ellipse Definition (VIRTUAL)
#' 
#' This class is used to defined all object that have elliptic properties.
#' This class can't be directly instantiate and must be derived. 
#' 
#'  @exportClass "Ellipse"
setClass("Ellipse",
         representation(cx="character",
                        cy="character",
                        rx="character",
                        ry="character",
                        "VIRTUAL"),
         prototype(cx="0",cy="0",rx="0",ry="0")
)

#' Coordinates of the Ellipse object
#' 
#' These methods are accessors to the center and radius of an Ellipse
#' object.
#' 
#' The method \code{bbox(object)} return the bounding box (cx,cy,rx,ry) of 
#' the ellipse object.
#' 
#' Coordinates or radius length are given as values compliant with the SVG 1.1
#' specifications
#' 
#' @name bbox
#' 
#' @param object an ellipse object
#' 
#' @return \code{bbox()}: a named list (\code{list(cx,cy,rx,ry)}) of values 
#' for the complete bounding-box given as strings.
#' 
#' @rdname ellipse.bbox-methods
#' @exportMethod bbox
#' @docType methods
NULL
#setGeneric(name="bbox", function(object) { standardGeneric("bbox") })

#' <title already defined>
#' 
#' 
#' 
#' The \code{bbox{object} <- value} method sets the center and radius length 
#' of an ellipse object. It is expected that \code{value} is a list containing 
#' the four named values \code{(cx,cy,rx,ry)}.
#' 
#' @name bbox<-
#' 
#' @rdname ellipse.bbox-methods
#' @exportMethod bbox<-
#' @docType methods  
NULL
#setGeneric(name="bbox<-", function(.Object,value) { standardGeneric("bbox<-") })

#' <title already defined>
#' 
#' 
#' 
#' The \code{cx(object)} method returns the X-axis center of the ellipse.
#' 
#' @return coordinates in units compliant with the SVG 1.1 specifications
#' 
#' @name cx
#' 
#' @rdname ellipse.bbox-methods
#' @exportMethod cx
#' @docType methods
setGeneric(name="cx", function(object) { standardGeneric("cx") })

#' <title already defined>
#' 
#' 
#' 
#' The \code{cx(object) <- value} method sets the X-axis center of
#' the ellipse.
#' 
#' @name cx<-
#' 
#' @rdname ellipse.bbox-methods
#' @exportMethod cx<-
#' @docType methods  
setGeneric(name="cx<-", function(.Object,value) { standardGeneric("cx<-") })

#' <title already defined>
#' 
#' 
#' 
#' The \code{cy(object)} method returns the Y-axis center of the ellipse.
#' 
#' @name cy
#' 
#' @rdname ellipse.bbox-methods
#' @exportMethod cy
#' @docType methods
setGeneric(name="cy", function(object) { standardGeneric("cy") })

#' <title already defined>
#' 
#' 
#' 
#' The \code{cy(object) <- value} method sets the Y-axis center of
#' the ellipse.
#' 
#' @name cy<-
#' 
#' @rdname ellipse.bbox-methods
#' @exportMethod cy<-
#' @docType methods  
setGeneric(name="cy<-", function(.Object,value) { standardGeneric("cy<-") })

#' <title already defined>
#' 
#' 
#' 
#' The \code{rx(object)} method returns the X-axis radius length of the ellipse.
#' 
#' @name rx
#' 
#' @rdname ellipse.bbox-methods
#' @exportMethod rx
#' @docType methods
setGeneric(name="rx", function(object) { standardGeneric("rx") })

#' <title already defined>
#' 
#' 
#' 
#' The \code{rx(object) <- value} method sets the X-axis radius length of
#' the ellipse.
#' 
#' @name rx<-
#' 
#'  @rdname ellipse.bbox-methods
#'  @exportMethod rx<-
#'  @docType methods  
setGeneric(name="rx<-", function(.Object,value) { standardGeneric("rx<-") })

#' <title already defined>
#' 
#' 
#' 
#' The \code{ry(object)} method returns the Y-axis radius length of the ellipse.
#' 
#' @name ry
#' 
#' @rdname ellipse.bbox-methods
#' @exportMethod ry
#' @docType methods
setGeneric(name="ry", function(object) { standardGeneric("ry") })

#' <title already defined>
#' 
#' 
#' 
#' The \code{ry(object) <- value} method sets the Y-axis radius length of
#' the ellipse.
#' 
#' @name ry<-
#' 
#'  @rdname ellipse.bbox-methods
#'  @exportMethod ry<-
#'  @docType methods  
setGeneric(name="ry<-", function(.Object,value) { standardGeneric("ry<-") })

setMethod(f="initialize", signature="Ellipse",
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
                all(list("cx","cy","rx","ry") %in% names(bbox)) )
                flag <- TRUE
              else
                stop("invalid 'bbox' argument")
            }
            if(flag) {
              bbox(.Object) <- bbox
            }
            else  {
              cx(.Object) <- .arg("cx","0")
              cy(.Object) <- .arg("cy","0")
              rx(.Object) <- .arg("rx","0")
              ry(.Object) <- .arg("ry","0")
            }
            
            ## eop
            return(.Object)
          }
)

#' @rdname ellipse.bbox-methods
setMethod(f="bbox", signature="Ellipse",
          definition=function(object)
          {
            return(list(cx=cx(object),cy=cy(object),rx=rx(object),ry=ry(object)))
          }
)

#' @name bbox<- 
#' @rdname ellipse.bbox-methods
setReplaceMethod(f="bbox", signature="Ellipse",
                 definition=function(.Object,value)
                 {
                   ## check
                   if(!is.list(value))
                     stop("'value' must be a 'list()'")
                   if(!all(list("cx","cy","rx","ry") %in% names(value)))
                     stop("'value' must at least contains 'cx,cy,rx,ry'")
                   
                   ## assign & eop
                   cx(.Object) <- value[["cx"]]
                   cy(.Object) <- value[["cy"]]
                   rx(.Object) <- value[["rx"]]
                   ry(.Object) <- value[["ry"]]
                   return(.Object)
                 }
)

#' @rdname ellipse.bbox-methods
setMethod(f="cx", signature="Ellipse",
          definition=function(object)
          {
            return(object@cx)
          }
)

#' @name cx<-
#' @rdname ellipse.bbox-methods
setReplaceMethod(f="cx", signature="Ellipse",
                 definition=function(.Object,value)
                 {
                   ## check
                   if(!is.atomic(value))
                     stop("'value' must be atomic")
                   if(is.numeric(value)) value <- as.character(value)
                   if(!is.character(value))
                     stop("'value' must be an atomic string")
                   
                   ## assign & eop
                   .Object@cx <- value
                   return(.Object)
                 }
)

#' @rdname ellipse.bbox-methods
setMethod(f="cy", signature="Ellipse",
          definition=function(object)
          {
            return(object@cy)
          }
)

#' @name cy<-
#' @rdname ellipse.bbox-methods
setReplaceMethod(f="cy", signature="Ellipse",
                 definition=function(.Object,value)
                 {
                   ## check
                   if(!is.atomic(value))
                     stop("'value' must be atomic")
                   if(is.numeric(value)) value <- as.character(value)
                   if(!is.character(value))
                     stop("'value' must be an atomic string")
                   
                   ## assign & eop
                   .Object@cy <- value
                   return(.Object)
                 }
)

#' @rdname ellipse.bbox-methods
setMethod(f="rx", signature="Ellipse",
          definition=function(object)
          {
            return(object@rx)
          }
)

#' @name rx<-
#' @rdname ellipse.bbox-methods
setReplaceMethod(f="rx", signature="Ellipse",
                 definition=function(.Object,value)
                 {
                   ## check
                   if(!is.atomic(value))
                     stop("'value' must be atomic")
                   if(is.numeric(value)) value <- as.character(value)
                   if(!is.character(value))
                     stop("'value' must be an atomic string")
                   
                   ## assign & eop
                   .Object@rx <- value
                   return(.Object)
                 }
)

#' @rdname ellipse.bbox-methods
setMethod(f="ry", signature="Ellipse",
          definition=function(object)
          {
            return(object@ry)
          }
)

#' @name ry<-
#' @rdname ellipse.bbox-methods
setReplaceMethod(f="ry", signature="Ellipse",
                 definition=function(.Object,value)
                 {
                   ## check
                   if(!is.atomic(value))
                     stop("'value' must be atomic")
                   if(is.numeric(value)) value <- as.character(value)
                   if(!is.character(value))
                     stop("'value' must be an atomic string")
                   
                   ## assign & eop
                   .Object@ry <- value
                   return(.Object)
                 }
)

#' @aliases .xml,Ellipse-method
#' @rdname svgnode.xml-methods
setMethod(f=".xml", signature="Ellipse",
          definition=function(object)
          {
            ## super
            attr <- list()
            
            ## attributes
            if(cx(object) != "0") attr <- c(attr, cx=cx(object))
            if(cy(object) != "0") attr <- c(attr, cy=cy(object))
            if(rx(object) != "0") attr <- c(attr, rx=rx(object))
            if(ry(object) != "0") attr <- c(attr, ry=ry(object))
            
            ## eop
            return(attr)
          }
)

