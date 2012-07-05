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

## C I R C L E
## --------------------------------------------------

setGenericVerif <- function(name,y){if(!isGeneric(name)){setGeneric(name,y)}else{}}

#' Circle Definition (VIRTUAL)
#' 
#' This class is used to defined all object that have circle properties.
#' This class can't be directly instantiate and must be derived. 
#' 
#' The circle class is implemented as an circle where both radius are equals
#'  
#'  @seealso \code{\link{Circle}} the parent class
#'  @exportClass "Circle"
setClass("Circle",
         representation("VIRTUAL"),
         contains="Ellipse"
)

#' Coordinates of the Circle object
#' 
#' These methods are accessors to the center and radius of a Circle
#' object.
#' 
#' The method \code{bbox(object)} return the bounding box (cx,cy,r) of 
#' the circle object.
#' 
#' Coordinates or radius length are given as values compliant with the SVG 1.1
#' specifications
#' 
#' @name bbox
#' 
#' @param object an circle object
#' 
#' @return \code{bbox()}: a named list (\code{list(cx,cy,r)}) of values 
#' for the complete bounding-box given as strings.
#' 
#' @rdname circle.bbox-methods
#' @exportMethod bbox
#' @docType methods
NULL
setGenericVerif(name="bbox", function(object) { standardGeneric("bbox") })

#' <title already defined>
#' 
#' 
#' 
#' The \code{bbox{object} <- value} method sets the center and radius length 
#' of a circle object. It is expected that \code{value} is a list containing 
#' the four named values \code{(cx,cy,r)}.
#' 
#' @name bbox<-
#' 
#' @seealso \code{\link{cx}} and \code{\link{cy}} method to set the center of 
#' the circle
#'  @rdname circle.bbox-methods
#'  @exportMethod bbox<-
#'  @docType methods  
NULL
setGenericVerif(name="bbox<-", function(.Object,value) { standardGeneric("bbox<-") })

#' <title already defined>
#' 
#' 
#' 
#' The \code{r(object)} method returns the radius length of the circle.
#' 
#' @name r
#' 
#' @rdname circle.bbox-methods
#' @exportMethod r
#' @docType methods
NULL
setGenericVerif(name="r", function(object) { standardGeneric("r") })

#' <title already defined>
#' 
#' 
#' 
#' The \code{r(object) <- value} method sets the radius length of
#' the circle.
#' 
#' @name r<-
#' 
#'  @rdname circle.bbox-methods
#'  @exportMethod r<-
#'  @docType methods  
NULL
setGenericVerif(name="r<-", function(.Object,value) { standardGeneric("r<-") })

setMethod(f="initialize", signature="Circle",
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
            
            ## super (ELLIPSE)
            .Object <- callNextMethod(.Object,...)
            
            ## get args
            args = list(...)
            args.names = names(args)
            if(is.null(args.names)) args.names <- list()
            
            ## default init.
            ## -- we overide bbox, but not regular super (cx,cy) methods
            flag <- FALSE
            bbox <- list()
            if(sum(grepl("^bbox$", args.names)) > 0) {
              bbox <- args[["bbox"]]
              if( is.list(bbox) &&
                all(list("cx","cy","r") %in% names(bbox)) )
                flag <- TRUE
              else
                stop("invalid 'bbox' argument")
            }
            if(flag) {
              bbox(.Object) <- bbox
            }
            else  {
              r(.Object) <- .arg("r","0")
            }
            
            ## eop
            return(.Object)
          }
)

#' @rdname circle.bbox-methods
setMethod(f="bbox", signature="Circle",
          definition=function(object)
          {
            return(list(cx=cx(object),cy=cy(object),r=r(object)))
          }
)

#' @name bbox<- 
#' @rdname circle.bbox-methods
setReplaceMethod(f="bbox", signature="Circle",
                 definition=function(.Object,value)
                 {
                   ## check
                   if(!is.list(value))
                     stop("'value' must be a 'list()'")
                   if(!all(list("cx","cy","r") %in% names(value)))
                     stop("'value' must at least contains 'cx,cy,r'")
                   
                   ## assign & eop
                   cx(.Object) <- value[["cx"]]
                   cy(.Object) <- value[["cy"]]
                   r(.Object) <- value[["r"]]
                   return(.Object)
                 }
)

#' @rdname circle.bbox-methods
setMethod(f="r", signature="Circle",
          definition=function(object)
          {
            return(rx(object))
          }
)

#' @name r<-
#' @rdname circle.bbox-methods
setReplaceMethod(f="r", signature="Circle",
                 definition=function(.Object,value)
                 {
                   ## check
                   if(!is.atomic(value))
                     stop("'value' must be atomic")
                   if(is.numeric(value)) value <- as.character(value)
                   if(!is.character(value))
                     stop("'value' must be an atomic string")
                   
                   ## assign & eop
                   rx(.Object) <- value
                   ry(.Object) <- value
                   return(.Object)
                 }
)

#' @aliases .xml,Circle-method
#' @rdname svgnode.xml-methods
setMethod(f=".xml", signature="Circle",
          definition=function(object)
          {
            ## super
            attr <- list()
            
            ## attributes
            if(cx(object) != "0") attr <- c(attr, cx=cx(object))
            if(cy(object) != "0") attr <- c(attr, cy=cy(object))
            if(r(object) != "0") attr <- c(attr, r=r(object))
            
            ## eop
            return(attr)
          }
)
