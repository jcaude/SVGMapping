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
#'  @aliases Circle-class
setClass("Circle",
         representation("VIRTUAL"),
         contains="Ellipse"
)

#' <title already define>
#' 
#' 
#' 
#' \bold{Circle:} the method \code{bbox(object)} return the bounding box 
#' \code{list(cx,cy,r)} of the circle object.
#' 
#' @name bbox
#' 
#' @rdname svgmapping.bbox-methods
#' @exportMethod bbox
#' @docType methods
NULL

#' <title already defined>
#' 
#' 
#' 
#' \bold{Circle:} the \code{bbox{object} <- value} method sets the center and 
#' radius length of a circle object. It is expected that \code{value} is a 
#' list containing the four named values \code{list(cx,cy,r)}.
#' 
#' @name bbox<-
#' 
#'  @rdname svgmapping.bbox-methods
#'  @exportMethod bbox<-
#'  @docType methods  
NULL

#' Coordinates of the Circle object
#' 
#' These methods are accessors to the center and radius of a Circle object.
#' 
#' The \code{r(object)} method returns the radius length of the circle as an
#' \code{\link{SVGLength}} object.
#' 
#' @name r
#'   
#' @rdname circle.bbox-methods
#' @exportMethod r
#' @docType methods
setGeneric(name="r", function(object) { standardGeneric("r") })

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
setGeneric(name="r<-", function(.Object,value) { standardGeneric("r<-") })

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
            
            ## get args
            args = list(...)
            args.names = names(args)
            if(is.null(args.names)) args.names <- list()
            
            ## super (ELLIPSE) w/o arguments
            if(length(args) == 0)
              .Object <- .callMethod("initialize","Ellipse",.Object)
            else {
              ellipse.args <- args[!args.names %in% "bbox"]
              .Object <- .callMethod("initialize","Ellipse",.Object,ellipse.args)
            }
            
            ## default init.
            ## -- we overide all ellipse initialization coords. methods
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
              r(.Object) <- .arg("r",SVGLength.factory())
            }
            
            ## eop
            return(.Object)
          }
)

#' @rdname svgmapping.bbox-methods
#' @aliases bbox,Circle-method
setMethod(f="bbox", signature="Circle",
          definition=function(object)
          {
            return(list(cx=cx(object),cy=cy(object),r=r(object)))
          }
)

#' @name bbox<- 
#' @rdname svgmapping.bbox-methods
#' @aliases bbox<-,Circle-method
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
#' @aliases r,Circle-method
setMethod(f="r", signature="Circle",
          definition=function(object)
          {
            return(rx(object))
          }
)

#' @name r<-
#' @rdname circle.bbox-methods
#' @aliases r<-,Circle-method
setReplaceMethod(f="r", signature="Circle",
                 definition=function(.Object,value)
                 {
                   ## check
                   if(is.atomic(value) && 
                        (is.numeric(value) || is.character(value))) 
                     value <- SVGLength.factory(value)
                   if(!(is.object(value) && is(value,"SVGLength")))
                     stop("'value' must be an SVGLength object")
                   
                   ## assign & eop
                   rx(.Object) <- value
                   ry(.Object) <- value
                   return(.Object)
                 }
)

#' @rdname svgnode.xml-methods
#' @aliases .xml,Circle-method
setMethod(f=".xml", signature="Circle",
          definition=function(object)
          {
            ## super
            attr <- list()
            uzero <- SVGUnit.factory()
            lzero <- SVGLength.factory()
            
            ## attributes
            if(cx(object) != uzero) attr <- c(attr, cx=as.character(cx(object)))
            if(cy(object) != uzero) attr <- c(attr, cy=as.character(cy(object)))
            if(r(object) != lzero) attr <- c(attr, r=as.character(r(object)))
            
            ## eop
            return(attr)
          }
)
