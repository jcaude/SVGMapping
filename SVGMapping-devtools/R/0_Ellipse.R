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
#'  @aliases Ellipse-class
setClass("Ellipse",
         representation(cx="SVGCoord",
                        cy="SVGCoord",
                        rx="SVGLength",
                        ry="SVGLength",
                        "VIRTUAL")
)

#' <title already defined>
#' 
#' 
#' 
#' @name bbox
#' 
#' @rdname svgmapping.bbox-methods
#' @exportMethod bbox
#' @docType methods
NULL

#' Location and dimensions of the Ellipse object
#' 
#' These methods are accessors to the location and dimensions of an
#' \code{Ellipse} object
#' 
#' The \code{coords{object} <- value} method sets the location of the center and
#' radius length of an ellipse object. It is expected that \code{value} is a 
#' list containing the four named values \code{list(cx,cy,rx,ry)}.
#' 
#' @name coords<-
#' 
#' @rdname ellipse.coords-methods
#' @exportMethod coords<-
#' @docType methods  
NULL

#' <title already defined>
#' 
#' 
#' 
#' The \code{cx(object)} method returns the X-axis center of the ellipse.
#' 
#' @return coordinates as SVGCoords (\code{cx,cy}) or SVGLength (\code{rx,ry})
#'   objects
#'   
#' @name cx
#'   
#' @rdname ellipse.coords-methods
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
#' @rdname ellipse.coords-methods
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
#' @rdname ellipse.coords-methods
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
#' @rdname ellipse.coords-methods
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
#' @rdname ellipse.coords-methods
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
#'  @rdname ellipse.coords-methods
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
#' @rdname ellipse.coords-methods
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
#'  @rdname ellipse.coords-methods
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
            if(sum(grepl("^coords$", args.names)) > 0) {
              bbox <- args[["coords"]]
              if( is.list(bbox) &&
                all(list("cx","cy","rx","ry") %in% names(bbox)) )
                flag <- TRUE
              else
                stop("invalid 'coords' argument")
            }
            if(flag) {
              coords(.Object) <- bbox
            }
            else  {
              cx(.Object) <- .arg("cx",SVGCoord.factory())
              cy(.Object) <- .arg("cy",SVGCoord.factory())
              rx(.Object) <- .arg("rx",SVGLength.factory())
              ry(.Object) <- .arg("ry",SVGLength.factory())
            }
            
            ## eop
            return(.Object)
          }
)

#' @rdname svgmapping.bbox-methods
#' @aliases bbox,Ellipse-method
setMethod(f="bbox", signature="Ellipse",
          definition=function(object)
          {
            # eop
            return(list(x1=cx(object)-rx(object),
                        y1=cy(object)-ry(object),
                        x2=cx(object)+rx(object),
                        y2=cy(object)+ry(object)
                        )
            )
          }
)

#' @name coords<- 
#' @rdname ellipse.coords-methods
#' @aliases coords<-,Ellipse-method
setReplaceMethod(f="coords", signature="Ellipse",
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

#' @rdname ellipse.coords-methods
#' @aliases cx,Ellipse-method
setMethod(f="cx", signature="Ellipse",
          definition=function(object)
          {
            return(object@cx)
          }
)

#' @name cx<-
#' @rdname ellipse.coords-methods
#' @aliases cx<-,Ellipse-method
setReplaceMethod(f="cx", signature="Ellipse",
                 definition=function(.Object,value)
                 {
                   ## check
                   if(is.atomic(value) && 
                        (is.numeric(value) || is.character(value))) 
                     value <- SVGCoord.factory(value)
                   if(!(is.object(value) && is(value,"SVGCoord")))
                     stop("'value' must be an SVGCoord object")
                   
                   ## assign & eop
                   .Object@cx <- value
                   return(.Object)
                 }
)

#' @rdname ellipse.coords-methods
#' @aliases cy,Ellipse-method
setMethod(f="cy", signature="Ellipse",
          definition=function(object)
          {
            return(object@cy)
          }
)

#' @name cy<-
#' @rdname ellipse.coords-methods
#' @aliases cy<-,Ellipse-method
setReplaceMethod(f="cy", signature="Ellipse",
                 definition=function(.Object,value)
                 {
                   ## check
                   if(is.atomic(value) && 
                        (is.numeric(value) || is.character(value))) 
                     value <- SVGCoord.factory(value)
                   if(!(is.object(value) && is(value,"SVGCoord")))
                     stop("'value' must be an SVGCoord object")
                   
                   ## assign & eop
                   .Object@cy <- value
                   return(.Object)
                 }
)

#' @rdname ellipse.coords-methods
#' @aliases rx,Ellipse-method
setMethod(f="rx", signature="Ellipse",
          definition=function(object)
          {
            return(object@rx)
          }
)

#' @name rx<-
#' @rdname ellipse.coords-methods
#' @aliases rx<-,Ellipse-method
setReplaceMethod(f="rx", signature="Ellipse",
                 definition=function(.Object,value)
                 {
                   ## check
                   if(is.atomic(value) && 
                        (is.numeric(value) || is.character(value))) 
                     value <- SVGLength.factory(value)
                   if(!(is.object(value) && is(value,"SVGLength")))
                     stop("'value' must be an SVGLength object")
                   
                   ## assign & eop
                   .Object@rx <- value
                   return(.Object)
                 }
)

#' @rdname ellipse.coords-methods
#' @aliases ry,Ellipse-method
setMethod(f="ry", signature="Ellipse",
          definition=function(object)
          {
            return(object@ry)
          }
)

#' @name ry<-
#' @rdname ellipse.coords-methods
#' @aliases ry<-,Ellipse-method
setReplaceMethod(f="ry", signature="Ellipse",
                 definition=function(.Object,value)
                 {
                   ## check
                   if(is.atomic(value) && 
                        (is.numeric(value) || is.character(value))) 
                     value <- SVGLength.factory(value)
                   if(!(is.object(value) && is(value,"SVGLength")))
                     stop("'value' must be an SVGLength object")
                   
                   ## assign & eop
                   .Object@ry <- value
                   return(.Object)
                 }
)

#' @rdname svgcore.xml-methods
#' @aliases .xml,Ellipse-method
setMethod(f=".xml", signature="Ellipse",
          definition=function(object)
          {
            ## init.
            uzero <- SVGCoord.factory()
            lzero <- SVGLength.factory()
            
            ## super
            attr <- list()
            
            ## attributes
            if(cx(object) != uzero) attr <- c(attr, cx=as.character(cx(object)))
            if(cy(object) != uzero) attr <- c(attr, cy=as.character(cy(object)))
            if(rx(object) != lzero) attr <- c(attr, rx=as.character(rx(object)))
            if(ry(object) != lzero) attr <- c(attr, ry=as.character(ry(object)))
            
            ## eop
            return(attr)
          }
)

