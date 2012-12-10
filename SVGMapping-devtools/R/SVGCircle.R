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

## S V G   C I R C L E
## --------------------------------------------------

setGenericVerif <- function(name,y){if(!isGeneric(name)){setGeneric(name,y)}else{}}

#' SVG Circle Definition
#' 
#' This class is used to describe a circle, as part of the SVG 1.1
#' specifications. This class inherited from the SVG Shape class.
#' 
#' @seealso \code{\link{SVGShape}} parent class.
#'  
#' @exportClass "SVGCircle"
#' @aliases SVGCircle-class
setClass("SVGCircle",
         contains=c("SVGShape","Circle")
)

#' @name print.SVGCircle
#' 
#' @rdname svgmapping.print-methods
#' @exportMethod print.SVGCircle
#' @docType methods
setGeneric(name="print.SVGCircle", function(x,...) { standardGeneric("print.SVGCircle") })

setMethod(f="initialize", signature="SVGCircle",
          definition=function(.Object,...)
          {            
            # super (SVGShape)
            .Object <- callNextMethod(.Object,...)
            
            # super (Ellipse)
            .Object <- .callMethod("initialize","Circle",.Object,...)
            
            # eop
            return(.Object)
          }
)

#' @rdname svgmapping.print-methods
#' @aliases print.SVGCircle,SVGCircle-method
setMethod(f="print.SVGCircle", signature="SVGCircle",
          definition=function(x,...)
          {
            circle.node <- .xml(x)
            print(circle.node)
          }
)

#' @rdname svgcore.xml-methods
#' @aliases .xml,SVGCircle-method
setMethod(f=".xml", signature="SVGCircle",
          definition=function(object)
          {
            ## init.
            circle <- newXMLNode("circle")
            
            ## core attributes (SVGShape)
            attr <- callNextMethod(object)
            
            ## shape attribures (Rectangle)
            attr <- c(attr,.callMethod(".xml", "Circle",object))
            
            ## attributes
            xmlAttrs(circle) <- attr
            
            ## eop
            return(circle)
          }
)

## F A C T O R Y
## --------------------------------------------------

#' SVGCircle Factory
#' 
#' This function returns an SVG Circle instance given its coordinates and 
#' optionally its class (CSS), style (CSS) and a geometric transformation.
#' 
#' The usage of \code{bbox} and \code{(cx,cy,r)} are mutually exclusive.
#' 
#' If calls with no arguments, this function returns an \emph{empty} line 
#' (\emph{i.e.} all coordinates are set to zero).
#' 
#' @name SVGCircle.factory
#'   
#' @param bbox a list of coordinates \code{(cx,cy,r)}
#' @param cx the X-axis coordinate of the center
#' @param cy the Y-axis coordinate of the center
#' @param r the radius length of the circle
#' @param class the CSS class name
#' @param style the CSS style description
#' @param transform the geometric transformation to apply to the coordinates
#'   
#' @return an \code{\link{SVGCircle}} instance
#'   
#' @export SVGCircle.factory
#'   
#' @examples
#' ## Simple circle
#' circle <- SVGCircle.factory(cx="3cm",cy="2cm",r="15cm")
#' ## Create a circle using bounding-box list
#' el.bbox <- list(cx="30px",cy="25px",r="135px")
#' circle <- SVGCircle.factory(bbox=el.bbox)
#' ## A circle with some inline CSS decorations
#' circle <- SVGCircle.factory(bbox=el.bbox,class="mylines",style="stroke-width:10cm")
#' ## A circle (0,0) translated of 20 to the right and 10 below, 
#' ## new coordinates are (20,10)
#' circle <- SVGCircle.factory(cx=0,cy=0,r=30,transform="translate(20,10)")
SVGCircle.factory <- function(cx,cy,r,bbox,class,style,transform) {
  
  ## init.
  args <- list("SVGCircle")
  if(!missing(bbox))
    args <- c(args,bbox=list(bbox))
  else {
    if(!missing(cx)) args <- c(args,cx=cx)
    if(!missing(cy)) args <- c(args,cy=cy)
    if(!missing(r)) args <- c(args,r=r)
  }
  if(!missing(class)) args <- c(args,class=class)
  if(!missing(style)) args <- c(args,style=style)
  if(!missing(transform)) args <- c(args,transform=transform)
  circle <- do.call(new,args)
  
  ## eop
  return(circle)
}


