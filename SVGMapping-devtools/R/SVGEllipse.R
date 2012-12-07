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

## S V G   E L L I P S E
## --------------------------------------------------

setGenericVerif <- function(name,y){if(!isGeneric(name)){setGeneric(name,y)}else{}}

#' SVG Ellipse Definition
#' 
#' This class is used to describe an ellipse, as part of the SVG 1.1
#' specifications. This class inherited from the SVG Shape class.
#' 
#' @seealso \code{\link{SVGShape}} parent class.
#'  
#' @exportClass "SVGEllipse"
#' @aliases SVGEllipse-class
setClass("SVGEllipse",
         contains=c("SVGShape","Ellipse")
)

#' @name print.SVGEllipse
#' 
#' @rdname svgmapping.print-methods
#' @exportMethod print.SVGEllipse
#' @docType methods
setGeneric(name="print.SVGEllipse", function(x,...) { standardGeneric("print.SVGEllipse") })

setGenericVerif(name=".xml", function(object) { standardGeneric(".xml") })

setMethod(f="initialize", signature="SVGEllipse",
          definition=function(.Object,...)
          {            
            # super (SVGShape)
            .Object <- callNextMethod(.Object,...)
            
            # super (Ellipse)
            .Object <- .callMethod("initialize","Ellipse",.Object,...)
            
            # eop
            return(.Object)
          }
)

#' @rdname svgmapping.print-methods
#' @aliases print.SVGEllipse,SVGEllipse-method
setMethod(f="print.SVGEllipse", signature="SVGEllipse",
          definition=function(x,...)
          {
            ellipse.node <- .xml(x)
            print(ellipse.node)
          }
)

#' @rdname svgnode.xml-methods
#' @aliases .xml,SVGEllipse-method
setMethod(f=".xml", signature="SVGEllipse",
          definition=function(object)
          {
            ## init.
            ellipse <- newXMLNode("ellipse")
            
            ## core attributes (SVGShape)
            attr <- callNextMethod(object)
            
            ## shape attribures (Rectangle)
            attr <- c(attr,.callMethod(".xml", "Ellipse",object))
            
            ## attributes
            xmlAttrs(ellipse) <- attr
            
            ## eop
            return(ellipse)
          }
)

## F A C T O R Y
## --------------------------------------------------

#' SVGEllipse Factory
#' 
#' This function returns an SVG Ellipse instance given its coordinates and 
#' optionally its class (CSS), style (CSS) and a geometric transformation.
#' 
#' The usage of \code{bbox} and \code{(x,y,width,height)} are mutually 
#' exclusive.
#' 
#' If calls with no arguments, this function returns an \emph{empty} line 
#' (\emph{i.e.} all coordinates are set to zero).
#' 
#' @name SVGEllipse.factory
#' 
#' @param bbox a list of coordinates \code{(cx,cy,rx,ry)}
#' @param cx the X-axis coordinate of the center
#' @param cy the Y-axis coordinate of the center
#' @param rx the X-axis radius length of the ellipse
#' @param ry the Y-axis radius length of the ellipse
#' @param class the CSS class name
#' @param style the CSS style description
#' @param transform the geometric transformation to apply to the coordinates
#' 
#' @return an \code{\link{SVGEllipse}} instance
#' 
#' @export SVGEllipse.factory
#' 
#' @examples
#' ## Simple ellipse
#' ellipse <- SVGEllipse.factory(cx="3cm",cy="2cm",rx="15cm",ry="10cm")
#' ## Create an ellipse using bounding-box list
#' el.bbox <- list(cx="30px",cy="25px",rx="250px",ry="135px")
#' ellipse <- SVGEllipse.factory(bbox=el.bbox)
#' ## An ellipse with some inline CSS decorations
#' ellipse <- SVGEllipse.factory(bbox=el.bbox,class="mylines",style="stroke-width:10cm")
#' ## An ellipse (0,0) translated of 20 to the right and 10 below, 
#' ## new coordinates are (20,10)
#' ellipse <- SVGEllipse.factory(cx=0,cy=0,rx=60,ry=30,transform="translate(20,10)")
SVGEllipse.factory <- function(cx,cy,rx,ry,bbox,class,style,transform) {
  
  ## init.
  args <- list("SVGEllipse")
  if(!missing(bbox))
    args <- c(args,bbox=list(bbox))
  else {
    if(!missing(cx)) args <- c(args,cx=cx)
    if(!missing(cy)) args <- c(args,cy=cy)
    if(!missing(rx)) args <- c(args,rx=rx)
    if(!missing(ry)) args <- c(args,ry=ry)
  }
  if(!missing(class)) args <- c(args,class=class)
  if(!missing(style)) args <- c(args,style=style)
  if(!missing(transform)) args <- c(args,transform=transform)
  ellipse <- do.call(new,args)
  
  ## eop
  return(ellipse)
}


