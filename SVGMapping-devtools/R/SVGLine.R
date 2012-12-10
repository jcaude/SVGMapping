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

#' SVG Line class
#' 
#' This class is used to describe a line, as part of the SVG 1.1 specifications.
#' This class inherited from the SVG Shape class.
#'  
#'  @seealso \code{\link{SVGShape}} parent class.
#'  @exportClass "SVGLine"
#'  @aliases SVGLine-class
setClass("SVGLine",
         representation(),
         contains=c("SVGShape","Vector")
         )

#' Print an SVG object to the terminal
#' 
#' These methods attempt to print SVG object to the terminal as text XML
#' 
#' @name print.SVGLine
#' 
#' @param x the SVG object to print
#' 
#' @return the SVG object invisibly
#' 
#' @rdname svgmapping.print-methods
#' @exportMethod print.SVGLine
#' @docType methods
setGeneric(name="print.SVGLine", function(x,...) { standardGeneric("print.SVGLine") })


setMethod(f="initialize", signature="SVGLine",
          definition=function(.Object,...)
          {
            ## super (SHAPE)
            .Object <- callNextMethod(.Object,...)

            ## super (VECTOR)
            .Object <- .callMethod("initialize","Vector",.Object,...)

            ## eop
            return(.Object)
          }
          )

#' @rdname svgmapping.print-methods
#' @aliases print.SVGLine,SVGLine-method
setMethod(f="print.SVGLine", signature="SVGLine",
          definition=function(x,...)
          {
            rect.node <- .xml(x)
            print(rect.node)
            return(invisible(x))
          }
          )

#' @rdname svgcore.xml-methods
#' @aliases .xml,SVGLine-method
setMethod(f=".xml", signature="SVGLine",
          definition=function(object)
          {
            ## init.
            line <- newXMLNode("line")

            ## super (SHAPE)
            attr <- callNextMethod(object)
            
            ## super (VECTOR)
            attr <- c(attr,.callMethod(".xml","Vector",object))
            
            ## eop
            xmlAttrs(line) <- attr
            return(line)
          }
          )
          
## F A C T O R Y
## --------------------------------------------------
#' SVGLine Factory
#' 
#' This function returns an SVG Line instance given its coordinates and 
#' optionally its class (CSS), style (CSS) and a geometric transformation.
#' 
#' The usage of \code{bbox} and \code{(x1,y1,x2,y2)} are mutually exclusive. 
#' 
#' If calls with no arguments, this function returns an \emph{empty} line 
#' (\emph{i.e.} all coordinates are set to zero).
#' 
#' @name SVGLine.factory
#' 
#' @param bbox a list of coordinates \code{(x1,y1,x2,y2)}
#' @param x1 the X-axis coordinate of the first extremum point
#' @param y1 the Y-axis coordinate of the first extremum point
#' @param x2 the X-axis coordinate of the second extremum point
#' @param y2 the Y-axis coordinate of the second extremum point
#' @param class the CSS class name
#' @param style the CSS style description
#' @param transform the geometric transformation to apply to the coordinates
#' 
#' @return an \code{\link{SVGLine}} instance
#' 
#' @seealso The \code{\link{SVGLine}} class definition
#' @export SVGLine.factory
#' 
#' @examples
#' ## Simple line
#' line <- SVGLine.factory(x1="10cm",y1="10cm",x2="40cm",y2="15cm")
#' ## Create a line using a list of coordinates
#' line.coords <- list(x1="5px",y1="10px",x2="650px",y2="315px")
#' ## A line with some CSS
#' line2 <- SVGLine.factory(bbox=line.coords,class="mylines",style="stroke-width:15cm")
#' ## A line (0,0,10,10) translated of 10 to the right and 20 below, 
#' ## new coordinates are (10,20,20,30)
#' line3 <- SVGLine.factory(0,0,10,10,transform="translate(10,20)")
SVGLine.factory <- function(x1,y1,x2,y2,bbox,class,style,transform) {

  ## init.
  args <- list("SVGLine")
  if(!missing(x1)) args <- c(args,x1=x1)
  if(!missing(y1)) args <- c(args,y1=y1)
  if(!missing(x2)) args <- c(args,x2=x2)
  if(!missing(y2)) args <- c(args,y2=y2)
  if(!missing(bbox)) args <- c(args,bbox=list(bbox))
  if(!missing(class)) args <- c(args,class=class)
  if(!missing(style)) args <- c(args,style=style)
  if(!missing(transform)) args <- c(args,transform=transform)
  line <- do.call(new,args)

  ## eop
  return(line)
}
