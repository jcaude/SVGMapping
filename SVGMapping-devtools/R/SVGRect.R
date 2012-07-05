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

## S V G R E C T
## --------------------------------------------------

setGenericVerif <- function(name,y){if(!isGeneric(name)){setGeneric(name,y)}else{}}

#' SVG Rect class
#' 
#' This class is used to describe a rectangle, as part of the SVG 1.1 specifications.
#' This class inherited from the SVG Shape class.
#'  
#'  @seealso \code{\link{SVGShape}} parent class.
#'  @exportClass "SVGRect"
setClass("SVGRect",
         representation(rx="character",
                        ry="character"),
         contains=c("SVGShape","Rectangle")
         )

#' @name print.SVGRect
#' 
#' @rdname svgmapping.print-methods
#' @exportMethod print.SVGRect
#' @docType methods
setGeneric(name="print.SVGRect", function(x,...) { standardGeneric("print.SVGRect") })

#' Round Off Corners Accessors
#' 
#' The methods get or set the round off corners of an SVG Shape.
#' 
#' The \code{rx(object)} method returns the \emph{X-axis} radius of the ellipse 
#' used to round off the corners of the shape.
#' 
#' @name rx
#'  
#' @param object the SVG shape instance
#' 
#' @return the length in SVG units  
#' 
#' @rdname svgrect.roundoff-methods
#' @exportMethod rx
#' @docType methods
setGeneric(name="rx", function(object) { standardGeneric("rx") })

#' <title already defined>
#' 
#' 
#' 
#' The \code{rx(object) <- value} method can be used to set the \emph{X-axis} 
#' radius of the ellipse used to round off the corners of the shape.
#' 
#' @name rx<- 
#' 
#' @rdname svgrect.roundoff-methods
#' @exportMethod rx<- 
#' @docType methods
setGeneric(name="rx<-", function(.Object,value) { standardGeneric("rx<-") })

#' <title already defined>
#' 
#' 
#' 
#' The \code{ry(object)} method returns the \emph{Y-axis} radius of the ellipse 
#' used to round off the corners of the shape.
#' 
#' @name ry
#' 
#' @rdname svgrect.roundoff-methods
#' @exportMethod ry
#' @docType methods
setGeneric(name="ry", function(object) { standardGeneric("ry") })

#' <title already defined>
#' 
#' 
#' 
#' The \code{ry(object) <- value} method can be used to set the \emph{Y-axis} 
#' radius of the ellipse used to round off the corners of the shape.
#' 
#' @name ry<- 
#' 
#' @rdname svgrect.roundoff-methods
#' @exportMethod ry<- 
#' @docType methods
setGeneric(name="ry<-", function(.Object,value) { standardGeneric("ry<-") })

setGenericVerif(name=".xml", function(object) { standardGeneric(".xml") })

setMethod(f="initialize", signature="SVGRect",
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

            ## super (SVGShape)
            .Object <- callNextMethod(.Object,...)

            ## super (Rectangle)
            .Object <- .callMethod("initialize","Rectangle",.Object,...)
             
            ## get args
            args = list(...)
            args.names = names(args)
            if(is.null(args.names)) args.names <- list()

            ## default init.
            rx(.Object) <- .arg("rx",character(0))
            ry(.Object) <- .arg("ry",character(0))

            ## eop
            return(.Object)
          }
          )

#' @rdname svgmapping.print-methods
setMethod(f="print.SVGRect", signature="SVGRect",
          definition=function(x,...)
          {
            rect.node <- .xml(x)
            print(rect.node)
          }
          )

#' @rdname svgrect.roundoff-methods
setMethod(f="rx", signature="SVGRect",
          definition=function(object)
          {
            return(object@rx)
          }
          )

#' @name rx<- 
#' @rdname svgrect.roundoff-methods
setReplaceMethod(f="rx", signature="SVGRect",
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

#' @rdname svgrect.roundoff-methods
setMethod(f="ry", signature="SVGRect",
          definition=function(object)
          {
            return(object@ry)
          }
          )

#' @name ry<-
#' @rdname svgrect.roundoff-methods
setReplaceMethod(f="ry", signature="SVGRect",
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

#' @rdname svgnode.xml-methods
setMethod(f=".xml", signature="SVGRect",
          definition=function(object)
          {
            ## init.
            rect <- newXMLNode("rect")

            ## core attributes (SVGShape)
            attr <- callNextMethod(object)
            
            ## shape attribures (Rectangle)
            attr <- c(attr,.callMethod(".xml", "Rectangle",object))

            ## attributes
            if(length(rx(object)) > 0) attr <- c(attr, rx=rx(object))
            if(length(ry(object)) > 0) attr <- c(attr, ry=ry(object))
            xmlAttrs(rect) <- attr

            ## eop
            return(rect)
          }
          )
          
## F A C T O R Y
## --------------------------------------------------

#' SVGRect Factory
#' 
#' This function returns an SVG Rectangle instance given its coordinates and 
#' optionally its class (CSS), style (CSS) and a geometric transformation.
#' 
#' The usage of \code{bbox} and \code{(x,y,width,height)} are mutually 
#' exclusive.
#' 
#' If calls with no arguments, this function returns an \emph{empty} line 
#' (\emph{i.e.} all coordinates are set to zero).
#' 
#' @name SVGRect.factory
#' 
#' @param bbox a list of coordinates \code{(x,y,width,height)}
#' @param x the X-axis coordinate of the upper left corner
#' @param y the Y-axis coordinate of the upper left corner
#' @param width the horizontal length of the rectangle
#' @param height the vertical length of the rectangle
#' @param class the CSS class name
#' @param style the CSS style description
#' @param transform the geometric transformation to apply to the coordinates
#' 
#' @return an \code{\link{SVGRect}} instance
#' 
#' @seealso The \code{\link{SVGRect}} class definition; the \code{\link{rx}} 
#' and \code{\link{ry}} methods to set the round off corners length.
#' 
#' @export SVGRect.factory
#' 
#' @examples
#' ## Simple line
#' rect <- SVGRect.factory(x="5cm",y="5cm",x2="60cm",y2="30cm")
#' ## Create a rectangle using bounding-box list
#' rect.bbox <- list(x="5px",y="5px",width="650px",height="315px")
#' rect <- SVGRect.factory(bbox=rect.bbox)
#' ## A rectangle with some inline CSS decorations
#' rect <- SVGRect.factory(bbox=rect.bbox,class="mylines",style="stroke-width:15cm")
#' ## A rectangle (0,0) translated of 10 to the right and 20 below, 
#' ## new coordinates are (10,20)
#' rect <- SVGRect.factory(x=0,y=0,width=60,height=30,transform="translate(10,20)")
SVGRect.factory <- function(bbox,x,y,width,height,rx,ry,class,style,transform) {

  ## init.
  args <- list("SVGRect")
  if(!missing(bbox)) args <- c(args,bbox=list(bbox))
  if(!missing(x)) args <- c(args,x=x)
  if(!missing(y)) args <- c(args,y=y)
  if(!missing(width)) args <- c(args,width=width)
  if(!missing(height)) args <- c(args,height=height)
  if(!missing(rx)) args <- c(args,rx=rx)
  if(!missing(ry)) args <- c(args,ry=ry)
  if(!missing(class)) args <- c(args,class=class)
  if(!missing(style)) args <- c(args,style=style)
  if(!missing(transform)) args <- c(args,transform=transform)
  rectangle <- do.call(new,args)

  ## eop
  return(rectangle)
}
 
