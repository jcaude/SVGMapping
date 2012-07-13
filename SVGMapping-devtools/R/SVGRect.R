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
#'  @aliases SVGRect-class
setClass("SVGRect",
         representation(roundx="character",
                        roundy="character"),
         contains=c("SVGShape","Rectangle")
         )

#' @name print.SVGRect
#' 
#' @rdname svgmapping.print-methods
#' @exportMethod print.SVGRect
#' @docType methods
setGeneric(name="print.SVGRect", function(x,...) { standardGeneric("print.SVGRect") })

#' Rectangle round off Corners accessors
#' 
#' The methods get or set the round off corners of an SVG Shape.
#' 
#' The \code{roundx(object)} method returns the \emph{X-axis} radius of the ellipse 
#' used to round off the corners of the shape.
#' 
#' @name roundx
#'  
#' @param object the SVG shape instance
#' 
#' @return the length in SVG units  
#' 
#' @rdname svgrect.roundoff-methods
#' @exportMethod roundx
#' @docType methods
setGeneric(name="roundx", function(object) { standardGeneric("roundx") })

#' <title already defined>
#' 
#' 
#' 
#' The \code{roundx(object) <- value} method can be used to set the \emph{X-axis} 
#' radius of the ellipse used to round off the corners of the shape.
#' 
#' @name roundx<- 
#' 
#' @rdname svgrect.roundoff-methods
#' @exportMethod roundx<- 
#' @docType methods
setGeneric(name="roundx<-", function(.Object,value) { standardGeneric("roundx<-") })

#' <title already defined>
#' 
#' 
#' 
#' The \code{roundy(object)} method returns the \emph{Y-axis} radius of the ellipse 
#' used to round off the corners of the shape.
#' 
#' @name roundy
#' 
#' @rdname svgrect.roundoff-methods
#' @exportMethod roundy
#' @docType methods
setGeneric(name="roundy", function(object) { standardGeneric("roundy") })

#' <title already defined>
#' 
#' 
#' 
#' The \code{roundy(object) <- value} method can be used to set the \emph{Y-axis} 
#' radius of the ellipse used to round off the corners of the shape.
#' 
#' @name roundy<- 
#' 
#' @rdname svgrect.roundoff-methods
#' @exportMethod roundy<- 
#' @docType methods
setGeneric(name="roundy<-", function(.Object,value) { standardGeneric("roundy<-") })

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
            roundx(.Object) <- .arg("roundx",character(0))
            roundy(.Object) <- .arg("roundy",character(0))

            ## eop
            return(.Object)
          }
          )

#' @rdname svgmapping.print-methods
#' @aliases print.SVGRect,SVGRect-method
setMethod(f="print.SVGRect", signature="SVGRect",
          definition=function(x,...)
          {
            rect.node <- .xml(x)
            print(rect.node)
          }
          )

#' @rdname svgrect.roundoff-methods
#' @aliases roundx,SVGRect-method
setMethod(f="roundx", signature="SVGRect",
          definition=function(object)
          {
            return(object@roundx)
          }
          )

#' @name roundx<- 
#' @rdname svgrect.roundoff-methods
#' @aliases roundx<-,SVGRect-method
setReplaceMethod(f="roundx", signature="SVGRect",
                 definition=function(.Object,value)
                 {
                   ## check
                   if(!is.atomic(value))
                     stop("'value' must be atomic")
                   if(is.numeric(value)) value <- as.character(value)
                   if(!is.character(value))
                     stop("'value' must be an atomic string")

                   ## assign & eop
                   .Object@roundx <- value
                   return(.Object)
                 }
                 )

#' @rdname svgrect.roundoff-methods
#' @aliases roundy,SVGRect-method
setMethod(f="roundy", signature="SVGRect",
          definition=function(object)
          {
            return(object@roundy)
          }
          )

#' @name roundy<-
#' @rdname svgrect.roundoff-methods
#' @aliases roundy<-,SVGRect-method
setReplaceMethod(f="roundy", signature="SVGRect",
                 definition=function(.Object,value)
                 {
                   ## check
                   if(!is.atomic(value))
                     stop("'value' must be atomic")
                   if(is.numeric(value)) value <- as.character(value)
                   if(!is.character(value))
                     stop("'value' must be an atomic string")

                   ## assign & eop
                   .Object@roundy <- value
                   return(.Object)
                 }
                 )

#' @rdname svgnode.xml-methods
#' @aliases .xml,SVGRect-method
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
            if(length(roundx(object)) > 0) attr <- c(attr, rx=roundx(object))
            if(length(roundy(object)) > 0) attr <- c(attr, ry=roundy(object))
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
#' @param roundx the X-avis corner round-off length
#' @param roundy the Y-axis corner round-off length
#' @param class the CSS class name
#' @param style the CSS style description
#' @param transform the geometric transformation to apply to the coordinates
#' 
#' @return an \code{\link{SVGRect}} instance
#' 
#' @seealso The \code{\link{SVGRect}} class definition; the \code{\link{roundx}} 
#' and \code{\link{roundy}} methods to set the round off corners length.
#' 
#' @export SVGRect.factory
#' 
#' @examples
#' ## Simple rectangle
#' rect <- SVGRect.factory(x="5cm",y="5cm",width="60cm",height="30cm",roundx="2px", roundy="1px")
#' ## Create a rectangle using bounding-box list
#' rect.bbox <- list(x="5px",y="5px",width="650px",height="315px")
#' rect <- SVGRect.factory(bbox=rect.bbox)
#' ## A rectangle with some inline CSS decorations
#' rect <- SVGRect.factory(bbox=rect.bbox,class="mylines",style="stroke-width:15cm")
#' ## A rectangle (0,0) translated of 10 to the right and 20 below, 
#' ## new coordinates are (10,20)
#' rect <- SVGRect.factory(x=0,y=0,width=60,height=30,transform="translate(10,20)")
SVGRect.factory <- function(x,y,width,height,bbox,roundx,roundy,class,style,transform) {

  ## init.
  args <- list("SVGRect")
  if(!missing(x)) args <- c(args,x=x)
  if(!missing(y)) args <- c(args,y=y)
  if(!missing(width)) args <- c(args,width=width)
  if(!missing(height)) args <- c(args,height=height)
  if(!missing(bbox)) args <- c(args,bbox=list(bbox))
  if(!missing(roundx)) args <- c(args,roundx=roundx)
  if(!missing(roundy)) args <- c(args,roundy=roundy)
  if(!missing(class)) args <- c(args,class=class)
  if(!missing(style)) args <- c(args,style=style)
  if(!missing(transform)) args <- c(args,transform=transform)
  rectangle <- do.call(new,args)

  ## eop
  return(rectangle)
}
 
