## Copyright (c) 2011, CEA DSV/iBiTecS
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

## L I N E A R   G R A D I E N T
## --------------------------------------------------

setGenericVerif <- function(name,y){if(!isGeneric(name)){setGeneric(name,y)}else{}}

#' SVG Linear Gradient class
#' 
#' This class is used to describe a linear gradient, as part of the SVG 1.1 
#' specifications. This class inherited from the Gradient class.
#'  
#'  @seealso \code{\link{Gradient}} and \code{\link{Vector}}.
#'  @exportClass "LinearGradient"
#'  @aliases LinearGradient-class
setClass("LinearGradient",
         representation(),
         contains=c("Gradient","Vector")
         )

setMethod(f="initialize", signature="LinearGradient",
          definition=function(.Object,...)
          {
            ## super (Gradient)
            .Object <- callNextMethod(.Object,...)

            ## super (Vector)
            .Object <- .callMethod("initialize","Vector",.Object,...)

            ## eop
            return(.Object)
          }
          )

#' @name show,LinearGradient-method
#' @rdname gradient.show-methods
#' @aliases show,LinearGradient-method
setMethod(f="show", signature="LinearGradient",
          definition=function(object)
          {
            ## forge an svg, apply linear gradient and just show it!
            svg <- SVG.factory(system.file("extdata/linear-gradient-sample.svg",
                                           package="SVGMapping")
                               )
            definitions(svg) <- object
            svg["id::rect-gradient","style::fill"] <- URL(object)
            show(svg)
          }
          )

#' @rdname svgnode.core-methods
#' @aliases .xml,LinearGradient-method
setMethod(f=".xml", signature="LinearGradient",
          definition=function(object)
          {
            ## init.
            gradient <- newXMLNode("linearGradient")
            
            ## core.attributes (Gradient)
            attr <- callNextMethod(object)
            
            ## vector attributes (Vector)
            attr <- c(attr, .callMethod(".xml","Vector",object))

            ## add attributes & stop elements
            xmlAttrs(gradient) <- attr
            svg.stops <- sapply(object@stops, .xml)
            addChildren(gradient,kids=svg.stops)

            ## eop
            return(gradient)
          }
          )

## F A C T O R Y
##----------------------------------------
#' Linear Gradient Factory
#' 
#' This function returns a LinearGradient instance given its coordinates and
#' core attributes.
#' 
#' The gradient can be previewed with the system browser using the 
#' \code{\link{show}} method.
#' 
#' Once defined a gradient can be inserted into the document definition using
#' the \code{definition} method of the \code{SVG} class. After insertion an
#' identifier is assigned to gradient and can be retrieve using the 
#' \code{\link{id}} or \code{\link{URL}} to get an already forge document URL.
#' 
#' @name LinearGradient.factory
#' 
#' @param ... all unnamed arguments are considered as gradient stops
#' @param stops a vector of color stops as \code{\link{GradientStop}} instances
#' @param bbox a list of coordinates \code{(x1,y1,x2,y2)}
#' @param spread.method indicates what happens if the gradient starts or ends 
#' inside the bounds of the target \emph{shape}.
#' 
#' @return an \code{\link{LinearGradient}} instance
#' 
#' @seealso The \code{\link{LinearGradient}} class definition and 
#' the \code{\link{GradientStop}} class used to defined each color stops. 
#' 
#' @export LinearGradient.factory
#' 
#' @examples
#' ## create and empy SVG document
#' ## svg <- SVG.factory(dim="a4")
#' ## First, we define two color stops: blue and red (opacity 50%)
#' blue <- GradientStop.factory(color="blue")
#' red.50 <- GradientStop.factory(offset="1",color="red",opacity=0.5) 
#' ## Horizontal blue-red Linear Gradient
#' gradient <- LinearGradient.factory(stops=c(blue,red.50), bbox=list(x1="0",y1="0",x2="1",y2="0"))
#' ## add the gradient to the document definition
#' ## definition(svg) <- gradient
#' ## retrieve the gradient url
#' gradient.url <- URL(gradient)
LinearGradient.factory <- function(...,stops,bbox,spread.method) {
  
  ## init. linear gradient
  args <- list("LinearGradient")
  if(!missing(bbox)) args <- c(args, coords=list(bbox))
  if(!missing(spread.method)) args <- c(args, spread.method=spread.method)
  linear.gradient = do.call(new, args)

  ## add stops
  stops(linear.gradient) <- if(!missing(stops)) stops else list(...)
  
  ## eop
  return(linear.gradient)
}
