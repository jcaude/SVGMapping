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

## R A D I A L   G R A D I E N T
## --------------------------------------------------

setGenericVerif <- function(name,y){if(!isGeneric(name)){setGeneric(name,y)}else{}}

#' SVG Radial Gradient class
#' 
#' This class is used to describe a radial gradient, as part of the SVG 1.1 
#' specifications. This class inherited from the Gradient class.
#'  
#'  @seealso \code{\link{Gradient}} and \code{\link{Circle}}.
#'  @exportClass "RadialGradient"
setClass("RadialGradient",
         representation(fx="character",
                        fy="character"),
         contains=c("Gradient","Circle")
         )

#' Focal point definition of the Radial Gradient
#' 
#' These methods are accessors to the focal point definition of a radial 
#' gradient object.
#' 
#' The method \code{fx(object)} return the X-axis of the focal point. 
#' The gradient will be drawn such that the 0% gradient stop is mapped to 
#' \code{(fx, fy)}.
#' 
#' @name fx
#' 
#' @param object a RadialGradient object
#' 
#' @return \code{fx(object)} return the coordinates in untis compliant with
#'  the SVG 1.1 specifications. \code{fx(object) <- value} return the invisible
#'  object.
#' 
#' @rdname radialgradient.bbox-methods
#' @exportMethod fx
#' @docType methods
NULL
setGenericVerif(name="fx", function(object) { standardGeneric("fx") })

#' <title already defined>
#' 
#' 
#' 
#' The \code{fx{object} <- value} method sets the X-axis coordinates of the 
#' focal point.
#' 
#' @name fx<-
#' 
#'  @rdname radialgradient.bbox-methods
#'  @exportMethod fx<-
#'  @docType methods  
NULL
setGenericVerif(name="fx<-", function(.Object,value) { standardGeneric("fx<-") })

#' Focal point definition of the Radial Gradient
#' 
#' 
#' 
#' If attribute \code{fy} is not specified, \code{fy} will coincide with the 
#' presentational value of \code{\link{cy}} for the element whether the value 
#' for \code{\link{cy}} was inherited or not.
#' 
#' @name fy
#' 
#' @param object a RadialGradient object
#' 
#' @return \code{fy(object)} return the coordinates in untis compliant with
#'  the SVG 1.1 specifications. \code{fy(object) <- value} return the invisible
#'  object.
#' 
#' @rdname radialgradient.bbox-methods
#' @exportMethod fy
#' @docType methods
NULL
setGenericVerif(name="fy", function(object) { standardGeneric("fy") })

#' <title already defined>
#' 
#' 
#' 
#' The \code{fy{object} <- value} method sets the Y-axis coordinates of the 
#' focal point.
#' 
#' @name fy<-
#' 
#'  @rdname radialgradient.bbox-methods
#'  @exportMethod fy<-
#'  @docType methods  
NULL
setGenericVerif(name="fy<-", function(.Object,value) { standardGeneric("fy<-") })

setGenericVerif(name=".xml", function(object) { standardGeneric(".xml") })

setMethod(f="initialize", signature="RadialGradient",
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

            ## super (Gradient)
            .Object <- callNextMethod(.Object,...)

            ##  super (Circle)
            .Object <- .callMethod("initialize","Circle",.Object,...)
            
            ## get args
            args = list(...)
            args.names = names(args)
            if(is.null(args.names)) args.names <- list()
            
            ## default init.
            fx(.Object) <- .arg("fx",character(0))
            fy(.Object) <- .arg("fy",character(0))

            ## eop
            return(.Object)
          }
          )

#' @name show,RadialGradient-method
#' @rdname gradient.show-methods
setMethod(f="show", signature="RadialGradient",
          definition=function(object)
          {
            ## forge an svg, apply linear gradient and just show it!
            svg <- SVG.factory(system.file("extdata/radial-gradient-sample.svg",
                                           package="SVGMapping")
                               )
            definitions(svg) <- object
            svg["id::circle-gradient","style::fill"] <- URL(object)
            show(svg)
            write.SVG(svg,file="~/Sources/R/Snippets/debug.svg")
          }
          )

#' @name .xml,RadialGradient-method
#' @rdname svgnode.core-methods
setMethod(f=".xml", signature="RadialGradient",
          definition=function(object)
          {
            ## init.
            gradient <- newXMLNode("radialGradient")

            ## core.attributes (Gradient)
            attr <- callNextMethod(object)
            
            ## circle attributes (Circle)
            attr <- c(attr,.callMethod(".xml","Circle",object))

            ## attributes
            if(length(fx(object) != 0)) attr <- c(attr, fx=fx(object))
            if(length(fy(object) != 0)) attr <- c(attr, fy=fy(object))
            xmlAttrs(gradient) <- attr

            ## add stop items
            svg.stops <- sapply(object@stops, .xml)
            addChildren(gradient,kids=svg.stops)

            ## eop
            return(gradient)
          }
          )

## F A C T O R Y
##----------------------------------------
#' Radial Gradient Factory
#' 
#' This function returns a RadialGradient instance given its coordinates and
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
#' @name RadialGradient.factory
#' 
#' @param ... all unnamed arguments are considered as gradient stops
#' @param stops a vector of color stops as \code{\link{GradientStop}} instances
#' @param bbox a list of \code{\link{Circle}} coordinates \code{(cx,cy,r)}
#' @param fx the \emph{X-axis} focal point definition
#' @param fy the \emph{Y-axis} focal point definition
#' @param spread.method indicates what happens if the gradient starts or ends 
#' inside the bounds of the target \emph{shape}.
#' 
#' @return an \code{\link{RadialGradient}} instance
#' 
#' @seealso The \code{\link{RadialGradient}} class definition and 
#' the \code{\link{GradientStop}} class used to defined each color stops. 
#' 
#' @export RadialGradient.factory
#' 
#' @examples
#' ## create and empy SVG document
#' ## svg <- SVG.factory(dim="a4")
#' ## First, we define two color stops: blue and red (opacity 50%)
#' blue <- GradientStop.factory(color="blue")
#' red.50 <- GradientStop.factory(offset="1",color="red",opacity=0.5) 
#' ## Radial blue-red Linear Gradient with a radius of 40%
#' gradient <- LinearGradient.factory(stops=c(blue,red.50), bbox=list(cx="0",cy="0",r="40%"))
#' ## add the gradient to the document definition
#' ## definition(svg) <- gradient
#' ## retrieve the gradient url
#' gradient.url <- URL(gradient)
RadialGradient.factory <- function(...,stops,bbox,fx,fy,spread.method) {

  ## init. radial gradient
  args <- list("RadialGradient")
  if(!missing(bbox)) args <- c(args, bbox=list(bbox))
  if(!missing(spread.method)) args <- c(args, spread.method=spread.method)
  if(!missing(fx)) args <- c(args, fx=list(fx))
  if(!missing(fy)) args <- c(args, fy=list(fy))
  gradient = do.call(new, args)

  ## add stops
  stops(gradient) <- if(!missing(stops)) stops else list(...)
  
  ## eop
  return(gradient)
}
