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

## G R A D I E N T S
## --------------------------------------------------

setGenericVerif <- function(name,y){if(!isGeneric(name)){setGeneric(name,y)}else{}}

#' Gradient root class (VIRTUAL)
#' 
#' This class is the root class for all gradient methods.
#' This class can't be directly instantiate and must be derived. 
#' 
#' @seealso \code{\link{SVGNode}} the parent class
#' @exportClass "Gradient"
#' @aliases Gradient-class
setClass("Gradient",
         representation(svg_units="character",
                        spread.method="character",
                        xlink.href="character",
                        stops="list",
                        "VIRTUAL"
                        ),
         contains="SVGNode"
         )

#' Gradient Core Attributes Accessors
#'
#' These methods are accessors to the \emph{core} attributes of an SVG Gradient
#' object. These attributes are described in the SVG 1.1 specifications document.
#'
#' The \code{svgUnits(object)} method returns the SVG \emph{units} attribute of 
#' an SVG Gradient.
#' 
#' @name svgUnits
#' 
#' @param object is an SVG Gradient object
#'
#' @return a core attribute as a string
#' 
#' @rdname gradient.core-methods
#' @exportMethod svgUnits
#' @docType methods
setGeneric(name="svgUnits", function(object) { standardGeneric("svgUnits") })

#' <title already defined>
#' 
#' 
#' 
#' The \code{svgUnits(object) <- value} method sets the SVG \emph{units} attribute of 
#' an SVG Gradient. The \emph{units} value must be etheir \code{userSpaceOnUse} or 
#' \code{objectBoundingBox} (see SVG specifications for details).
#' 
#' @name svgUnits<-
#' 
#' @rdname gradient.core-methods
#' @exportMethod svgUnits<-
#' @docType methods
setGeneric(name="svgUnits<-", function(.Object, value) { standardGeneric("svgUnits<-") })

#' <title already defined>
#' 
#' 
#' 
#' The \code{spreadMethod(object)} method returns the SVG \emph{spreadMethod} attribute of 
#' an SVG Gradient.
#' 
#' @name spreadMethod
#' 
#' @rdname gradient.core-methods
#' @exportMethod spreadMethod
#' @docType methods
setGeneric(name="spreadMethod", function(object) { standardGeneric("spreadMethod") })

#' <title already defined>
#' 
#' 
#' 
#' The \code{spreadMethod(object) <- value} method sets the SVG \emph{spreadMethod} attribute of 
#' an SVG Gradient. The \emph{spreadMethod} value must be etheir \code{pad}, \code{reflect} or 
#' \code{repeat} (see SVG specifications for details).
#' 
#' @name spreadMethod<-
#' 
#' @rdname gradient.core-methods
#' @exportMethod spreadMethod<-
#' @docType methods
setGeneric(name="spreadMethod<-", function(.Object, value) { standardGeneric("spreadMethod<-") })

#' <title already defined>
#' 
#' 
#' 
#' The \code{xlinkHref(object)} method returns the SVG \emph{xlink:href} attribute of 
#' an SVG Gradient.
#' 
#' @name xlinkHref
#' 
#' @rdname gradient.core-methods
#' @exportMethod xlinkHref
#' @docType methods
setGeneric(name="xlinkHref", function(object) { standardGeneric("xlinkHref") })

#' <title already defined>
#' 
#' 
#' 
#' The \code{xlinkHref(object) <- value} method sets the SVG \emph{xlink:href} attribute of 
#' an SVG Gradient.
#' 
#' @name xlinkHref<-
#' 
#' @rdname gradient.core-methods
#' @exportMethod xlinkHref<-
#' @docType methods
setGeneric(name="xlinkHref<-", function(.Object, value) { standardGeneric("xlinkHref<-") })

#' Gradient Stops
#' 
#' A Gradient is made of several stops. Each stop is associated with a relative position, a color 
#' and an opacity level. This list is used to render a shape.
#' 
#' The \code{stops(object)} method returns the list of gradient stops.
#' 
#' @name stops
#' 
#' @param object is an SVG Gradient object
#' 
#' @return the list of gradient stops
#' 
## @seealso GradientStop
#' @rdname gradient.stops-methods
#' @exportMethod stops
#' @docType methods
setGeneric(name="stops", function(object) { standardGeneric("stops") })

#' <title already defined>
#' 
#' 
#' 
#' The \code{stops(object) <- value} method set the list of gradient stops of the gradient.
#' 
#' @name stops
#' 
#' @rdname gradient.stops-methods
#' @exportMethod stops<-
#' @docType methods
setGeneric(name="stops<-", function(.Object, value) { standardGeneric("stops<-") })

#' Gradient document URL
#' 
#' When a gradient object is added to the definition element of an SVG document 
#' it gets an XML ID. The \code{URL} method uses this identifier to forge a 
#' unique URL. This URL is used to link the gradient with a given shape, 
#' mask operation ...
#' 
#' @name URL
#' 
#' @param object is an SVG Gradient object
#' 
#' @return the gradient URL
#' 
#' @rdname gradient.url-methods
#' @exportMethod URL
#' @docType methods
setGeneric(name="URL", function(object) { standardGeneric("URL") })

#' Show method for Gradient
#' 
#' The \code{show()} method called with a gradient object as an argument allows
#' to preview the gradient rendering results. It opens
#  the default system browser with a predifined sketch that contains either a
#' square shape (for \code{\link{LinearGradient}} objects) or a circle 
#' (for \code{\link{RadialGradient}} objects). This shape is colored using the
#' gradient definition.
#' 
#' The gradient is transformed to fit into the shape.
#' 
#' @name show
#' 
#' @param object is either a \code{\link{LinearGradient}} or a 
#' \code{\link{RadialGradient}} instance.
#' 
#' @rdname gradient.show-methods
#' @docType methods
NULL

setMethod(f="initialize", signature="Gradient",
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

            ## super
            .Object <- callNextMethod(.Object,...)

            ## get args
            args = list(...)
            args.names = names(args)
            if(is.null(args.names)) args.names <- list()
            
            ## default init.
            svgUnits(.Object) <- .arg("units","objectBoundingBox")
            spreadMethod(.Object) <- .arg("spread.method","pad")
            xlinkHref(.Object) <- .arg("xlink.href",character(0))
            stops(.Object) <- .arg("stops",list())

            ## eop
            return(.Object)
          }
          )

#' @rdname gradient.core-methods
#' @aliases svgUnits,Gradient-method
setMethod(f="svgUnits", signature="Gradient",
          definition=function(object)
          {
            return(object@svg_units)
          }
          )

#' @name svgUnits<- 
#' @rdname gradient.core-methods
#' @aliases svgUnits<-,Gradient-method
setReplaceMethod(f="svgUnits", signature="Gradient",
                 definition=function(.Object, value)
                 {
                   ## check
                   if(!is.character(value))
                     stop("'value' must be a character string")
                   if(!(value %in% list("userSpaceOnUse", "objectBoundingBox")))
                     stop("'value' must be either 'userSpaceOnUse' or 'objectBoundingBox'")

                   ## assign & eop
                   .Object@svg_units <- value
                   return(.Object)
                 }
                 )

#' @rdname gradient.core-methods
#' @aliases spreadMethod,Gradient-method
setMethod(f="spreadMethod", signature="Gradient",
          definition=function(object)
          {
            return(object@spread.method)
          }
          )

#' @name spreadMethod<- 
#' @rdname gradient.core-methods
#' @aliases spreadMethod<-,Gradient-method
setReplaceMethod(f="spreadMethod", signature="Gradient",
                 definition=function(.Object, value)
                 {
                   ## check
                   if(!is.character(value))
                     stop("'value' must be a character string")
                   value <- tolower(value)
                   if(!(value %in% list("pad", "reflect", "repeat")))
                     stop("'value' must be either: pad, reflect or repeat")

                   ## assign & eop
                   .Object@spread.method <- value
                   return(.Object)
                 }
                 )

#' @rdname gradient.core-methods
#' @aliases xlinkHref,Gradient-method
setMethod(f="xlinkHref", signature="Gradient",
          definition=function(object)
          {
            return(object@xlink.href)
          }
          )

#' @name xlinkHref<- 
#' @rdname gradient.core-methods
#' @aliases xlinkHref<-,Gradient-method
setReplaceMethod(f="xlinkHref", signature="Gradient",
                 definition=function(.Object, value)
                 {
                   ## check
                   if(!is.character(value))
                     stop("'value' must be a character string")

                   ## assign & eop
                   .Object@xlink.href <- value
                   return(.Object)
                 }
                 )

#' @rdname gradient.stops-methods
#' @aliases stops,Gradient-method
setMethod(f="stops", signature="Gradient",
          definition=function(object)
          {
            return(object@stops)
          }
          )

#' @name stops<- 
#' @rdname gradient.stops-methods
#' @aliases stops<-,Gradient-method
setReplaceMethod(f="stops", signature="Gradient",
                 definition=function(.Object, value)
                 {
                   ## case 1 - list()
                   if(is.list(value)) {
                     if(length(value) > 0) {
                       check <- sapply(value,function(x) {return(is.object(x) && is(x,"GradientStop"))})
                       if(!all(check)) 
                          stop("'value' must be a list of GradientStop")
                       .Object@stops <- value
                     }
                   }

                   ## case 2 - object
                   else if(is.object(value) && is(value, "GradientStop")) 
                     .Object@stops <- c(.Object@stops, value)

                   ## error
                   else 
                     stop("'value' must be either a list of GradientStop or a GradientStop")
                   
                   ## eop
                   return(.Object)
                 }
                 )

#' @rdname svgnode.xml-methods
#' @aliases .xml,Gradient-method
setMethod(f=".xml", signature="Gradient",
          definition=function(object)
          {
            ## super
            attr <- callNextMethod(object)
            
            ## return a list of core attributes
            if(object@svg_units != "objectBoundingBox")
              attr <- c(attr, gradientUnits=object@svg_units)
            if(object@spread.method != "pad")
              attr <- c(attr, spreadMethod=object@spread.method)
            if(length(object@xlink.href) > 0)
              attr <- c(attr, "xlink:href"=object@xlink.href)
            return(attr)
          }
          )

#' @rdname gradient.url-methods
#' @aliases URL,Gradient-method
setMethod(f="URL", signature="Gradient",
          definition=function(object)
          {
            if(length(object@id) == 0)
              return(object@id)
            else
              return(paste("url(#",object@id,")",sep=""))
          }
          )
