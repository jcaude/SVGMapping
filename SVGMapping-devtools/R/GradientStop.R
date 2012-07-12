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

## G R A D I E N T S   S T O P S
## --------------------------------------------------

setGenericVerif <- function(name,y){if(!isGeneric(name)){setGeneric(name,y)}else{}}

#' Gradient Stops Definition
#' 
#' This class is used to describe a \emph{stop} used to compose gradient. A \emph{stop} is
#' definied by an offset, a color and an opacity level.
#' 
#' @seealso Gradient
#' @exportClass "GradientStop"
#' @aliases GradientStop-class
setClass("GradientStop",
         representation(offset="character",
                        color="character",
                        opacity="numeric"
                        )
         )

#' Gradient Stop Accessors
#' 
#' These methods allow to get/set attributes of a Gradient Stop.
#' 
#' The \code{offset(object)} method returns the offset value of 
#' a Gradient stop.
#' 
#' @name offset
#' 
#' @param object is a Gradient stop instance
#' 
#' @return a string (\code{offset} and \code{color}) or a numeric value (\code{opacity})
#' 
#' @rdname gradientstop.core-methods
#' @exportMethod offset
#' @docType methods
NULL

#' <title already defined>
#' 
#' 
#' 
#' The \code{offset(object) <- value} method set the offset value of a
#' Gradient stop. This value must be a compatible offset as defined in the 
#' SVG 1.1 specifications.
#' 
#' @name offset<-
#' 
#' @rdname gradientstop.core-methods
#' @exportMethod offset<-
#' @docType methods
setGeneric("offset<-", function(.Object,value) {standardGeneric("offset<-")})

#' <title already defined>
#' 
#' 
#' 
#' The \code{color(object)} method returns the color value of 
#' a Gradient stop.
#' 
#' @name color
#' 
#' @rdname gradientstop.core-methods
#' @exportMethod color
#' @docType methods
setGeneric("color", function(object) { standardGeneric("color")})

#' <title already defined>
#' 
#' 
#' 
#' The \code{color(object) <- value} method set the color value of a
#' Gradient stop. This value must be a compatible offset as defined in the 
#' SVG 1.1 specifications.
#' 
#' @name color<-
#' 
#' @rdname gradientstop.core-methods
#' @exportMethod color<-
#' @docType methods
setGeneric("color<-", function(.Object,value) {standardGeneric("color<-")})

#' <title already defined>
#' 
#' 
#' 
#' The \code{opacity(object)} method returns the opacity leve of 
#' a Gradient stop.
#' 
#' @name opacity,GradientStop-method
#' 
#' @rdname gradientstop.core-methods
#' @exportMethod opacity
#' @docType methods
NULL

#' <title already defined>
#' 
#' 
#' 
#' The \code{opacity(object) <- value} method set the opacity level of a
#' Gradient stop. This value must be in the range [0,1].
#' 
#' @name opacity<-,GradientStop-method
#' 
#' @rdname gradientstop.core-methods
#' @exportMethod opacity<-
#' @docType methods
NULL

setMethod(f="initialize", signature="GradientStop",
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
            if(!is.null(args.names)) arg.names <- list()
            
            ## default init.
            offset(.Object) <- .arg("offset","0")
            color(.Object) <- .arg("color","white")
            opacity(.Object) <- .arg("opacity",1.0)
            
            ## eop
            return(.Object)
          }
          )

#' @rdname gradientstop.core-methods
#' @aliases offset,GradientStop-method
setMethod(f="offset", signature="GradientStop",
          definition=function(object) {
            return(object@offset)  
          }
          )

#' @name offset<-
#' @rdname gradientstop.core-methods
#' @aliases offset<-,GradientStop-method
setReplaceMethod(f="offset", signature="GradientStop",
                 definition=function(.Object,value) 
                 {                   
                   ## check
                   if(!is.atomic(value))
                     stop("'value' must be atomic")
                   if(is.numeric(value)) value <- as.character(value)
                   if(!is.character(value))
                     stop("'value' must be a character string")
                   
                   ## assign & eop
                   .Object@offset <- value
                   return(.Object)
                 }
                 )

#' @rdname gradientstop.core-methods
#' @aliases color,GradientStop-method
setMethod(f="color", signature="GradientStop",
          definition=function(object) {
            return(object@color)  
          }
          )

#' @name color<-
#' @rdname gradientstop.core-methods
#' @aliases color<-,GradientStop-method
setReplaceMethod(f="color", signature="GradientStop",
                 definition=function(.Object,value) 
                 {                   
                   ## check
                   if(!is.atomic(value))
                     stop("'value' must be atomic")
                   if(!is.character(value))
                     stop("'value' must be a character string")
                   
                   ## assign & eop
                   .Object@color <- value
                   return(.Object)
                 }
                 )

#' @rdname gradientstop.core-methods
#' @aliases opacity,GradientStop-method
setMethod(f="opacity", signature="GradientStop",
          definition=function(object) {
            return(object@opacity)  
          }
          )

#' @name opacity<-
#' @rdname gradientstop.core-methods
#' @aliases opacity<-,GradientStop-method
setReplaceMethod(f="opacity", signature="GradientStop",
                 definition=function(.Object,value) 
                 {                   
                   ## check
                   if(!is.atomic(value))
                     stop("'value' must be atomic")
                   if(!is.numeric(value))
                     stop("'value' must be numeric")
                   
                   ## assign & eop
                   .Object@opacity <- value
                   return(.Object)
                 }
                 )

#' @rdname svgnode.xml-methods
#' @aliases .xml,GradientStop-method
setMethod(f=".xml", signature="GradientStop",
          definition=function(object)
          {
            ## SVG rendering
            svg <- newXMLNode("stop",
                              attrs=list(offset=object@offset,
                                "stop-color"=object@color,
                                "stop-opacity"=object@opacity)
                              )
            return(svg)
          }
          )

## F A C T O R Y
##----------------------------------------

#' GradientStop Factory
#' 
#' This function build a Gradient Stop instance given an offset, a color 
#' and an opacity level
#' 
#' The default values are:
#' 
#' \itemize{
#'  \item \code{offset = '0'}
#'  \item \code{color = 'white'}
#'  \item \code{opacity = 1.0}
#' }
#' 
#' @name GradientStop.factory
#' 
#' @param offset is string defining the \emph{stop} offset value (see SVG 1.1 specification) 
#' @param color is a string defining the color of the \emph{stop} (see SVG 1.1 specification)
#' @param opacity is a numeric value in the range [0,1] defining the opacity level of the \emph{stop}
#' 
#' @return a \code{\link{GradientStop}} instance
#' 
#' @seealso The \code{\link{GradientStop}} class definition
#' @export GradientStop.factory
#' 
#' @examples
#' mystop <- GradientStop.factory(offset="0.5",color="#AAFFBB", opacity=0.90)
GradientStop.factory <- function(offset="0",color="white",opacity=1.0) {

  ## init.
  if(is.numeric(offset)) offset <- as.character(offset)

  ## create offset.
  return(new("GradientStop", offset=offset, color=color, opacity=opacity))
}


