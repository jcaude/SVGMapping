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

## S V G   U N I T  
## --------------------------------------------------
setGenericVerif <- function(name,y){if(!isGeneric(name)){setGeneric(name,y)}else{}}

#' SVGUnit SVG Unit Class Descritpion
#' 
#' This class is used as a container for all SVG mesurement values
#' 
#' @export "SVGUnit"
#' @aliases SVGUnit-class
setClass("SVGUnit",
         representation(u.value="numeric",
                        u.unit="character",
                        u.dpi="numeric")
         )

#' Device Resolution Accessors
#' 
#' These methods are accessors to the  device resolution of a \emph{SVGUnit} 
#' object. This resolution is given in the standard pixels by inches unit.
#' 
#' The \code{dpi(object)} method returns the device resolution of the current
#' unit object.
#' 
#' @name dpi
#'   
#' @param object is an SVGUnit object
#'   
#' @return a numeric dpi value 
#'   
#' @rdname svgunit.dpi-methods
#' @exportMethod dpi
#' @docType methods
setGeneric(name="dpi", function(object) { standardGeneric("dpi") })

#' <title already defined>
#' 
#' 
#' 
#' The \code{dpi(object) <- value} method can be used to set the device
#' resolution of a \emph{SVGUnit} object.
#' 
#' @name dpi<-
#'   
#' @rdname svgunit.dpi-methods
#' @exportMethod dpi<-
#' @docType methods
setGeneric(name="dpi<-", function(.Object,value) { standardGeneric("dpi<-") })

#' Show method for SVGUnit objects
#' 
#' The \code{show()} method called with an SVGUnit object as an argument allows
#' display the unit value in the current unit as a string.
#' 
#' @name show
#' 
#' @param object is a \code{\link{SVGUnit}} instance.
#' 
#' @rdname svgunit.show-methods
#' @docType methods
NULL


setMethod(f="initialize", signature="SVGUnit",
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
            
            ## detault init.
            dpi(.Object) <- .arg("dpi",72)
            
            ## eop
            return(.Object)
          }
)

#' @rdname svgunit.dpi-methods
#' @aliases dpi,SVGUnit-method
setMethod(f="dpi",signature="SVGUnit",
          definition=function(object) 
          {
            return(object@u.dpi)
          }
          )

#' @name dpi<-
#' @rdname svgunit.dpi-methods
#' @aliases dpi<-,SVGUnit-method
setReplaceMethod(f="dpi", signature="SVGUnit",
                 definition=function(.Object,value)
                 {
                   ## check
                   if(!is.numeric(value))
                     stop("DPI 'value' must be a numeric")

                   ## init.
                   .Object@u.dpi <- value
                   return(.Object)
                 }
                 )

#' @rdname svgunit.show-methods
#' @aliases show,SVGUnit-method
setMethod(f="show",signature="SVGUnit",
          definition=function(object)
          {
            return(paste(object@u.value,object@u.unit,sep=""))            
          })
