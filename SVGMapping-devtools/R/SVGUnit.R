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
#' @name SVGUnit
#' @exportClass "SVGUnit"
#' @aliases SVGUnit-class
setClass("SVGUnit",
         representation(u.value="numeric",
                        u.unit="character",
                        u.dpi="numeric",
                        "VIRTUAL")
         )

#' Unit Value Accessors
#' 
#' These methods are accessors to the unit value and units sytem of a 
#' \emph{SVGUnit} object. Valid units system are defined in the SVG 1.1
#' specification.
#' 
#' The \code{uValue(object)} method returns the unit value in the current unit 
#' system.
#' 
#' @name uValue
#'   
#' @param object is an SVGUnit object
#'   
#' @return \code{uValue} and \code{uUser} return a numeric unit value
#'   
#' @rdname svgunit.unit-methods
#' @exportMethod uValue
#' @docType methods
setGeneric(name="uValue", function(object) { standardGeneric("uValue") })

#' <title already defined>
#' 
#' 
#' 
#' The \code{uValue(object) <- value} method can be used to set the unit value
#' of a \emph{SVGUnit} object
#' 
#' @name uValue<-
#' 
#' @rdname svgunit.unit-methods
#' @exportMethod uValue<-
#' @docType methods
setGeneric(name="uValue<-", function(.Object,value) { standardGeneric("uValue<-") })

#' <title already defined>
#'
#'
#' 
#' The \code{uUnits(object)} method returns the unit system of the current unit
#' object.
#' 
#' @name uUnits
#' 
#' @return \code{uUnits} returns a character unit system abreviation
#' 
#' @rdname svgunit.unit-methods
#' @exportMethod uUnits
#' @docType methods
setGeneric(name="uUnits", function(object) { standardGeneric("uUnits") })

#' <title already defined>
#' 
#' 
#' 
#' The \code{uUser(object)} method returns the unit value in the user system
#' after applying the required units transformation.
#' 
#' @name uUser
#'   
#' @rdname svgunit.unit-methods
#' @exportMethod uUser
#' @docType methods
setGeneric(name="uUser", function(object) { standardGeneric("uUser") })

#' Unit Device Resolution Accessors
#' 
#' These methods are accessors to the device resolution of a \emph{SVGUnit} 
#' object. This resolution is given in the standard pixels by inches unit.
#' 
#' The default DPI value is 90.
#' 
#' The \code{uDpi(object)} method returns the device resolution of the current
#' unit object.
#' 
#' @name uDpi
#'   
#' @param object is an SVGUnit object
#'   
#' @return a numeric dpi value 
#'   
#' @rdname svgunit.dpi-methods
#' @exportMethod uDpi
#' @docType methods
setGeneric(name="uDpi", function(object) { standardGeneric("uDpi") })

#' <title already defined>
#' 
#' 
#' 
#' The \code{uDpi(object) <- value} method can be used to set the device
#' resolution of a \emph{SVGUnit} object.
#' 
#' @name uDpi<-
#'   
#' @rdname svgunit.dpi-methods
#' @exportMethod uDpi<-
#' @docType methods
setGeneric(name="uDpi<-", function(.Object,value) { standardGeneric("uDpi<-") })

#' Show method for SVGUnit objects
#' 
#' Various methods to show or convert SVGUnit objects
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

#' <title already defined>
#' 
#' 
#' 
#' The \code{as.character()} method converts an SVGUnit object into a string
#' 
#' @name as.character
#' @rdname svgunit.show-methods
#' @docType methods
NULL

#' Print an SVGUnit object to the terminal
#' 
#' This method attempts to print SVGUnit object to the terminal as text
#' 
#' @name print.SVGUnit
#' 
#' @param x the SVGUnit object to print
#' 
#' @return the SVGUnit object invisibly
#' 
#' @rdname svgunit.print-methods
#' @exportMethod print.SVGUnit
#' @docType methods
setGeneric(name="print.SVGUnit", function(x,...) { standardGeneric("print.SVGUnit") })

#' Group Generic methods for SVGUnit objects
#' 
#' Some of the S4 group generic methods are implemented for SVGUnit object. For
#' more details see \link{S4groupGeneric} documentation page.
#' 
#' @name Compare
#' 
#' @rdname svgunit.groupGeneric-methods
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
            
            ## check: empty args
            if(length(args) == 0) {
              args <- list(value=0,unit="")
              args.names <- c("value","unit")
            }
            
            ## check: missing args names
            if(!"value" %in% args.names) {
              args.names[1] <- "value"
              names(args) <- args.names
            }
            if((!"unit" %in% args.names) && (length(args) > 1)) {
              args.names[2] <- "unit"
              names(args) <- args.names
            }
            
            ## detault init.
            uDpi(.Object) <- .arg("dpi",90)
            .Object@u.unit <- .arg("unit","")
            uValue(.Object) <- .arg("value",0)
            
            ## eop
            return(.Object)
          }
)

#' @rdname svgunit.unit-methods
#' @aliases uValue,SVGUnit-method
setMethod(f="uValue",signature="SVGUnit",
          definition=function(object) 
          {
            return(object@u.value)
          }
)

#' @name uValue<-
#' @rdname svgunit.unit-methods
#' @aliases uValue<-,SVGUnit-method
setReplaceMethod(f="uValue", signature="SVGUnit",
                 definition=function(.Object,value)
                 {
                   ## check
                   if(!is.numeric(value))
                     stop("unit 'value' must be a numeric")
                   if(is.na(value))
                     stop("unit 'value' can't be NA")
                   
                   ## init.
                   .Object@u.value <- value
                   return(.Object)
                 }
)

#' @rdname svgunit.unit-methods
#' @aliases uUnits,SVGUnit-method
setMethod(f="uUnits",signature="SVGUnit",
          definition=function(object) 
          {
            return(object@u.unit)
          }
)

#' @rdname svgunit.unit-methods
#' @aliases uUser,SVGUnit-method
setMethod(f="uUser",signature="SVGUnit",
          definition=function(object) 
          {
            ## init.
            unit <- paste("U_",uUnits(object),sep="")
            value <- uValue(object)
            dpi <- uDpi(object)
           
            ## check: 0
            if(value == 0) return(value)
            
            ## conversion
            user.unit <- switch(unit,
                                U_mm = dpi * value / 25.4,
                                U_px = value,
                                U_pt = dpi * value / 72,
                                U_pc = dpi * value / 6,
                                U_cm = dpi * value / 2.54,
                                U_in = dpi * value,
                                U_ = value,
                                default= NA)
            
            ## eop
            return(user.unit)
          }
)

#' @rdname svgunit.dpi-methods
#' @aliases uDpi,SVGUnit-method
setMethod(f="uDpi",signature="SVGUnit",
          definition=function(object) 
          {
            return(object@u.dpi)
          }
          )

#' @name uDpi<-
#' @rdname svgunit.dpi-methods
#' @aliases uDpi<-,SVGUnit-method
setReplaceMethod(f="uDpi", signature="SVGUnit",
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
            cat(as.character(object),"\n")
          })

#' @rdname svgunit.show-methods
#' @aliases print.SVGUnit,SVGUnit-method
setMethod(f="print.SVGUnit", signature="SVGUnit",
          definition=function(x,...)
          {
            print(as.character(x))
            return(invisible(x))
          }
)

#' @rdname svgunit.show-methods
#' @aliases as.character,SVGUnit-method
setMethod(f="as.character", signature="SVGUnit",
          definition=function(x,...)
          {
              return(paste(uValue(x),uUnits(x),sep=""))
          }
)

#' @name Compare
#' @rdname svgunit.groupGeneric-methods
#' @aliases Compare,SVGUnit-method
setMethod("Compare", signature="SVGUnit", 
          definition=function(e1, e2) 
          {
            if( (uUnits(e1) == uUnits(e2)) 
                || (uValue(e1) == 0)
                || (uValue(e2) == 0) )
              v = callGeneric(uValue(e1), uValue(e2))
            else
              v = callGeneric(uUser(e1), uUser(e2))
            return(v)
          }
)

#' @name Logic
#' @rdname svgunit.groupGeneric-methods
#' @aliases Logic,SVGUnit-method
setMethod("Logic", signature="SVGUnit", 
          definition=function(e1, e2) 
          {
            if( (uUnits(e1) == uUnits(e2)) 
                || (uValue(e1) == 0)
                || (uValue(e2) == 0) )
              v = callGeneric(uValue(e1), uValue(e2))
            else
              v = callGeneric(uUser(e1), uUser(e2))
            return(v)
          }
)

#' @name Ops
#' @rdname svgunit.groupGeneric-methods
#' @aliases Ops,SVGUnit-method
setMethod("Ops", signature="SVGUnit", 
          definition=function(e1, e2) 
          {
            if(uUnits(e1) == uUnits(e2))
              v = callGeneric(uValue(e1), uValue(e2))
            else              
              v = callGeneric(uUser(e1), uUser(e2))
            return(v)
          }
)
