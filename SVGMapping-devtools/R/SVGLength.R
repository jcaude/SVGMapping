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

## S V G   L E N G T H  
## --------------------------------------------------
setGenericVerif <- function(name,y){if(!isGeneric(name)){setGeneric(name,y)}else{}}

#' SVGLength SVG Length Class Descritpion
#' 
#' This class is used as a container for all SVG length values
#' 
#' @name SVGLength
#' @exportClass "SVGLength"
#' @aliases SVGLength-class
setClass("SVGLength",
         representation(u.relative="logical"),
         contains=c("SVGUnit")
)

#' Check for relative length
#' 
#' This method checks if the length value is relative to another one or not.
#' 
#' @name isRelative
#' 
#' @param object is the SVGLength object
#' 
#' @return TRUE for a relative length value, FALSE otherwise
#' 
#' @rdname svglength.relative-methods
#' @exportMethod isRelative
#' @docType methods
setGeneric(name="isRelative", function(object) { standardGeneric("isRelative") })

#' Value in user units system accessor
#' 
#' This method allows to access to the length value in the user units system, 
#' Unless the length value is given relatively to another one (\emph{eg} 'em',
#' 'ex' or '%' units systems)
#' 
#' @name uUser
#' 
#' @param object is an SVGLength object
#'   
#' @return return a numeric unit value or \code{NA} for relative length 
#'   
#' @rdname svglength.unit-methods
#' @exportMethod uUser
#' @docType methods
NULL

#' Group Generic methods for SVGLength objects
#' 
#' Some of the S4 group generic methods are implemented for SVGLength objects.
#' For more details see \link{S4groupGeneric} documentation page.
#' 
#' @name Arith
#'   
#' @rdname svglength.groupGeneric-methods
#' @docType methods
NULL

setMethod(f="initialize", signature="SVGLength",
          definition=function(.Object,...)
          {            
            # super
            .Object <- callNextMethod(.Object,...)
            
            # check for valid units
            if(!uUnits(.Object) %in% c("pt","pc","cm","mm","in","px","","em","ex","%"))
              stop("Invalid Units System.. failed to create object")
            
            # check for relative units
            if(uUnits(.Object) %in% c("em","ex","%"))
              .Object@u.relative <- TRUE
            else
              .Object@u.relative <- FALSE
            
            # eop
            return(.Object)
          }
)

#' @rdname svglength.relative-methods
#' @aliases isRelative,SVGLength-method
setMethod(f="isRelative", signature="SVGLength",
          definition=function(object)
          {
            return(object@u.relative)
          }
)

#' @rdname svglength.unit-methods
#' @aliases uUser,SVGLength-method
setMethod(f="uUser",signature="SVGLength",
          definition=function(object) 
          {
            if(isRelative(object) && (uValue(object) != 0))
              return(NA)
            else
              return(callNextMethod(object))
          }
)

#' @rdname svglength.groupGeneric-methods
#' @aliases Arith,SVGLength-method
setMethod("Arith", signature="SVGLength", 
          definition=function(e1, e2) 
          {
            if((uUnits(e1) == uUnits(e2)) && (uDpi(e1) == uDpi(e2))) {
              v <- callGeneric(uValue(e1),uValue(e2))
              sv <- SVGLength.factory(v,unit=uUnits(e1),dpi=uDpi(e1))
            }
            else {
              v = callGeneric(uUser(e1), uUser(e2))
              sv <- SVGLength.factory(v,dpi=uDpi(e1), target.unit=uUnits(e1))
            }
            return(sv)
          }
)

## F A C T O R Y
##--------------

#' SVGLength Factory
#' 
#' This function returns a new \code{\link{SVGLength}} object. \code{SVGLength} 
#' is a subclass of \code{\link{SVGUnit}} that allows relative units such as 
#' \emph{'em'}, \emph{'ex'} or \emph{'\%'}.
#' 
#' \code{SVGLength} objects are used to specify length of some shape attributes. 
#' 
#' @name SVGLength.factory
#'   
#' @param x the unit value
#' @param unit the value unit system
#' @param dpi the device resoluation
#' @param target.unit the return unit system after value conversion. Note that
#'   only non relative source units can be converted into another unit system.
#'   
#' @return a \code{\link{SVGLength}} object
#'   
#' @export SVGLength.factory
#' 
#' @seealso The \code{\link{SVGUnit.factory}} function.
#'   
#' @examples
#' SVGLength.factory(1.5)
#' SVGLength.factory(7,"%")
#' SVGLength.factory(10,"px",dpi=100)
#' SVGLength.factory("10.43em")
#' SVGLength.factory(0.9,"in",target.unit="px")
#' SVGLength.factory(1.5,"cm") - SVGLength.factory(70,"mm")
SVGLength.factory <- function(x,unit,dpi,target.unit) {
  
  ## init.
  if(missing(x)) x <- 0
  if(missing(unit)) unit <- ""
  
  ## check: character value+unit (eg '10cm')
  if(is.character(x)) {
    v <- x
    x <- as.numeric(gsub("([-+]?[0-9]*\\.?[0-9]+([eE][-+]?[0-9]+)?)(.*)",
                         "\\1",v))
    unit <- gsub("([-+]?[0-9]*\\.?[0-9]+([eE][-+]?[0-9]+)?)([ \\t]*)(.*)",
                 "\\4",v)
  }
  
  ## create length value
  if(missing(dpi))
    svg_length <- new("SVGLength",x,unit)
  else
    svg_length <- new("SVGLength",x,unit,dpi=dpi)
  
  ## target unit conversion
  if(!missing(target.unit) && !isRelative(svg_length)) {
    user_value <- uUser(svg_length)
    target_unit <- paste("U_",target.unit,sep="")
    dpi <- 1/uDpi(svg_length)
    target_value <- switch(target_unit,
                           U_mm = dpi * 25.4 * user_value,
                           U_px = user_value,
                           U_pt = dpi * 72 * user_value,
                           U_pc = dpi * 6 * user_value,
                           U_cm = dpi * 2.54 * user_value,
                           U_in = dpi * user_value,
                           U_ = user_value,
                           default= NA)
    svg_length <- new("SVGLength",target_value,target.unit,dpi=uDpi(svg_length))
  }
  
  ## eop
  return(svg_length)  
}
