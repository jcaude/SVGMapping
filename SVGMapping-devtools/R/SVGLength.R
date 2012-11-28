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

setMethod(f="initialize", signature="SVGUnit",
          definition=function(.Object,...)
          {            
            # super
            .Object <- callNextMethod(.Object,...)
            
            # check for relative units
            if(uUnits(.Object) %in% c("em","ex","%"))
              .Object@u.relative <- TRUE
            
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
            if(isRelative(object))
              return(NA)
            else
              return(callNextMethod(object))
          }
)

## F A C T O R Y
##--------------

#' SVGLength Factory

