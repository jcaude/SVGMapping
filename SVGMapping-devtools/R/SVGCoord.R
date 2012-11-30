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

## S V G   C O O R D
## --------------------------------------------------
setGenericVerif <- function(name,y){if(!isGeneric(name)){setGeneric(name,y)}else{}}

#' SVGCoord SVG Coordinates Class Descritpion
#' 
#' This class is used as a container for all SVG coordinates values
#' 
#' In practice it is only an \emph{alias} of the \code{\link{SVGLength}} class
#' 
#' @name SVGCoord
#' @exportClass "SVGCoord"
#' @aliases SVGCoord-class
setClass("SVGCoord",
         contains=c("SVGLength")
)

#' Group Generic methods for SVGCoord objects
#' 
#' Some of the S4 group generic methods are implemented for SVGCoord objects.
#' For more details see \link{S4groupGeneric} documentation page.
#' 
#' @name Arith
#'   
#' @rdname svgcoord.groupGeneric-methods
#' @docType methods
NULL

setMethod(f="initialize", signature="SVGCoord",
          definition=function(.Object,...)
          {            
            # super
            .Object <- callNextMethod(.Object,...)
            
            # eop
            return(.Object)
          }
)

#' @rdname svgcoord.groupGeneric-methods
#' @aliases Arith,SVGCoord-method
setMethod("Arith", signature="SVGCoord", 
          definition=function(e1, e2) 
          {
            if(uUnits(e1) == uUnits(e2))
              v <- callGeneric(uValue(e1),uValue(e2))
            else
              v = callGeneric(uUser(e1), uUser(e2))
            return(SVGCoord.factory(v,target.unit=uUnits(e1)))
          }
)


## F A C T O R Y
##--------------

#' SVGCoord Factory
#' 
#' This function returns a new \code{\link{SVGCoord}} object. \code{SVGCoord} is
#' a subclass of \code{\link{SVGUnit}} that allows relative units such as 
#' \emph{'em'}, \emph{'ex'} or \emph{'\%'}.
#' 
#' \code{SVGCoord} objects are used to specify coordinates of some shape
#' attributes.
#' 
#' @name SVGCoord.factory
#'   
#' @param x the unit value
#' @param unit the value unit system
#' @param dpi the device resoluation
#' @param target.unit the return unit system after value conversion. Note that 
#'   only non relative source units can be converted into another unit system.
#'   
#' @return an \code{\link{SVGCoord}} object
#'   
#' @export SVGCoord.factory
#'   
#' @seealso The \code{\link{SVGLengthfactory}} function.
#'   
#' @examples
#' SVGCoord.factory(1.5)
#' SVGCoord.factory(7,"%")
#' SVGCoord.factory(10,"px",dpi=100)
#' SVGCoord.factory("10.43em")
#' SVGCoord.factory(0.9,"in",target.unit="px")
#' SVGCoord.factory(1.5,"cm") - SVGCoord.factory(70,"mm")
SVGCoord.factory <- function(x,unit,dpi,target.unit) {
  
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
  
  ## create unit value
  if(missing(dpi))
    svg_coord <- new("SVGCoord",x,unit)
  else
    svg_coord <- new("SVGCoord",x,unit,dpi=dpi)
  
  ## target unit conversion
  if(!missing(target.unit) && !isRelative(svg_coord)) {
    user_value <- uUser(svg_coord)
    target_unit <- paste("U_",target.unit,sep="")
    dpi <- 1/uDpi(svg_coord)
    target_value <- switch(target_unit,
                           U_mm = dpi * 25.4 * user_value,
                           U_px = user_value,
                           U_pt = dpi * 72 * user_value,
                           U_pc = dpi * 6 * user_value,
                           U_cm = dpi * 2.54 * user_value,
                           U_in = dpi * user_value,
                           U_ = user_value,
                           default= NA)
    svg_coord <- new("SVGCoord",target_value,target.unit,dpi=uDpi(svg_coord))
  }
  
  ## eop
  return(svg_coord)  
}

