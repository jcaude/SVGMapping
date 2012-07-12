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

## S V G S H A P E
## --------------------------------------------------

setGenericVerif <- function(name,y){if(!isGeneric(name)){setGeneric(name,y)}else{}}

#' SVG Shapes root class (VIRTUAL)
#' 
#' This class is the root class for all SVG shapes. It contains the specifications
#' for the core attributes of the basic shapes (detailed in SVG 1.1 specification).
#' This class can't be directly instantiate and must be derived.
#' 
#' @seealso \link{SVGNode}
#' @exportClass "SVGShape"
#' @aliases SVGShape-class
setClass("SVGShape",
         representation("VIRTUAL"),
         contains=c("SVGNode","CSS")
         )

setGenericVerif(name=".xml", function(object) { standardGeneric(".xml") })

setMethod(f="initialize", signature="SVGShape",
          definition=function(.Object,...)
          {
            ## - locals
            .arg <- function(name,default.value) {
              if(any(grepl(paste("^",name,"$",sep=""), args.names))) {
                v <- args[[grep(paste("^",name,"$",sep=""),args.names)]]
                return(v)
              }
              else {
                return(default.value)
              }
            }

            ## super
            .Object <- callNextMethod(.Object,...)

            ## super (CSS)
            .Object <- .callMethod("initialize","CSS",.Object,...)
            
            ## eop
            return(.Object)
          }
          )

#' @rdname svgnode.xml-methods
#' @aliases .xml,SVGShape-method
setMethod(f=".xml", signature="SVGShape",
          definition=function(object)
          {
            ## super (SVGNode)
            attr <- callNextMethod(object)
            
            ## super (CSS)
            attr <- c(attr,.callMethod(".xml","CSS",object))

            ## eop
            return(attr)            
          }
          )
