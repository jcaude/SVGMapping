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
         representation(svg.transform="SVGTransform",
                        "VIRTUAL"),
         contains=c("SVGNode")
         )

#' Shape Core Attributes Accessors
#' 
#' These methods are accessors to the \emph{core} attributes of an SVG shape.
#' These attributes are the one defined in the SVG 1.1 specifications.
#' 
#' The \code{svgTransform(object)} method returns the SVG \emph{transform}
#' attributes of an SVG shape
#' 
#' @name svgTransform
#'   
#' @param object an SVG shape
#'   
#' @return an \link{SVGTransform} object
#'   
#' @rdname svgshape.core-methods
#' @exportMethod svgTransform
#' @docType methods
setGeneric(name="svgTransform", function(object) { standardGeneric("svgTransform") })

#' <title already defined>
#' 
#' 
#' 
#' The \code{svgTransform(object) <- value} method sets the SVG \emph{transform}
#' attributes of an SVG shape. \emph{value} must be a valid \link{SVGTransform}
#' object or a compliant definition as a character string. In the latter, it
#' will be automatically converted into an \link{SVGTransform} object.
#' 
#' @name svgTransform<-
#'   
#' @rdname svgshape.core-methods
#' @exportMethod svgTransform<-
#' @docType methods
setGeneric(name="svgTransform<-", function(.Object,value) { standardGeneric("svgTransform<-") })


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

            ## get args
            args = list(...)
            args.names = names(args)
            if(is.null(args.names)) args.names <- list()
            
            ## default init.
            svgTransform(.Object) <- .arg("transform",SVGTransform.factory())
            
            ## eop
            return(.Object)
          }
          )

#' @rdname svgshape.core-methods
#' @aliases svgTransform,SVGShape-method
setMethod(f="svgTransform", signature="SVGShape",
          definition=function(object)
          {
            return(object@svg.transform)
          }
)

#' @name svgTransform<- 
#' @rdname svgshape.core-methods
#' @aliases svgTransform<-,SVGShape-method
setReplaceMethod(f="svgTransform", signature="SVGShape",
                 definition=function(.Object,value)
                 {
                   ## check
                   if(!(is.atomic(value) && is.character(value)) ||
                        !(is.object(value) && is(value,"SVGTransform")))
                     stop("'value' must be atomic and a string or an SVGTransform object")
                   
                   ## assign & eop
                   if(is.object(values))
                     .Object@svg.transform <- value
                   else
                     .Object@svg.transform <- SVGTransform.factory(value)
                   return(.Object)
                 }
)

#' @rdname svgcore.xml-methods
#' @aliases .xml,SVGShape-method
setMethod(f=".xml", signature="SVGShape",
          definition=function(object)
          {
            ## super (SVGNode)
            attr <- callNextMethod(object)
            
            # init.
            tf <- .xml(svgTransform(object))
            
            ## return an attributes list            
            if(length(tf) >0) 
              attr <- c(attr,transform=tf)
            
            ## eop
            return(attr)            
          }
          )
