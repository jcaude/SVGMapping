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

## L A Y O U T
## --------------------------------------------------

setGenericVerif <- function(name,y){if(!isGeneric(name)){setGeneric(name,y)}else{}}

#' Layout root class (VIRTUAL)
#' 
#' This class is the root class for all document layout methods.
#' This class can't be directly instantiate and must be derived. 
#' 
#' @seealso \code{\link{SVGNode}} and \code{\link{Rectangle}} classes
#' @exportClass "Layout"
#' @aliases Layout-class
setClass("Layout",
         representation(opacity="numeric",
                        "VIRTUAL"),
         contains=c("SVGNode","Rectangle")
         )

#' Layout Opacity level
#'
#' By default layout are invisible to the user. But one can control the opacity level
#' of a layout using the methods presented here.
#' 
#' Layout can be preview using the \code{\link{show}} method. The opacity method determines 
#' the visibility level of the layout borders.
#'
#' @name opacity
#' 
#' @param object a Layout object
#' 
#' @return the opacity level as a numeric value in the range [0,1]
#' 
#' @rdname layout.opacity-methods
#' @exportMethod opacity
#' @docType methods
setGeneric(name="opacity", function(object) { standardGeneric("opacity") })

#' <title already defined>
#' 
#' 
#' 
#' The \code{opacity(object)<-value} method can be used to set the opacity level of 
#' the layout. The opacity \bold{value} paramter must be in the range [0,1]. 
#' 
#' @name opacity<-
#' 
#' @rdname layout.opacity-methods
#' @exportMethod opacity<-
#' @docType methods 
setGeneric(name="opacity<-", function(.Object,value) { standardGeneric("opacity<-") })


setMethod(f="initialize", signature="Layout",
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

            ## super (SVGNode)
            .Object <- callNextMethod(.Object,...)

            ## super (Rectangle)
            .Object <- .callMethod("initialize","Rectangle",.Object,...)
            
            ## get args
            args = list(...)
            args.names = names(args)
            if(is.null(args.names)) args.names <- list()

            ## default init.
            opacity(.Object) <- .arg("opacity",0.0)

            ## eop
            return(.Object)         
          }
          )

#' @rdname layout.opacity-methods
#' @aliases opacity,Layout-method
setMethod(f="opacity", signature="Layout",
          definition=function(object)
          {
            return(object@opacity)
          }
          )

#' @name opacity<- 
#' @rdname layout.opacity-methods
#' @aliases opacity<-,Layout-method
setReplaceMethod(f="opacity", signature="Layout",
                 definition=function(.Object,value)
                 {
                   ## check
                   if(!is.atomic(value) && !is.numeric(value))
                     stop("'value' must be atomic and numeric")

                   ## assign & eop
                   .Object@opacity <- value
                   return(.Object)
                 }
                 )

#' @rdname svgnode.xml-methods
#' @aliases .xml,Layout-method
setMethod(f=".xml", signature="Layout",
          definition=function(object)
          {
            ## super (SVGNode)
            attr <- callNextMethod(object)
            
            ## super (Rectangle)
            attr <- c(attr,.callMethod(".xml","Rectangle",object))
            
            ## other attributes
            if(opacity(object) != 0) 
              attr <- c(attr,opacity=opacity(object))
            
            ## eop
            return(attr)
          }
)

