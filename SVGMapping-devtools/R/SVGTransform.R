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

## S V G   T R A N S F O R M
## --------------------------------------------------

#' SVGTransform SVG Transform Class Descritpion
#' 
#' This class is used as a container for all SVG transformations operations
#' 
#' @name SVGTransform
#' @exportClass "SVGTransform"
#' @aliases SVGTransform-class
setClass("SVGTransform",
         representation(transforms="vector",
                        ctm="matrix",
                        .tm="matrix")
)

#' Transforms Accessors
#' 
#' These methods are accessors to the list of transform operations stored as a
#' vector of character strings.
#'
#' The \code{transform(object)} method returns the current vector of 
#' transformations
#' 
#' @name transforms
#'   
#' @param object is an SVGTransformation object
#'   
#' @return \code{transforms} returns a vector of characters
#'   
#' @rdname svgtransform.transforms-methods
#' @exportMethod transforms
#' @docType methods
setGeneric(name="transforms", function(object) { standardGeneric("transforms") })

#' <title already defined>
#' 
#' 
#' 
#' The \code{transforms()<-} method sets the vector of transformations
#' of a SVGTransform object.
#' 
#' @name transforms<-
#' 
#' @rdname svgtransform.transforms-methods
#' @exportMethod transforms<-
#' @docType methods
setGeneric(name="transforms<-", function(.Object,value) { standardGeneric("transforms<-") })

#' CTM Matrix Accessors
#' 
#' This method is a getter to the Current Transformation Matrix (CTM) of a 
#' \emph{SVGTransform} object. CTM are 3x3 numeric matrices as defined in the 
#' SVG 1.1 specification. Transformation matrices are of the form:
#'
#' m = [ [a c e]
#'       [b d f]
#'       [0 0 1] ]
#'       
#' where [a b c d e f] are the transformation parameters
#' 
#' The \code{ctm(object)} method returns the current transformation matrix
#' 
#' @name ctm
#'   
#' @param object is an SVGTransformation object
#'   
#' @return \code{ctm} returns a numeric matrix
#'   
#' @rdname svgtransform.ctm-methods
#' @exportMethod ctm
#' @docType methods
setGeneric(name="ctm", function(object) { standardGeneric("ctm") })

#' Show method for SVGTransform objects
#' 
#' Various methods to show or convert SVGTransform objects
#' 
#' The \code{show()} method called with an SVGTransform object as an argument 
#' allows display the current transforms operations as a string.
#' 
#' @name show
#' 
#' @param object is a \code{\link{SVGTransform}} instance.
#' 
#' @rdname svgtransform.show-methods
#' @docType methods
NULL

#' <title already defined>
#' 
#' 
#' 
#' The \code{as.character()} method converts an SVGTransform object into a 
#' string
#' 
#' @name as.character
#' @rdname svgtransform.show-methods
#' @docType methods
NULL

#' Print an SVGTransform object to the terminal
#' 
#' This method attempts to print SVGTransform object to the terminal as text
#' 
#' @name print.SVGTransform
#' 
#' @param x the SVGTransform object to print
#' 
#' @return the SVGTransform object invisibly
#' 
#' @rdname svgtransform.print-methods
#' @exportMethod print.SVGTransform
#' @docType methods
setGeneric(name="print.SVGTransform", function(x,...) { standardGeneric("print.SVGTransform") })

setMethod(f="initialize", signature="SVGTransform",
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
            transforms(.Object) <- .arg("transform",character())
            
            ## eop
            return(.Object)
          }
)

#' @rdname svgtransform.transforms-methods
#' @aliases transforms,SVGTransform-method
setMethod(f="transforms",signature="SVGTransform",
          definition=function(object)
          {
            return(object@transforms)
          }
)

#' @name transforms<-
#' @rdname svgtransform.transforms-methods
#' @aliases transforms<-,SVGTransform-method
setReplaceMethod(f="transforms", signature="SVGTransform",
                 definition=function(.Object,value)
                 {
                   # check
                   if(!is.character(value) || length(value) > 1)
                     stop("transform 'value' must be a string of characters")
                     
                   # init.
                   .Object@ctm <- matrix(numeric(0), 0,0)
                   value <- gsub("\\)(,|[ ])+","\\);",value)
                   value <- gsub("^[ ]*","",value)
                   value <- gsub("[ ;]*$","",value)
                   if(length(value)>0)
                     .Object@transforms <- strsplit(value,";")[[1]]
                   else
                     .Object@transforms <- character()
                     
                   # check
                   if(length(.Object@transforms) == 0) {
                     .Object@.tm <- diag(3)
                   }
                    
                   # loop over transforms
                   for(i in 1:length(.Object@transforms)) {
                     t <- .Object@transforms[[i]]
                     
                   }
                   
                   # eop
                   return(.Object)
                 }
)
                   

#' @rdname svgtransform.ctm-methods
#' @aliases ctm,SVGTransform-method
setMethod(f="ctm",signature="SVGTransform",
          definition=function(object) 
          {
            return(object@ctm)
          }
)

#' @rdname svgtransform.show-methods
#' @aliases show,SVGTransform-method
setMethod(f="show",signature="SVGTransform",
          definition=function(object)
          {
            cat(as.character(object),"\n")
          })

#' @rdname svgtransform.show-methods
#' @aliases print.SVGTransform,SVGTransform-method
setMethod(f="print.SVGTransform", signature="SVGTransform",
          definition=function(x,...)
          {
            print(as.character(x))
            return(invisible(x))
          }
)

#' @rdname svgtransform.show-methods
#' @aliases as.character,SVGTransform-method
setMethod(f="as.character", signature="SVGTransform",
          definition=function(x,...)
          {
            return(paste(transforms(x),collapse=" "))
          }
)

## F A C T O R Y
##--------------

#' SVGTransform Factory
#' 
#' This function returns a new \code{\link{SVGTransform}} object.
#' \code{SVGTransform} allows to describe geometrical transformation that can
#' applied to a (group) SVG graphical object(s).
#' 
#' @name SVGTransform.factory
#'   
#' @param transform the SVG transform list as a string
#'
#' @return a \code{\link{SVGTransform}} object
#'   
#' @export SVGTransform.factory
#'   
#' @examples
#' # a skew x=4.2,y=3.1 transformation given an as css instruction
#' SVGTransform.factory("skewY(3.1) skewX(4.2)")
SVGTransform.factory <- function(transform) {
    
  ## eop
  return(new("SVGTransform", transform=transform))  
}

