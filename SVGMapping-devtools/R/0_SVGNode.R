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

## S V G   N O D E
## --------------------------------------------------

#' SVG Node root class (VIRTUAL)
#' 
#' SVGNode is the root class for all SVG elements sub-class. Normally it should not be directly
#' instantiate. All SVG related classes derived from this class. It also provides methods to
#' convert the class into XML representation using the \code{\link{.xml}} method.
#' This class can't be directly instantiate and must be derived. 
#'
#' @exportClass "SVGNode"
#' @aliases SVGNode-class
setClass("SVGNode",
         representation(id="character",
                        svg.transform="character",
                        "VIRTUAL")
         )

#' XML ID accessors of an SVG node
#'
#' This method returns the XML ID of an SVG Node
#' 
#' @name id
#' 
#' @param object an SVG node
#' 
#' @return a character string
#' 
#' @rdname svgnode.id-methods
#' @exportMethod id
#' @docType methods
setGeneric(name="id", function(object) { standardGeneric("id") })

#' <title already defined>
#' 
#' 
#' 
#' The \code{id(object)<-} method can be used to set an XML ID to an SVG Node
#' (see the detail section)
#' 
#' @name id<-
#' 
#' @rdname svgnode.id-methods
#' @exportMethod id<-
#' @docType methods
setGeneric(name="id<-", function(.Object, value) { standardGeneric("id<-") })

#' Node Core Attributes Accessors
#'
#' These methods are accessors to the \emph{core} attributes of an SVG node. These
#' attributes are the one defined in the SVG 1.1 specifications.
#'
#' The \code{svgTransform(object)} method returns the SVG \emph{transform} attributes of 
#' an SVG node
#' 
#' @name svgTransform
#' 
#' @param object an SVG Node
#' 
#' @return a character string containing the attribute value
#' 
#' @rdname svgnode.core-methods
#' @exportMethod svgTransform
#' @docType methods
setGeneric(name="svgTransform", function(object) { standardGeneric("svgTransform") })

#' <title already defined>
#' 
#' 
#' 
#' The \code{svgTransform(object) <- value} method sets the SVG \emph{transform} attributes of 
#' an SVG node
#' 
#' @name svgTransform<-
#' 
#' @rdname svgnode.core-methods
#' @exportMethod svgTransform<-
#' @docType methods
setGeneric(name="svgTransform<-", function(.Object,value) { standardGeneric("svgTransform<-") })

#' SVG Node to XML Internal Representation Conversion
#' 
#' This is the based method used by derived classes to transform a node
#' into an XML internal description (aka XMLInternalNode).
#' 
#' In most virtual classes the method does not return the XML representation, 
#' but a list of attributes. This list can be subsequently used by 
#' \emph{sub-classes} to generate valid XML internal representation. In the latter
#' we usually expect a \code{XMLInternalNode} to be returned.
#' 
#' @param object the SVG node
#'
#' @return an XMLInternal Node or a list of attributes (virtual classes)
#' 
#' @rdname svgnode.xml-methods
#' @exportMethod .xml
#' @docType methods
setGeneric(name=".xml", function(object) { standardGeneric(".xml") })

setMethod(f="initialize", signature="SVGNode",
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

            ## get args
            args = list(...)
            args.names = names(args)
            if(is.null(args.names)) args.names <- list()

            ## default init.
            id(.Object) <- .arg("id",character(0))
            svgTransform(.Object) <- .arg("transform",character(0))
            
            ## eop
            return(.Object)
          }
          )

#' @rdname svgnode.id-methods
#' @aliases id,SVGNode-method
setMethod(f="id", signature="SVGNode",
          definition=function(object)
          {
            return(object@id)
          }
          )

#' @name id<-
#' @rdname svgnode.id-methods
#' @aliases id<-,SVGNode-method
setReplaceMethod(f="id", signature="SVGNode",
                 definition=function(.Object,value)
                 {
                   ## check
                   if(!is.character(value))
                     stop("'value' must be a character string")
                     
                   ## eop
                   .Object@id <- as.character(value)
                   return(.Object)
                 }
                 )

#' @rdname svgnode.core-methods
#' @aliases svgTransform,SVGNode-method
setMethod(f="svgTransform", signature="SVGNode",
          definition=function(object)
          {
            return(object@svg.transform)
          }
)

#' @name svgTransform<- 
#' @rdname svgnode.core-methods
#' @aliases svgTransform<-,SVGNode-method
setReplaceMethod(f="svgTransform", signature="SVGNode",
                 definition=function(.Object,value)
                 {
                   ## check
                   if(!is.atomic(value) && !is.character(value))
                     stop("'value' must be atomic and a string")
                   
                   ## assign & eop
                   .Object@svg.transform <- value
                   return(.Object)
                 }
)

#' @rdname svgnode.xml-methods
#' @aliases .xml,SVGNode-method
setMethod(f=".xml", signature="SVGNode",
          definition=function(object)
          {
            ## init.
            attr <- list()
            
            ## return an attributes list
            if(length(id(object)) > 0) attr <- list(id=id(object))
            if(length(svgTransform(object)) >0) 
              attr <- c(attr,transform=svgTransform(object))
            return(attr)
          }
          )
