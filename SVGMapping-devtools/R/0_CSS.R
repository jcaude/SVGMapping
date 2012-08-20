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

## C S S  
## --------------------------------------------------
setGenericVerif <- function(name,y){if(!isGeneric(name)){setGeneric(name,y)}else{}}

#' CSS inline attribute class (VIRTUAL)
#' 
#' This class handles all style proterties that are defined using inline CSS
#' instructions. Furthermore, this class also provides some useful accessors
#' to deal with inline style getter and/or setter.
#' 
#' This class is mostly used in a multiple inheritance scheme, where it provides
#' the additional definitions and implementations to deal with style properties.
#' Due to this constraints there is no initialization method but a single 
#' prototype field in the class defintion.
#' 
#' @exportClass "CSS"
#' @aliases CSS-class
setClass("CSS",
         representation(css.class="character",
                        css.style="character",
                        "VIRTUAL"),
         prototype=c(character(0),character(0))
         )

#' CSS Attributes Accessors
#'
#' These methods are accessors to the \emph{CSS} attributes of an
#' object. These attributes are described in the SVG 1.1 specifications document.
#'
#' The \code{cssClass(object)} method returns the CSS \emph{class} attributes of 
#' an object.
#' 
#' @name cssClass
#' 
#' @param object is an object that inherits from the CSS class
#'
#'  @return a core attribute as a string
#' 
#' @rdname css.core-methods
#' @exportMethod cssClass
#' @docType methods
setGeneric(name="cssClass", function(object) { standardGeneric("cssClass") })

#' <title already defined>
#' 
#' 
#' 
#' The \code{cssClass(object) <- value} method sets the CSS \emph{class} attributes of 
#' an object.
#' 
#' @name cssClass<-
#' 
#' @rdname css.core-methods
#' @exportMethod cssClass<-
#' @docType methods
setGeneric(name="cssClass<-", function(.Object,value) { standardGeneric("cssClass<-") })

#' <title already defined>
#'
#'
#'
#' The \code{cssStyle(object)} method returns the CSS \emph{style} attributes of 
#' an object.
#' 
#' @name cssStyle
#' 
#' @rdname css.core-methods
#' @exportMethod cssStyle
#' @docType methods
setGeneric(name="cssStyle", function(object) { standardGeneric("cssStyle") })

#' <title already defined>
#' 
#' 
#' 
#' The \code{cssStyle(object) <- value} method sets the CSS \emph{style} attributes of 
#' an object.
#' 
#' @name cssStyle<-
#' 
#' @rdname css.core-methods
#' @exportMethod cssStyle<-
#' @docType methods
setGeneric(name="cssStyle<-", function(.Object,value) { standardGeneric("cssStyle<-") })

setMethod(f="initialize", signature="CSS",
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
            args <- list(...)
            args.names <- names(args)
            if(is.null(args.names)) args.names <- list()
            
            ## default init.
            cssClass(.Object) <- .arg("class", character(0))
            cssStyle(.Object) <- .arg("style", character(0))
            
            ## eop
            return(.Object)
          }
          )

#' @rdname css.core-methods
#' @aliases cssClass,CSS-method
setMethod(f="cssClass", signature="CSS",
          definition=function(object)
          {
            return(object@css.class)
          }
)

#' @name cssClass<- 
#' @rdname css.core-methods
#' @aliases cssClass<-,CSS-method 
setReplaceMethod(f="cssClass", signature="CSS",
                 definition=function(.Object,value)
                 {
                   ## check
                   if(!(is.vector(value) && !is.list(value) && is.character(value)))
                     stop("'value' must be a vector of character strings")
                   
                   ## assign & eop
                   .Object@css.class <- value
                   return(.Object)
                 }
)

#' @rdname css.core-methods
#' @aliases cssStyle,CSS-method
setMethod(f="cssStyle", signature="CSS",
          definition=function(object)
          {
            return(object@css.style)
          }
)

#' @name cssStyle<- 
#' @rdname css.core-methods
#' @aliases cssStyle<-,CSS-method
setReplaceMethod(f="cssStyle", signature="CSS",
                 definition=function(.Object,value)
                 {
                   ## check
                   if(!(is.atomic(value) && is.character(value)))
                     stop("'value' must be an atomic string")
                   
                   ## assign & eop
                   .Object@css.style <- value
                   return(.Object)                   
                 }
)

#' @rdname svgnode.xml-methods
#' @aliases .xml,CSS-method
setMethod(f=".xml", signature="CSS",
          definition=function(object)
          {
            ## super
            attr <- list()
            
            ## return a list of core attributes
            if(length(cssClass(object)) > 0)
              attr <- c(attr, class=cssClass(object))
            if(length(cssStyle(object)) > 0)
              attr <- c(attr, style=cssStyle(object))
            return(attr)
          }
)
