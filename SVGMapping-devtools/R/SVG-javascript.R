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

## S V G  -- JAVASCRIPTS
## --------------------------------------------------

#' SVG Javascript Accessors
#' 
#' These methods allows to associate some javascript code to an SVG document. 
#' This code could be directly attache to the document or referenced as an 
#' external document.
#' 
#' The \code{jsAnimation(object)} method returns \code{TRUE} if the SVG document
#' have some animations or \code{FALSE} otherwise.
#' 
#' @name jsAnimation
#'   
#' @param object is the SVG document object
#'   
#' @return \code{jsAnimations} returns a logical value
#'   
#' @rdname svg.js-methods
#' @exportMethod jsAnimation
#' @docType methods
setGeneric(name="jsAnimation", function(object) {standardGeneric("jsAnimation")})

#' <title already defined>
#' 
#' 
#' 
#' The \code{jsAnimation(object) <- value} method allows to enable or disable
#' the SVGMapping animation engine. Here, \emph{value} is logical.
#' 
#' @name jsAnimation<-
#' 
#' @rdname svg.js-methods
#' @exportMethod jsAnimation<-
#' @docType methods
setGeneric(name="jsAnimation<-", function(.Object,value) {standardGeneric("jsAnimation<-")})

#' <title already defined>
#' 
#' 
#' 
#' The \code{jsHRefs(object)} method returns the list of Javascript HREF that 
#' are associated with the SVG documemnt.
#' 
#' @name jsHRefs
#' 
#' @return \code{jsHRefs} and \code{jsInlines} return lists
#'   
#' @rdname svg.js-methods
#' @exportMethod jsHRefs
#' @docType methods
setGeneric(name="jsHRefs", function(object) {standardGeneric("jsHRefs")})

#' <title already defined>
#' 
#' 
#' 
#' The \code{jsHRefs(object) <- value} method associates a named list of 
#' Javascript HREF with the SVG documemnt. List names are used as identifiers
#' and must be unique across the whole SVG document. One can use the \code{uid}
#' method to automatically forge new IDs.
#' 
#' @name jsHRefs<-
#'   
#' @rdname svg.js-methods
#' @exportMethod jsHRefs<-
#' @docType methods
NULL

#' <title already defined>
#' 
#' 
#' 
#' The \code{jsInlines(object)} method returns the list of inline Javascript
#' scripts that are associated with the SVG documemnt.
#' 
#' @name jsInlines
#'   
#' @rdname svg.js-methods
#' @exportMethod jsInlines
#' @docType methods
setGeneric(name="jsInlines", function(object) {standardGeneric("jsInlines")})

#' <title already defined>
#' 
#' 
#' 
#' The \code{jsInlines(object) <- value} method associates a named list of
#' inline Javascripts with the SVG documemnt.
#' 
#' @name jsInlines<-
#'   
#' @rdname svg.js-methods
#' @exportMethod jsInlines<-
#' @docType methods
NULL

#' SVG Script Processing
#' 
#' The \code{addScript(object)} method allows to insert script (either as 
#' inline text or href references) within the SVG document.
#' 
#' First script must be added to the SVG object using the 
#' \code{\link{jsHRefs()}} or \code{\link{jsInlines()}} method. If an \code{ID}
#' is set as argument only the related script will be updated. Otherwise, all
#' referenced scripts are be updated.
#' 
#' @name addScript
#' 
#' @param object is the SVG document object
#' @param id (optional) the script to add or update 
#' @return the list of nodes that have been added to the SVG document.
#' 
#' @rdname svg.jsproc-methods
#' @exportMethod addScript
#' @docType methods
setGeneric(name="addScript", function(object,id) {standardGeneric("addScript")})

#' @rdname svg.js-methods
#' @aliases jsAnimation,SVG-method
setMethod(f="jsAnimation", signature="SVG",
          definition=function(object)
          {
            return(object@js.animation)
          }
)

#' @name jsAnimation<-,SVG-method
#' @rdname svg.js-methods
#' @aliases jsAnimation<-,SVG-method
setReplaceMethod(f="jsAnimation", signature="SVG",
                 definition=function(.Object, value)
                 {
                   ## check
                   if(!is.logical(value))
                     stop("'value' must be logical")
                   
                   ## eop
                   .Object@js.animation <- value
                   return(.Object)
                 }
)

#' @rdname svg.js-methods
#' @aliases jsHRefs,SVG-method
setMethod(f="jsHRefs", signature="SVG",
          definition=function(object)
          {
            return(object@js.hrefs)
          }
)

#' @name jsHRefs<-,SVG-method
#' @rdname svg.js-methods
#' @aliases jsHRefs<-,SVG-method
setReplaceMethod(f="jsHRefs", signature="SVG",
                 definition=function(.Object,value)
                 {
                   # check
                   if(!is.list(value))
                     stop("'value' must be a list")
                   if(is.null(names(value)))
                     stop("'value' must be a named (ID) list")
                   if(length(value)==0)
                     return(.Object)                   
                   
                   # init.
                   l.names <- names(value)
                   l.names <- l.names[l.names != ""]
                   if(length(l.names) != length(value))
                     stop("all 'value' items must have a (non empty) ID")
                   
                   ## update js file list
                   for(i in 1:length(l.names)) {
                     name <- l.names[[i]]                     
                     .Object@js.hrefs[name] <- value[[i]]
                   }
                   
                   ## eop                
                   return(.Object)
                 }
)

#' @rdname svg.js-methods
#' @aliases jsInlines,SVG-method
setMethod(f="jsInlines", signature="SVG",
          definition=function(object)
          {
            return(object@js.inlines)
          }
)

#' @name jsInlines<-,SVG-method
#' @rdname svg.js-methods
#' @aliases jsInlines<-,SVG-method
setReplaceMethod(f="jsInlines", signature="SVG",
                 definition=function(.Object,value)
                 {
                   # check
                   if(!is.list(value))
                     stop("'value' must be a list")
                   if(is.null(names(value)))
                     stop("'value' must be a ID list")
                    if(length(value)==0)
                      return(.Object)
                   
                   # init.
                   l.names <- names(value)
                   l.names <- l.names[l.names != ""]
                   if(length(l.names) != length(value))
                     stop("all 'value' items must have a (non empty) ID")
                   
                   ## update js file list
                   for(i in 1:length(l.names)) {
                     name <- l.names[[i]]                     
                     .Object@js.inlines[name] <- value[[i]]
                   }
                   
                   ## eop                
                   return(.Object)
                 }
)

#' @rdname svg.jcproc-methods
#' @aliases addScript,SVG-method
setMethod(f="addScript", signature="SVG",
          definition=function(object,id)
          {
            ## -- insert node
            .insert_node <- function(object,id,script.node) {
              updated <- FALSE
              search.node <- object[paste("id::",id,sep="")]
              if(length(search.node) > 0) {
                replaceNodes(search.node[[1]], script.node)
                updated <- TRUE
              }
              if(!updated)
                addChildren(xmlRoot(object@svg), kids=list(script.node))              
            }
            
            ## -- process href scripts
            .proc_js_href <- function(object, id) {
              
              # check
              if( !(id %in% names(jsHRefs(object))) )
                stop("Can't find script among inlines/files script definitions")
              
              # init
              js_attrs <- list(id=id, 
                               type="text/ecmascript",
                               'xlink:href'=jsHRefs(object)[[id]])
              js_node <- newXMLNode("script", attrs=js_attrs)
              .insert_node(object,id,js_node)
              
              # eop
              return(js_node)
            }
            
            ## -- process inline scripts
            .proc_js_inline <- function(object, id) {

              # check
              if( !(id %in% names(jsInlines(object))) )
                stop("Can't find script among inlines/files script definitions")
              
              # init.
              script <- jsInlines(object)[[id]]
              
              # build script nodes
              cdata <- newXMLCDataNode(paste("\n", script, "\n", sep=""))
              js_attrs <- list(id=id, type="text/ecmascript")
              js_node <- newXMLNode("script", attrs=js_attrs, .children=list(cdata))
              .insert_node(object,id,js_node)
              
              # eop
              return(js_node)
              
            }
            
            ## -- All scripts
            nodes <- list()
            if(missing(id)) {
              # hrefs
              hrefs <- names(jsHRefs(object))
              if(length(hrefs) > 0) {
                for(i in 1:length(hrefs))
                  nodes <- c(nodes,.proc_js_href(object,hrefs[[i]]))
              }
              # inlines
              inlines <- names(jsInlines(object))
              if(length(inlines) > 0) {
                for(i in 1:length(inlines))
                  nodes <- c(nodes,.proc_js_inline(object,inlines[[i]]))
              }
            }
            
            ## -- Specific Script
            else {
              if(id %in% names(jsHRefs(object)))
                nodes <- c(nodes,.proc_js_href(object,id))
              else if(id %in% names(jsInlines(object)))
                nodes <- c(nodes,.proc_js_inline(object,id))
              else
                warning(paste("Script with ID='",id,"' not found",sep=""))
            }            
            
            ## eop
            return(nodes)
          }
)
