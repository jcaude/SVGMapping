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

## S V G  -- UTILITIES
## --------------------------------------------------

#' SVG Summary
#' 
#' summary of the SVG content
#' 
#' @name summary.SVG
#'   
#' @param object for which the summary is returned
#' @return a list of various statistics that includes xml, animations and
#'   javascript
#' 
#' @rdname svg.summary-methods
#' @exportMethod summary.SVG
#' @docType methods  
setGeneric(name="summary.SVG", function(object, ...) {standardGeneric("summary.SVG")})

#' Print an SVGUnit object to the terminal
#' 
#' This method attempts to print SVG object to the terminal as text
#' 
#' @name print.SVG
#' 
#' @param x the SVG object to print
#' 
#' @return the SVG object invisibly
#' 
#' @rdname svg.print-methods
#' @exportMethod print.SVG
#' @docType methods
setGeneric(name="print.SVG", function(x, ...) {standardGeneric("print.SVG")})

#' Show method for SVG objects
#' 
#' Display the SVG object by opening the document in a webpage using the default
#' browser.
#' 
#' @name show
#'   
#' @param object is a \code{\link{SVG}} instance.
#'   
#' @rdname svg.show-methods
#' @docType methods
NULL

#' Unique ID Generator
#' 
#' Forge unique identifier(s) among the SVG object. This function used random
#' sequences and an optional prefix to build new ids.
#' 
#' @note this function is not thread-safe at all.
#'   
#' @name uid
#'   
#' @param object is the SVG instance
#' @param prefix is a string used to prefix forged identifiers
#' @param n is the number of ids to return
#' 
#' @return a list of unique ids
#' 
#' @rdname svg.uid-methods
#' @exportMethod uid
#' @docType methods
setGeneric(name="uid", function(object,prefix,n) {standardGeneric("uid")})

#' @rdname svg.summary-methods
#' @aliases summary.SVG,SVG-method
setMethod(f="summary.SVG", signature="SVG",
          definition=function(object, ...)
          {
            ## init.
            svg.stats <- summary(object@svg)
            svg.stats$jsAnimation <- jsAnimation(object)
            svg.stats$scripts <- length(object@.js_scripts) + length(object@.js_files)
            
            ## return stats
            return(svg.stats)
          }
)

#' @rdname svg.show-methods
#' @aliases print.SVG,SVG-method
setMethod(f="print.SVG", signature="SVG",
          definition=function(x,...)
          {
            # print SVG as XML text
            print(object@svg)
            
            # eop
            return(invisible(object))
          }
)

#' @rdname svg.show-methods
#' @aliases show,SVG-method
setMethod(f="show", signature="SVG",
          definition=function(object)
          {
            ## init.
            browser <- getOption("browser")
            path <- tempfile()
            svg.path <- paste(path, ".svg", sep="")            
            html.path <- paste(path, ".html", sep="")
            
            ## save temp & write HTML page
            write.SVG(object, svg.path)
            con <- file(system.file("extdata/show-template.html", 
                                    package="SVGMapping"), "rb")
            html.template <- paste(readLines(con),collapse="\n")
            close(con)
            html <- sprintf(html.template,basename(svg.path))
            con <- file(html.path, "w")
            cat(html, file=con)
            close(con)
            browseURL(paste("file:///", html.path, sep=""), browser=browser)
          }
)

#' @rdname svg.uid-methods
#' @aliases uid,SVG-method
setMethod(f="uid", signature="SVG",
          definition=function(object,prefix,n)
          {
            ## check
            if(missing(prefix)) prefix <- "id"
            if(missing(n)) n <- 1
            
            ## create random uid
            ids <- list()
            for(i in 1:n) {            
              repeat {
                test.id <- paste(prefix,
                                 sprintf("%X",
                                         trunc(runif(n=1,min=1,max=99999999))
                                         ),
                                 sep="")
                chck=(length(object[paste("id::",test.id,sep="")]) == 0)
                chck=chck && (sum(test.id %in% ids) == 0)
                if(chck) break
              }
              ids <- c(ids,test.id)
            }
            
            ## eop
            if(n==1) ids <- ids[[1]]
            return(ids)
          }
)
