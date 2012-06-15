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

setGenericVerif <- function(name,y){if(!isGeneric(name)){setGeneric(name,y)}else{}}

setClass("SVGNode",
         representation(id="character")
         )

setGenericVerif(name="id", function(object) { standardGeneric("id") })
setGenericVerif(name="id<-", function(.Object, value) { standardGeneric("id<-") })
setGenericVerif(name=".xml", function(object) { standardGeneric(".xml") })

setMethod(f="initialize", signature="SVGNode",
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

            ## default init.
            .Object@id <- .arg("id",character(0))

            ## eop
            return(.Object)
          }
          )

setMethod(f="id", signature="SVGNode",
          definition=function(object)
          {
            return(object@id)
          }
          )

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

setMethod(f=".xml", signature="SVGNode",
          definition=function(object)
          {
            ## init.
            attr <- list()
            
            ## return an attributes list
            if(length(id(object)) > 0) attr <- list(id=id(object))
            return(attr)
          }
          )
