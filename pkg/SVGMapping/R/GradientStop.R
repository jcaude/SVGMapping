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

## G R A D I E N T S   S T O P S
## --------------------------------------------------

setGenericVerif <- function(name,y){if(!isGeneric(name)){setGeneric(name,y)}else{}}

#' This class is used by 
#' @
#' @export
setClass("GradientStop",
         representation(offset="character",
                        stop.color="character",
                        stop.opacity="numeric"
                        )
         )

setGenericVerif(name=".xml", function(object) { standardGeneric(".xml") })

setMethod(f="initialize", signature="GradientStop",
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
            if(!is.null(args.names)) arg.names <- list()
            
            ## default init.
            .Object@offset <- .arg("offset","0")
            .Object@stop.color <- .arg("stop.color","white")
            .Object@stop.opacity <- .arg("stop.opacity",1.0)
            
            ## eop
            return(.Object)
          }
          )

setMethod(f=".xml", signature="GradientStop",
          definition=function(object)
          {
            ## SVG rendering
            svg <- newXMLNode("stop",
                              attrs=list(offset=object@offset,
                                "stop-color"=object@stop.color,
                                "stop-opacity"=object@stop.opacity)
                              )
            return(svg)
          }
          )

## F A C T O R Y
##----------------------------------------

GradientStop.factory <- function(offset="0",color="white",opacity=1.0) {

  ## init.
  if(is.numeric(offset)) offset <- as.character(offset)

  ## create offset.
  return(new("GradientStop", offset=offset, stop.color=color, stop.opacity=opacity))
}


