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

## L I N E A R   G R A D I E N T S
## --------------------------------------------------

setGenericVerif <- function(name,y){if(!isGeneric(name)){setGeneric(name,y)}else{}}

setClass("LinearGradient",
         representation(x1="character",
                        y1="character",
                        x2="character",
                        y2="character"
                        ),
         contains="Gradient"
         )

setGenericVerif(name="coords", function(object) { standardGeneric("coords") })
setGenericVerif(name="coords<-", function(.Object,value) { standardGeneric("coords<-") })
setGenericVerif(name="toSVG", function(object) { standardGeneric("toSVG") })

setMethod(f="initialize", signature="LinearGradient",
          definition=function(.Object,...)
          {
            ## super
            .Object <- callNextMethod()
            
            ## default init.
            .Object@x1 <- "0%"
            .Object@y1 <- "0%"
            .Object@x2 <- "100%"
            .Object@y2 <- "0%"

            ## eop
            return(.Object)
          }
          )

setMethod(f="coords", signature="LinearGradient",
          definition=function(object)
          {
            return(list(x1=object@x1,y1=object@y1,x2=object@x2,y2=objecty2))
          }
          )

setReplaceMethod(f="coords", signature="LinearGradient",
                 definition=function(.Object,value)
                 {
                   ## check
                   if(!is.list(value))
                     stop("'value' must be a 'list()'")
                   check <- names(value) %in% list("x1","y1","x2","y2")
                   if(length(check[check == TRUE]) != 4)
                     stop("'value' must at least contains 'x1,y1,x2,y2'")

                   ## assign & eop
                   .Object@x1 = value[["x1"]]
                   .Object@y1 = value[["y1"]]
                   .Object@x2 = value[["x2"]]
                   .Object@y2 = value[["y2"]]
                   return(.Object)
                 }
                 )

setMethod(f="toSVG", signature="LinearGradients",
          definition=function(object)
          {
            ## super
            core.attr <- callNextMethod()
            
            ## header
            header <- paste("<LinearGradient ", core.attr,sep="")
            if(object@x1 != "0%") header <- paste(header," x1=\"",object@x1,"\"",sep="")
            if(object@y1 != "0%") header <- paste(header," y1=\"",object@y1,"\"",sep="")
            if(object@x2 != "100%") header <- paste(header," x2=\"",object@x2,"\"",sep="")
            if(object@y2 != "0%") header <- paste(header," y2=\"",object@y2,"\"",sep="")
            header <- paste(header,">\n")

            ## stop elements
            svg.stops <- paste(sapply(object@stops, toSVG), collapse="\n")

            ## eop
            return(paste(header,stops,"</LinearGradient>",collapse="\n"))
          }
          )
