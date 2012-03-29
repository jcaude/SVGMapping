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
                        y2="character",
                        stops="list"
                        ),
         contains="Gradient"
         )

setGenericVerif(name="coords", function(object) { standardGeneric("coords") })
setGenericVerif(name="coords<-", function(.Object,value) { standardGeneric("coords<-") })
setGenericVerif(name="stops", function(object) { standardGeneric("stops") })
setGenericVerif(name="stops<-", function(.Object,value) { standardGeneric("stops<-") })
setGenericVerif(name="SVG", function(object) { standardGeneric("SVG") })

setMethod(f="initialize", signature="LinearGradient",
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

            ## super
            .Object <- callNextMethod(.Object,...)

            ## get args
            args = list(...)
            args.names = names(args)
            if(is.null(args.names)) args.names <- list()
            
            ## default init.
            if(sum(grepl("^coords$", args.names)) > 0) {
              coords <- args[["coords"]]
              if( is.list(coords) &&
                 (length(names(coords) %in% list("x1","y1","x2","y2"))==4) )
                flag <- TRUE
              .Object@x1 <- if(flag) value[["x1"]] else "0%"
              .Object@y1 <- if(flag) value[["y1"]] else "0%"
              .Object@x2 <- if(flag) value[["x2"]] else "100%"
              .Object@y2 <- if(flag) value[["y2"]] else "0%"
            }
            else {
              .Object@x1 <-.arg("x1", "0%")
              .Object@y1 <- .arg("y1", "0%")
              .Object@x2 <- .arg("x2", "100%")
              .Object@y2 <- .arg("y2", "0%")
            }
            .Object@stops <- list()
            
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

setMethod(f="show", signature="LinearGradient",
          definition=function(object)
          {
            cat("Linear Gradient:")
            callNextMethod(object)
            cat("\n")
            sapply(object@stops,show)
          }
          )

setMethod(f="stops", signature="LinearGradient",
          definition=function(object)
          {
            return(object@stops)
          }
          )

setReplaceMethod(f="stops", signature="LinearGradient",
                 definition=function(.Object,value)
                 {
                   ## case 1 - list()
                   if(is.list(value)) {
                     if(length(value) > 0) {
                       check <- sapply(value,function(x) {return(is.object(x) && is(x,"GradientStop"))})
                       value <- value[check]
                       .Object@stops <- value
                     }
                   }

                   ## case 2 - object
                   else if(is.object(value) && is(value, "GradientStop")) 
                     .Object@stops <- c(.Object@stops, value)

                   ## error
                   else 
                     stop("'value' must be either a list of GradientStop or a GradientStop")
                   
                   ## eop
                   return(.Object)
                 }
                 )

setMethod(f="SVG", signature="LinearGradient",
          definition=function(object)
          {
            ## init.
            gradient <- newXMLNode("linearGradient")
            
            ## core.attributes
            attr <- callNextMethod(object)
            
            ## attributes
            if(object@x1 != "0%") attr <- c(attr, x1=object@x1)
            if(object@y1 != "0%") attr <- c(attr, y1=object@y1)
            if(object@x2 != "100%") attr <- c(attr, x2=object@x2)
            if(object@y2 != "0%") attr <- c(attr, y2=object@y2)
            xmlAttrs(gradient) <- attr

            ## add stop elements
            svg.stops <- sapply(object@stops, SVG)
            addChildren(gradient,kids=svg.stops)

            ## eop
            return(gradient)
          }
          )

## F A C T O R Y
##----------------------------------------

LinearGradient.factory <- function(...,coords,spread.method,units,transform) {
  
  ## init. linear gradient
  args <- list("LinearGradient")
  if(!missing(coords)) args <- c(args, coords=coords)
  if(!missing(spread.method)) args <- c(args, spread.method=spread.method)
  if(!missing(units)) args <- c(args,units=units)
  if(!missing(transform)) args <- c(args, transform)
  linear.gradient = do.call(new, args)

  ## add stops
  stops(linear.gradient) <- list(...)
  
  ## eop
  return(linear.gradient)
}
