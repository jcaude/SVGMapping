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

## R A D I A L   G R A D I E N T
## --------------------------------------------------

setGenericVerif <- function(name,y){if(!isGeneric(name)){setGeneric(name,y)}else{}}

setClass("RadialGradient",
         representation(cx="character",
                        cy="character",
                        r="character",
                        fx="character",
                        fy="character"
                        ),
         contains="Gradient"
         )

setGenericVerif(name="coords", function(object) { standardGeneric("coords") })
setGenericVerif(name="coords<-", function(.Object,value) { standardGeneric("coords<-") })
setGenericVerif(name=".xml", function(object) { standardGeneric(".xml") })

setMethod(f="initialize", signature="RadialGradient",
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
            flag <- FALSE
            coords <- list()
            if(sum(grepl("^coords$", args.names)) > 0) {
              coords <- args[["coords"]]
              if( is.list(coords) &&
                 (length(names(coords) %in% list("cx","cy","r","fx","fy"))==5) )
                flag <- TRUE
            }
            .Object@cx <- if(flag) coords[["cx"]] else .arg("cx","50%")
            .Object@cy <- if(flag) coords[["cy"]] else .arg("cy","50%")
            .Object@r <- if(flag) coords[["r"]] else .arg("r","50%")
            .Object@fx <- if(flag) coords[["fx"]] else .arg("fx",.Object@cx)
            .Object@fy <- if(flag) coords[["fy"]] else .arg("fy",.Object@cy)

            ## eop
            return(.Object)
          }
          )

setMethod(f="show", signature="RadialGradient",
          definition=function(object)
          {
            ## forge an svg, apply linear gradient and just show it!
            svg <- SVG.factory(system.file("extdata/radial-gradient-sample.svg",
                                           package="SVGMapping")
                               )
            definitions(svg) <- object
            svg["id::circle-gradient","style::fill"] <- URL(object)
            show(svg)
            write.SVG(svg,file="~/Sources/R/Snippets/debug.svg")
          }
          )

setMethod(f="coords", signature="RadialGradient",
          definition=function(object)
          {
            return(list(cx=object@cx,
                        cy=object@cy,
                        r=object@r,
                        fx=object@fx,
                        fy=object@fy))
          }
          )

setReplaceMethod(f="coords", signature="RadialGradient",
                 definition=function(.Object,value)
                 {
                   ## check
                   if(!is.list(value))
                     stop("'value' must be a 'list()'")
                   check <- names(value) %in% list("cx","cy","r","fx","fy")
                   if(length(check[check == TRUE]) != 5)
                     stop("'value' must at least contains 'cx,cy,r,fx,fy'")

                   ## assign & eop
                   .Object@cx <- value[["cx"]]
                   .Object@cy <- value[["cy"]]
                   .Object@r <- value[["r"]]
                   .Object@fx <- value[["fx"]]
                   .Object@fy <- value[["fy"]]
                   return(.Object)
                 }                 
                 )

setMethod(f=".xml", signature="RadialGradient",
          definition=function(object)
          {
            ## init.
            gradient <- newXMLNode("radialGradient")

            ## core.attributes
            attr <- callNextMethod(object)

            ## attributes
            if(object@cx != "50%") attr <- c(attr, cx=object@cx)
            if(object@cy != "50%") attr <- c(attr, cy=object@cy)
            if(object@r != "50%") attr <- c(attr, r=object@r)
            if(object@fx != object@cx) attr <- c(attr, fx=object@fx)
            if(object@fy != object@cy) attr <- c(attr, fy=object@fy)
            xmlAttrs(gradient) <- attr

            ## add stop items
            svg.stops <- sapply(object@stops, .xml)
            addChildren(gradient,kids=svg.stops)

            ## eop
            return(gradient)
          }
          )

## F A C T O R Y
##----------------------------------------
RadialGradient.factory <- function(...,stops,coords,spread.method,units,transform) {

  ## init. radial gradient
  args <- list("RadialGradient")
  if(!missing(coords)) args <- c(args, coords=list(coords))
  if(!missing(spread.method)) args <- c(args, spread.method=spread.method)
  if(!missing(units)) args <- c(args,units=units)
  if(!missing(transform)) args <- c(args, transform)
  gradient = do.call(new, args)

  ## add stops
  stops(gradient) <- if(imissing(stops)) stops else list(...)
  
  ## eop
  return(gradient)
}
