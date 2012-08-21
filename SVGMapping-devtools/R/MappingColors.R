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

## M A P P I N G   -   C O L O R S
## --------------------------------------------------

setGenericVerif <- function(name,y){if(!isGeneric(name)){setGeneric(name,y)}else{}}

#' Colors Mapping Operation
#' 
#' This class allows to convert numerical values into colors and use them
#' to alter the target shapes. For example, this class can be use to change 
#' the fill or the stroke colors of shapes.
#' 
#'  The conversion between \emph{values} and \emph{colors} use a linear mapping. 
#'  To perform this operation we make several assumptions. First, we consider 
#'  that \emph{values} are in the range [\code{colrange.min},\code{colrange.max}], 
#'  if  \emph{values} are rounded to these bounds. Second, \emph{colors} must
#'  be given as a list using the following string format: \code{#RRGGBB}. Giving
#'  many intermediate colors will usually result in smoother rendering. At this
#'  point we mention that a linear mapping is performed for the \emph{values} 
#'  to \emph{colors} mapping. If one wants a different mapping function, he must
#'  use a \emph{transformation} function. This function must convert \emph{value}
#'  to a linear scale that will be suitable for the color mapping operation.
#'  
#'  
#'  
#'  @exportClass "MappingColors"
#'  @aliases MappingColors-class
setClass("MappingColors",
         representation(target.attribute="character",
                        map.colors="vector",
                        colrange.min="numeric",
                        colrange.max="numeric",
                        gradient.type="character",
                        fill.angle="numeric"
                        ),
         contains="Mapping"
         )

#' Target Style Attribute Accessors
#' 
#' Given the set of \emph{targets}, to apply the \code{MappingColors} operation
#' one as to provide the name of the \emph{style} attribute to change. The 
#' methods described here allow to specify this attribute.
#' 
#' The \code{targetAttribute(object)} method retrieves the current target style 
#' attribute value.
#' 
#' @name targetAttribute
#' 
#' @param object the mapping colors instance object
#' 
#' @return a CSS style attribute name
#' 
#' @rdname mappingcolors.styleattr-methods
#' @exportMethod targetAttribute
#' @docType methods
setGenericVerif(name="targetAttribute", function(object) { standardGeneric("targetAttribute") })

#' <title already defined>
#' 
#' 
#' 
#' The \code{targetAttribute(object) <- value} method sets the target style 
#' attribute of the \code{object} mapping instance
#' 
#' @rdname mappingcolors.styleattr-methods
#' @exportMethod targetAttribute<-
#' @docType methods
setGenericVerif(name="targetAttribute<-", function(.Object,value) { standardGeneric("targetAttribute<-") })

#' Mapping Colors vector accessors
#' 
#' These methods allow to get/set the list of colors that will be used by
#' the \code{MappingColors} operation. 
#' 
#' The list of colors must be a vector of single color definition as a string. 
#' Each color must be compliant with the SVG color definition standard. 
#' A good practice is to give color as string using the \code{#RRGGBB} format.
#' 
#' The \code{mapColors(object)} method retrieve the current list of mapping 
#' colors.
#' 
#' @name mapColors
#' 
#' @param object the mapping colors instance object
#' 
#' @return a vector of color definition (each as a string)
#' 
#' @rdname mappingcolors.mapcolors-methods
#' @exportMethod mapColors
#' @docType methods
setGenericVerif(name="mapColors", function(object) { standardGeneric("mapColors") })

#' <title already defined>
#' 
#' 
#' 
#' The \code{mapColors(objec) <- value} method sets the list of mapping colors
#' of the \code{object} mapping instance.
#' 
#' @rdname mappingcolors.mapcolors-methods
#' @exportMethod mapColors<-
#' @docType methods
setGenericVerif(name="mapColors<-", function(.Object,value) { standardGeneric("mapColors<-") })

#' Mapping Input Range Accessors
#' 
#' \code{MappingColors} operation consists of a linear mapping between 
#' \emph{values}, in a given range, and a list of colors. The following methods
#' let get/set the input range of the (linear) mapping.
#' 
#' The mapping range is given as a numeric interval with lower and upper bounds. 
#' If input \emph{values} are not included in the mapping range, then they are
#' automatically bounded to this range. Thus, values outside of the input range
#' are mapped to the first or last mapping colors. 
#' 
#' The \code{mapRange.min(object)} method returns the lower bound of the 
#' mapping range.
#' 
#' @name mapRange.min
#' 
#' @param object the mapping colors instance object
#' 
#' @rdname mappingcolors.maprange-methods
#' @exportMethod mapRange.min
#' @docType methods
setGenericVerif(name="mapRange.min", function(object) { standardGeneric("colRange.min") })

#' <title already defined>
#' 
#' 
#' 
#' The \code{mapRange.min(object) <- value} method sets the lower bound of a 
#' mapping range instance.
#' 
#' @rdname mappingcolors.maprange-methods
#' @exportMethod mapRange.min<-
#' @docType methods
setGenericVerif(name="mapRange.min<-", function(.Object,value) { standardGeneric("colRange.min<-") })

#' <title already defined>
#' 
#' 
#' 
#' The \code{mapRange.max(object)} method returns the upper bound of the 
#' mapping range.
#' 
#' @rdname mappingcolors.maprange-methods
#' @exportMethod mapRange.max
#' @docType methods
setGenericVerif(name="mapRange.max", function(object) { standardGeneric("colRange.max") })

#' <title already defined>
#' 
#' 
#' 
#' The \code{mapRange.max(object) <- value} method sets the upper bound of a 
#' mapping range instance.
#' 
#' @rdname mappingcolors.maprange-methods
#' @exportMethod mapRange.max<- 
#' @docType methods
setGenericVerif(name="mapRange.max<-", function(.Object,value) { standardGeneric("colRange.max<-") })

#' <title already defined>
#' 
#' 
#' 
#' The \code{mapRange(object)} methods returns, as a vector, the lower and upper 
#' bounds of the mapping range.
#' 
#' @rdname mappingcolors.maprange-methods
#' @exportMethod mapRange
#' @docType methods
setGenericVerif(name="mapRange", function(object) { standardGeneric("colRange") })

#' <title already defined>
#' 
#' 
#' 
#' The \code{mapRange(object) <- value} methods sets the lower and upper bounds,
#' stored in the vector \emph{value}, of a mapping colors instance. 
#' 
#' @rdname mappingcolors.maprange-methods
#' @exportMethod mapRange<-
#' @docType methods
setGenericVerif(name="mapRange<-", function(.Object,value) { standardGeneric("colRange<-") })

setGenericVerif(name="gradientType", function(object) { standardGeneric("gradientType") })
setGenericVerif(name="gradientType<-", function(.Object,value) { standardGeneric("gradientType<-") })
setGenericVerif(name="fillAngle", function(object) { standardGeneric("fillAngle") })
setGenericVerif(name="fillAngle<-", function(.Object,value) { standardGeneric("fillAngle<-") })
setGenericVerif(name="exec", function(.Object,svg) { standardGeneric("exec") })

setMethod(f="initialize", signature="MappingColors",
          definition=function(.Object,...)
          {            
            ## super
            .Object <- callNextMethod()

            ## detault init.
            .Object@map.colors <- vector()
            .Object@colrange.min <- 0
            .Object@colrange.max <- 1
            .Object@target.attribute <- character()
            .Object@fill.angle <- 0
            .Object@gradient.type <- "linear"

            ## eop
            return(.Object)
          }
          )

setMethod(f="targetAttribute", signature="MappingColors",
          definition=function(object)
          {
            return(object@target.attribute)
          }
          )

setReplaceMethod(f="targetAttribute", signature="MappingColors",
                 definition=function(.Object,value)
                 {
                   ## check
                   if(!is.character(value))
                     stop("Target attribute 'value' must be a string")

                   ## init.
                   .Object@target.attribute <- value
                   return(.Object)
                 }
                 )

setMethod(f="mapColors", signature="MappingColors",
          definition=function(object)
          {
            return(object@map.colors)
          }
          )

setReplaceMethod(f="mapColors", signature="MappingColors",
                 definition=function(.Object,value)
                 {
                   ## check
                   if(!is.vector(value))
                     stop("Colors 'value' must be a vector")

                   ## init.
                   .Object@map.colors <- unlist(value)
                   return(.Object)
                 }
                 )

setMethod(f="colRange.min", signature="MappingColors",
          definition=function(object)
          {
            return(object@colrange.min)
          }
          )

setReplaceMethod(f="colRange.min", signature="MappingColors",
                 definition=function(.Object,value)
                 {
                   ## check
                   if(!is.numeric(value))
                     stop("Minimum Color Range 'value' must be numeric")

                   ## init.
                   .Object@colrange.min <- value
                   return(.Object)
                 }
                 )

setMethod(f="colRange.max", signature="MappingColors",
          definition=function(object)
          {
            return(object@colrange.max)
          }
          )

setReplaceMethod(f="colRange.max", signature="MappingColors",
                 definition=function(.Object,value)
                 {
                   ## check
                   if(!is.numeric(value))
                     stop("Maximum Color Range 'value' must be numeric")

                   ## init.
                   .Object@colrange.max <- value
                   return(.Object)
                 }
                 )

setMethod(f="colRange", signature="MappingColors",
          definition=function(object)
          {
            return(c(object@colrange.min,object@colrange.max))
          }
          )

setReplaceMethod(f="colRange", signature="MappingColors",
                 definition=function(.Object,value)
                 {
                   ## check
                   if(!is.vector(value) && (length(value) != 2))
                     stop("Colors Range 'value' must be a vector such as: c(min,max)")

                   ## init.
                   value <- unlist(value)
                   .Object@colrange.min <- value[[1]]
                   .Object@colrange.max <- value[[2]]
                   return(.Object)
                 }
                 )

setMethod(f="gradientType", signature="MappingColors",
          definition=function(object)
          {
            return(object@gradient.type)
          }
          )

setReplaceMethod(f="gradientType", signature="MappingColors",
                 definition=function(.Object,value)
                 {
                   ## check
                   if(!is.character(value) ||
                      !(tolower(value) %in% list("linear", "radial")))
                     stop("'value' must be a character string, either 'linear' or 'radial'")

                   ## eop
                   .Object@gradient.type <- tolower(value)
                   return(.Object)
                 }
                 )

setMethod(f="fillAngle", signature="MappingColors",
          definition=function(object)
          {
            return(object@fill.angle)
          }
          )

setReplaceMethod(f="fillAngle", signature="MappingColors",
                 definition=function(.Object,value)
                 {
                   ## -- TODO: more check about 'vectors'
                   ## --       or move concept to the Roadmap
                   if(!is.numeric(value))
                     stop("'value' must be a numeric value")

                   ## eop
                   .Object@fill.angle <- value
                   return(.Object)
                 }
                 )

setMethod(f="exec", signature="MappingColors",
          definition=function(.Object, svg)
          {
            ## local
            .v2col <- function(v) {
              v <- (v-cmin)*cscale+1
              v <- if(v < 1) 1 else v
              v <- if(v > clen) clen else v
              return(colors[as.integer(round(v))])
            }

            .v2grad <- function(v) {
           
              ## compute colors & create gradient stops
              vcolors <- sapply(v, .v2col)
              stops <- list()
              for(i in 1:length(vcolors)) {
                s1 <- GradientStop.factory((i-1)/ncond, vcolors[[i]])
                s2 <- GradientStop.factory((i)/ncond, vcolors[[i]])
                stops <- c(stops,s1,s2)
              }
              
              ## gradient init.
              if((gtype == "linear") && (angle != 0)) {
                x <- cos(angle)
                y <- sin(angle)
                if (x < 0) {
                  x1 <- -x
                  x2 <- 0
                } else {
                  x1 <- 0
                  x2 <- x
                }
                if (y < 0) {
                  y1 <- -y
                  y2 <- 0
                } else {
                  y1 <- 0
                  y2 <- y
                }
                coords <- list(x1 = paste(round(x1*100), "%", sep=""),
                               x2 = paste(round(x2*100), "%", sep=""),
                               y1 = paste(round(y1*100), "%", sep=""),
                               y2 = paste(round(y2*100), "%", sep=""))
                gradient <- LinearGradient.factory(stops=stops, coords=coords)
              }
              else if(gtype == "linear")
                gradient <- LinearGradient.factory(stops=stops)
              else
                gradient <- RadialGradient.factory(stops=stops)
                
              ## put gradient in def.
              definitions(svg) <- gradient
              return(gradient)            
            }

            ##------------------------------
            
            ## call super
            callNextMethod()

            ## init.
            colors <- .Object@map.colors
            clen <- length(colors)
            cmin <- .Object@colrange.min
            cscale <- (clen-1) / (.Object@colrange.max - cmin)
            ncond <- ncol(.Object@values)
            if(is.null(ncond)) ncond <- 1
            angle <- .Object@fill.angle
            gtype <- .Object@gradient.type
            namedOjbect <- deparse(substitute(.Object))          

            ## 1) Single Value (one-color) mode
            if(ncond < 2) {
              
              ## transform fn(values) -> colors
              .Object@.values <- sapply(.Object@.values, .v2col)
              
              ## map colors on the template attribute
              svg[.Object@targets,.Object@target.attribute] <- .Object@.values
            }
            
            ## 2) Multiple Values (gradients) mode
            else {

              ## transform fn(values) -> gradients
              .Object@.values <- apply(.Object@.values, 1, .v2grad)

              ## map gradient on the template attribute
              svg[.Object@targets, .Object@target.attribute] <- sapply(.Object@.values, URL)              
            }
            
            ## eop
            assign(namedOjbect, .Object, envir=parent.frame())
            return(invisible(.Object))
          }
          )

## F A C T O R Y
##--------------
MappingColors.factory <- function(data,targets=rownames(data),
                                  colors=microarrayColors,colors.range=c(-2,2),
                                  attribute="style::fill",
                                  gradient.type="linear", fill.angle=0,
                                  fn="Identity", fn.parameters=list()) {
  ## init.
  mapC <- new("MappingColors")

  ## fill mapping structure
  values(mapC) <- data
  targets(mapC) <- targets
  mapColors(mapC) <- colors
  colRange(mapC) <- colors.range
  targetAttribute(mapC) <- attribute
  gradientType(mapC) <- gradient.type
  fillAngle(mapC) <- fill.angle
  if(missing(fn.parameters))
    mapC <- setFunction(mapC,fn)
  else
    mapC <- setFunction(mapC,fn,fn.parameters)
  
  ## eop
  return(mapC)
}

MappingFillColors.factory <- function(data,targets=rownames(data),
                                      colors=microarrayColors,colors.range=c(-2,2),
                                      gradient.type="linear", fill.angle=0,
                                      fn="Identity", fn.parameters=list()) {
  ## init.
  mapC <- new("MappingColors")

  ## fill mapping structure
  values(mapC) <- data
  targets(mapC) <- targets
  mapColors(mapC) <- colors
  colRange(mapC) <- colors.range
  targetAttribute(mapC) <- "style::fill"
  gradientType(mapC) <- gradient.type
  fillAngle(mapC) <- fill.angle
  if(missing(fn.parameters))
    mapC <- setFunction(mapC,fn)
  else
    mapC <- setFunction(mapC,fn,fn.parameters)
  
  ## eop
  return(mapC)
  
}

MappingStrokeColors.factory <- function(data,targets=rownames(data),
                                        colors=microarrayColors,colors.range=c(-2,2),
                                        gradient.type="linear", fill.angle=0,
                                        fn="Identity", fn.parameters=list()) {
  ## init.
  mapC <- new("MappingColors")

  ## fill mapping structure
  values(mapC) <- data
  targets(mapC) <- targets
  mapColors(mapC) <- colors
  colRange(mapC) <- colors.range
  targetAttribute(mapC) <- "style::stroke"
  gradientType(mapC) <- gradient.type
  fillAngle(mapC) <- fill.angle
  if(missing(fn.parameters))
    mapC <- setFunction(mapC,fn)
  else
    mapC <- setFunction(mapC,fn,fn.parameters)
  
  ## eop
  return(mapC)
  
}
