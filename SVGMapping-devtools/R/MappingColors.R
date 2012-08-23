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
#'  that \emph{values} are in the range [\code{maprange.min},\code{maprange.max}], 
#'  if  \emph{values} are rounded to these bounds. Second, \emph{colors} must
#'  be given as a list using the following string format: \code{#RRGGBB}. Giving
#'  many intermediate colors will usually result in smoother rendering. At this
#'  point we mention that a linear mapping is performed for the \emph{values} 
#'  to \emph{colors} mapping. If one wants a different mapping function, he must
#'  use a \emph{transformation} function. This function must convert \emph{value}
#'  to a linear scale that will be suitable for the color mapping operation.
#'  
#'  The MappingColors operation can be operate using either \emph{univariate} 
#'  dataset (ie. one numerical value for each \emph{target}) or 
#'  \emph{multivariate} (ie. at leat two values per \emph{target}) datasets. In
#'  the univariate case the the single numerical value is converted into an SVG 
#'  color specification that is applied to the specified attribute. In the 
#'  multivariate case, each numerical value is converted into a color 
#'  specification using the same process previously described. Then, this set
#'  is converted into a linear or radial gradient with a given angle (as 
#'  specified in the mapping colors instance).
#'  
#'  @name MappingColors
#'  @exportClass "MappingColors"
#'  @aliases MappingColors-class
setClass("MappingColors",
         representation(target.attribute="character",
                        map.colors="vector",
                        maprange.min="numeric",
                        maprange.max="numeric",
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
#' @rdname mappingcolors.targetattr-methods
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
#' @rdname mappingcolors.targetattr-methods
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
setGenericVerif(name="mapRange.min", function(object) { standardGeneric("mapRange.min") })

#' <title already defined>
#' 
#' 
#' 
#' The \code{mapRange.min(object) <- value} method sets the lower bound of a 
#' mapping range instance.
#' 
#' @name mapRange.min<-
#' @rdname mappingcolors.maprange-methods
#' @exportMethod mapRange.min<-
#' @docType methods
setGenericVerif(name="mapRange.min<-", function(.Object,value) { standardGeneric("mapRange.min<-") })

#' <title already defined>
#' 
#' 
#' 
#' The \code{mapRange.max(object)} method returns the upper bound of the 
#' mapping range.
#' 
#' @name mapRange.max
#' @rdname mappingcolors.maprange-methods
#' @exportMethod mapRange.max
#' @docType methods
setGenericVerif(name="mapRange.max", function(object) { standardGeneric("mapRange.max") })

#' <title already defined>
#' 
#' 
#' 
#' The \code{mapRange.max(object) <- value} method sets the upper bound of a 
#' mapping range instance.
#' 
#' @name mapRange.max<-
#' @rdname mappingcolors.maprange-methods
#' @exportMethod mapRange.max<- 
#' @docType methods
setGenericVerif(name="mapRange.max<-", function(.Object,value) { standardGeneric("mapRange.max<-") })

#' <title already defined>
#' 
#' 
#' 
#' The \code{mapRange(object)} methods returns, as a vector, the lower and upper 
#' bounds of the mapping range.
#' 
#' @name mapRange
#' @rdname mappingcolors.maprange-methods
#' @exportMethod mapRange
#' @docType methods
setGenericVerif(name="mapRange", function(object) { standardGeneric("mapRange") })

#' <title already defined>
#' 
#' 
#' 
#' The \code{mapRange(object) <- value} methods sets the lower and upper bounds,
#' stored in the vector \emph{value}, of a mapping colors instance. 
#' 
#' @name mapRange<-
#' @rdname mappingcolors.maprange-methods
#' @exportMethod mapRange<-
#' @docType methods
setGenericVerif(name="mapRange<-", function(.Object,value) { standardGeneric("mapRange<-") })

#' Gradient Specification Accessors
#' 
#' When using multivariate input values (see the \code{\link{MappingColors}} 
#' class definition) the mapping operation results in the assignment of a 
#' gradient to the target style attribute. The methods described here allow to
#' specify the type of gradient (either linear or radial) and the filling 
#' angle.
#' 
#' The \code{gradientType(object)} method returns the type of gradient, 
#' either \emph{linear} or \emph{radial}, generated.
#' 
#' @name gradientType
#' @rdname mappingcolors.gradient-methods
#' @exportMethod gradientType
#' @docType methods
setGenericVerif(name="gradientType", function(object) { standardGeneric("gradientType") })

#' <title already defined>
#' 
#' 
#' 
#' The \code{gradientType(object) <- value} method sets the type of gradient, 
#' here \emph{value} must be either \code{'linear'} or \code{'radial'}, to 
#' generate.
#' 
#' @name gradientType<-
#' @rdname mappingcolors.gradient-methods
#' @exportMethod gradientType<-
#' @docType methods 
setGenericVerif(name="gradientType<-", function(.Object,value) { standardGeneric("gradientType<-") })

#' <title already defined>
#' 
#' 
#' 
#' The \code{fillAngle(object)} method returns the filling angle of the 
#' gradient.
#' 
#' @name fillAngle
#' @rdname mappingcolors.gradient-methods
#' @exportMethod fillAngle
#' @docType methods
setGenericVerif(name="fillAngle", function(object) { standardGeneric("fillAngle") })

#' <title already defined>
#' 
#' 
#' 
#' The \code{fillAngle(object) <- value} method sets the filling angle value 
#' of the gradient to generate.
#' 
#' @name fillAngle<-
#' @rdname mappingcolors.gradient-methods
#' @exportMethod fillAngle<-
#' @docType methods
setGenericVerif(name="fillAngle<-", function(.Object,value) { standardGeneric("fillAngle<-") })

#' <title already defined>
#' 
#' 
#' 
#' The \code{exec(object,svg)} method is called to process the input \emph{svg}
#' document with the current \emph{object} mapping colors instance.
#' 
#'  @name exec
#'  @rdname mapping.exec-methods
#'  @exportMethod exec
#'  @docType methods 
setGenericVerif(name="exec", function(.Object,svg) { standardGeneric("exec") })

setMethod(f="initialize", signature="MappingColors",
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
            
            ## detault init.
            targetAttribute(.Object) <- .arg("target.attribute",character())
            mapColors(.Object) <- .arg("map.colors", vector())
            mapRange(.Object) <- .arg("map.range",c(0,1))
            mapRange.min(.Object) <- .arg("map.range.min",0)
            mapRange.max(.Object) <- .arg("map.range.max",1)
            gradientType(.Object) <- .arg("gradient.type","linear")
            fillAngle(.Object) <- .arg("fill.angle", 0)

            ## eop
            return(.Object)
          }
          )


#' @aliases targetAttribute,MappingColors-method
#' @rdname mappingcolors.targetattr-methods
setMethod(f="targetAttribute", signature="MappingColors",
          definition=function(object)
          {
            return(object@target.attribute)
          }
          )

#' @name targetAttribute<-
#' @aliases targetAttribute<-,MappingColors-method
#' @rdname mappingcolors.targetattr-methods
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

#' @aliases mapColors,MappingColors-method
#' @rdname mappingcolors.mapcolors-methods
setMethod(f="mapColors", signature="MappingColors",
          definition=function(object)
          {
            return(object@map.colors)
          }
          )

#' @name mapColors<-
#' @aliases mapColors<-,MappingColors-method
#' @rdname mappingcolors.mapcolors-methods
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

#' @aliases mapRange.min,MappingColors-method
#' @rdname mappingcolors.maprange-methods
setMethod(f="mapRange.min", signature="MappingColors",
          definition=function(object)
          {
            return(object@maprange.min)
          }
          )

#' @name mapRange.min<-
#' @aliases mapRange.min<-,MappingColors-method
#' @rdname mappingcolors.maprange-methods
setReplaceMethod(f="mapRange.min", signature="MappingColors",
                 definition=function(.Object,value)
                 {
                   ## check
                   if(!is.numeric(value))
                     stop("Mapping Range minimum 'value' must be numeric")

                   ## init.
                   .Object@maprange.min <- value
                   return(.Object)
                 }
                 )

#' @aliases mapRange.max,MappingColors-method
#' @rdname mappingcolors.maprange-methods
setMethod(f="mapRange.max", signature="MappingColors",
          definition=function(object)
          {
            return(object@maprange.max)
          }
          )

#' @name mapRange.max<-
#' @aliases mapRange.max<-,MappingColors-method
#' @rdname mappingcolors.maprange-methods
setReplaceMethod(f="mapRange.max", signature="MappingColors",
                 definition=function(.Object,value)
                 {
                   ## check
                   if(!is.numeric(value))
                     stop("Mapping Range maximum 'value' must be numeric")

                   ## init.
                   .Object@maprange.max <- value
                   return(.Object)
                 }
                 )

#' @aliases mapRange,MappingColors-method
#' @rdname mappingcolors.maprange-methods
setMethod(f="mapRange", signature="MappingColors",
          definition=function(object)
          {
            return(c(object@maprange.min,object@maprange.max))
          }
          )

#' @name mapRange<-
#' @aliases mapRange<-,MappingColors-method
#' @rdname mappingcolors.maprange-methods
setReplaceMethod(f="mapRange", signature="MappingColors",
                 definition=function(.Object,value)
                 {
                   ## check
                   if(!is.vector(value) && (length(value) != 2))
                     stop("Mapping Range 'value' must be a vector such as: c(min,max)")

                   ## init.
                   value <- unlist(value)
                   mapRange.min(.Object) <- value[[1]]
                   mapRange.max(.Object) <- value[[2]]
                   return(.Object)
                 }
                 )

#' @aliases gradientType,MappingColors-method
#' @rdname mappingcolors.gradient-methods
setMethod(f="gradientType", signature="MappingColors",
          definition=function(object)
          {
            return(object@gradient.type)
          }
          )

#' @name gradientType<-
#' @aliases gradientType<-,MappingColors-method
#' @rdname mappingcolors.gradient-methods
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

#' @aliases fillAngle,MappingColors-method
#' @rdname mappingcolors.gradient-methods
setMethod(f="fillAngle", signature="MappingColors",
          definition=function(object)
          {
            return(object@fill.angle)
          }
          )

#' @name fillAngle<-
#' @aliases fillAngle<-,MappingColors-method
#' @rdname mappingcolors.gradient-methods
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

#' @aliases exec,MappingColors-method
#' @rdname mapping.exec-methods
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
            cmin <- .Object@maprange.min
            cscale <- (clen-1) / (.Object@maprange.max - cmin)
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

## generated using the colorRamps package and 'cat(deparse(blue2red(50)))'
.default_mapping_colors <- c("#0000FF", "#000BFF", "#0015FF", "#0020FF", 
                             "#002BFF", "#0035FF", "#0040FF", "#004AFF", 
                             "#0055FF", "#0060FF", "#006AFF", "#0075FF",  
                             "#0080FF", "#008AFF", "#0095FF", "#009FFF", 
                             "#00AAFF", "#00B5FF", "#00BFFF", "#00CAFF", 
                             "#00D4FF", "#00DFFF", "#00EAFF", "#00F4FF",  
                             "#00FFFF", "#FFF500", "#FFEB00", "#FFE000", 
                             "#FFD600", "#FFCC00", "#FFC200", "#FFB800", 
                             "#FFAD00", "#FFA300", "#FF9900", "#FF8F00",  
                             "#FF8500", "#FF7A00", "#FF7000", "#FF6600", 
                             "#FF5C00", "#FF5200", "#FF4700", "#FF3D00", 
                             "#FF3300", "#FF2900", "#FF1F00", "#FF1400",  
                             "#FF0A00", "#FF0000")

#' Mapping Colors Factory
#' 
#' This function returns a \code{\link{MappingColors}} instance that will change
#' the filling and stroke colors of a template according to input values.
#' 
#' Once defined a \code{MappingColors} instance can be applied to the template 
#' using the \code{mapping} function.
#' 
#' @name MappingColors.factory
#'   
#' @param data is the input datasets to use for this mapping
#'   
#' @param targets is the list of template node targets to alter. This can be a 
#'   list of SVG nodes identifiers or any node selection expression. By default 
#'   the targets are the row names of the input data variable.
#'   
#' @param target.attribute
#'   
#' @param map.colors to use by the mapping operations. The default value is a 
#'   color map that runs from blue to red, with 50 intermediates levels. This 
#'   color map has been generated using the \code{blue2red} function from the 
#'   \strong{colorRamps} package.
#'   
#' @param map.range is a vector that contains the bounds of the linear 
#'   transformation used to convert numerical values into colors. The default 
#'   range value is \eqn{[0,1]}{[0,1]}. Outliers are converted to the closest 
#'   bound of this range. For more details about map ranges see the 
#'   \code{\link{mapRange}} method.
#'   
#' @param gradient.type defines the kind of gradient (\emph{linear} or 
#'   \emph{radial}) to use when dealing with multivariate input datasets. By 
#'   default a \emph{linear} gradient is selected.
#'   
#' @param fill.angle is the filling angle of the gradient. The default value for
#'   this parameter is 0.
#'   
#' @param fn is the transformation function that is applied onto the data, prior
#'   to the color mapping (see the \code{\link{Mapping}} class documentation). 
#'   By default the \emph{identity} function, which do not transformed the input
#'   data, is assigned to the newly created instance.
#'   
#' @param fn.parameters is the list of parameters values associated with the 
#'   transformation function.
#'   
#' @return a \code{\link{MappingColors}} object
#'  
#' @export MappingColors.factory 
MappingColors.factory <- function(data,targets=rownames(data),
                                  target.attribute="style::fill",
                                  map.colors=microarrayColors,map.range=c(-2,2),
                                  gradient.type="linear", fill.angle=0,
                                  fn="Identity", fn.parameters=list()) {
  ## init.
  mapC <- new("MappingColors")

  ## fill mapping structure
  values(mapC) <- data
  targets(mapC) <- targets
  mapColors(mapC) <- colors
  mapRange(mapC) <- colors.range
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
  mapRange(mapC) <- colors.range
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
  mapRange(mapC) <- colors.range
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
