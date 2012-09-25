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

## M A P P I N G   -   M A S K S
## --------------------------------------------------

setGenericVerif <- function(name,y){if(!isGeneric(name)){setGeneric(name,y)}else{}}

#' Mask Mapping Operation
#' 
#' This class creates SVG Masks to hide, partially or completely, target shapes.
#' Input values are used to defined the percentage of the shape to be hidden.
#' Furthermore, an angle parameter can be used to set the direction of the mask.
#' 
#' @name MappingMasks
#' @exportClass "MappingMasks"
#' @aliases MappingMasks-class
setClass("MappingMasks",
         representation(fill.angle="numeric"),
         contains="Mapping"
         )

#' <title already defined>
#' 
#' 
#' 
#' The Mask mapping operation use a linear gradient to partielly hide target 
#' shapes. One can retrieve the fill angle of this gradient using the 
#' \code{fillAngle(object)} method. Conversely, one can set the fill angle of
#' the gradient using the \code{fillAngle(object) <- value} method.
#'
#' @name fillAngle
#' @rdname mappingcolors.gradient-methods
#' @exportMethod fillAngle
#' @docType methods
NULL
#setGenericVerif(name="fillAngle", function(object) { standardGeneric("angle") })

#' <title already defined>
#' 
#' 
#' @name fillAngle<-
#' @rdname mappingcolors.gradient-methods
#' @exportMethod fillAngle<-
#' @docType methods
NULL
#setGenericVerif(name="fillAngle<-", function(.Object,value) { standardGeneric("angle<-") })

setMethod(f="initialize", signature="MappingMasks",
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
            fillAngle(.Object) <- .arg("fill.angle",-pi/2)

            ## eop
            return(.Object)
          }
          )

#' @aliases fillAngle,MappingMasks-method
#' @rdname mappingcolors.gradient-methods
setMethod(f="fillAngle", signature="MappingMasks",
          definition=function(object)
          {
            return(object@fill.angle)
          }
          )

#' @name fillAngle<-
#' @aliases fillAngle<-,MappingMasks-method
#' @rdname mappingcolors.gradient-methods
setReplaceMethod(f="fillAngle", signature="MappingMasks",
                 definition=function(.Object,value)
                 {
                   ## check
                   if(!is.numeric(value))
                     stop("fill angle 'value' must be a numeric")
                   if(!is.vector(value) && ((value < -pi) || (value > pi)))
                     stop("fill angle 'value' must be in the range [-pi,pi]")

                   ## init.
                   .Object@fill.angle <- value
                   return(.Object)
                 }
                 )

#' @aliases exec,MappingMasks-method
#' @rdname mapping.exec-methods
setMethod(f="exec", signature="MappingMasks",
          definition=function(.Object, svg)
          {
            .getID <- function(node) {
              attr <- xmlAttrs(node)
              return(node["id"])
            }
            
            .v2mask <- function(v,group,angle) {

              ## init.
              if(v > 1) v <- 1
              if(v < 0) v <- 0

              ## init. group id
              group.attr <- xmlAttrs(group)
              if(!"id" %in% names(group.attr)) {
                group.attr["id"] <- uid(svg,prefix="")
                xmlAttrs(group) <- group.attr
              }
              group.id <- group.attr["id"]
              
              ## init. (gradient stops)
              stops <- c(GradientStop.factory(0.0,"white",1.0),
                         GradientStop.factory(v,"white",1.0),
                         GradientStop.factory(v,"white",0.0),
                         GradientStop.factory(1.0, "white", 0.0))

              ## init. (gradient coords)
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

              ## build gradient
              gradient <- LinearGradient.factory(stops=stops, coords=coords)
              definitions(svg) <- gradient
              gradient.url <- paste("url(#",id(gradient),")",sep="")

              ## clone 'group' and fix id's (mask)
              group.mask <- duplicate.node(svg,group,"mask")
              group.mask.id <- .getID(group.mask)
              
              ## build alpha-mask, put it in the definitions
              mask <- Mask.factory(group.mask)
              definitions(svg) <- mask
              mask.url <- paste("url(#",id(mask),")",sep="")              

              ## fill the mask with the associated gradient
              svg[group.mask.id,"style::fill"] <- gradient.url

              ## clone 'group' and fix id's (stroke)
              group.stroke <- duplicate.node(svg,group,"stroke")
              group.stroke.id <- .getID(group.stroke)
              svg[group.stroke.id,"style::fill-opacity"] <- 0

              ## mask original group and remove its stroke
              svg[group.id,"mask"] <- mask.url
              svg[group.id,"style::stroke-opacity"] <- 0

              ## create a new group and put the stroke + shape inside
              new.group <- newXMLNode("g")
              replaceNodes(group,new.group)
              addChildren(new.group,group.stroke)
              addChildren(new.group,group)

              ## eop
              return(invisible())
            }
            
            ## call super
            .Object <- callNextMethod(.Object,svg)

            ## init.
            ncond <- ncol(.Object@values)
            namedOjbect <- deparse(substitute(.Object))
            targets <- svg[.Object@targets]

            ## check (#rows)
            if(nrow(targets) != nrow(.Object@values))
              stop("'targets' and 'values' have a different number of rows")
            if(is.vector(.Object@fill.angle) && (nrow(.Object@fill.angle) != nrow(targets)))
              stop("'targets' and 'fill.angle' (vector) have a different number of rows")
            
            ## check
            if(ncond > 1)
              warning("only values in the first column will be used")

            ## main loop: value -> mask
            for(i in 1:nrow(.Object@.values)) {

              ## get value
              if(is.vector(.Object@.values))
                v <- .Object@.values[[i]]
              else
                v <- .Object@.values[i,1]

              ## get target
              target <- targets[[i]]

              ## get filling angle
              if(!is.vector(.Object@fill.angle))
                angle <- .Object@fill.angle
              else
                angle <- .Object@fill.angle[[i]]

              ## forge mask
              .v2mask(v,target,angle)
            }
            
            ## eop
            assign(namedOjbect, .Object, envir=parent.frame())
            return(invisible(.Object))
          }
          )

## F A C T O R Y
##--------------

#' Mapping Mask Factory
#' 
#' This function returns a \emph{default} \code{\link{MappingMasks}} 
#' instance.
#' 
#' Using this factory function, input \emph{values} and \emph{targets} are 
#' initialized in the object. This mapping operation associate default tooltips 
#' to the targets shapes if applied to an SVG template. The tooltip is very
#' simple, with only one line that is filled using input \emph{values}. Thus,
#' the input value data.frame must only contain one column of character strings.
#' 
#' @name MappingMasks.factory
#'   
#' @param data is the input dataset to use for this mapping
#'   
#' @param targets is the list of template node targets to alter. This can be a 
#'   list of SVG nodes identifiers or any node selection expression. By default 
#'   the targets are the row names of the input data variable.
#'   
#' @param fill.angle is the filling angle of the gradient. The default value for
#'   this parameter is -pi/2.
#'   
#' @param trans.function is the transformation function that is applied onto the
#'   data, prior to the color mapping (see the \code{\link{Mapping}} class 
#'   documentation). By default the \emph{identity} function, which do not 
#'   transformed the input data, is assigned to the newly created instance.
#'   
#' @param trans.parameters is the list of parameters values associated with the 
#'   transformation function.
#'   
#' @return a \code{\link{MappingMasks}} object
#'   
#' @export MappingMasks.factory
#'   
#' @examples
#' ## load 'basic-sample.svg' a demo SVG template. 
#' ## template <- SVG.factory(file=system.file("extdata/basic-sample.svg",package="SVGMapping))
#' 
#' ## In this demo template, the top six circles are identified with the 
#' ## 'circle.A' ... 'circle.F' svg ID attributes. We will generate a list that
#' ## contains such identifiers..
#' circles <- paste("circle.",LETTERS[1:6],sep="")
#' 
#' ## Then, we will use the following dummy dataset. This data.frame contains 
#' ## three columns named x,y and z. Row names are the circles identifiers.
#' dummy <- data.frame(x=c(0,0.2,0.4,0.6,0.8,1.0),
#'                    row.names=circles)
#'                    
#' ## First, we color all circles in blue using SVG accessors
#' ## template[circles,"fill:color"] <- "blue"
#' 
#' ## Then, we create a MappingMask instance using the factory
#' ## function. Then, we apply this mapping object to the template, and show
#' ## it in the default browser.
#' mask.map <- MappingMasks.factory(dummy[,"x",drop=FALSE])
#' ## mapping(template,color.map)
#' ## show(template)
MappingMasks.factory <- function(data,targets,
                                 fill.angle,
                                 trans.function, trans.parameters) {

  ## check
  if(missing(data))
    stop("'data' argument is absolutely required")
  
  ## init.
  args <- list("MappingMasks")
  args[["values"]] <- data
  
  ## fill mapping structure
  if(!missing(targets)) 
    args[["targets"]] <- targets
  else    
    args[["targets"]] <- row.names(data)
  if(!missing(fill.angle)) args <- c(args,fill.angle=fill.angle)
  if(!missing(trans.function)) args <- c(args,trans.function=trans.function)
  if(!missing(trans.parameters)) args[["trans.parameters"]] <- trans.parameters
  mapM <- do.call(new,args)
  
  ## eop
  return(mapM)
} 
