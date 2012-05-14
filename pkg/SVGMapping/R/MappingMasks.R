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

setClass("MappingMasks",
         representation(fill.angle="numeric"),
         contains="Mapping"
         )

setGenericVerif(name="fillAngle", function(object) { standardGeneric("angle") })
setGenericVerif(name="fillAngle<-", function(.Object,value) { standardGeneric("angle<-") })

setMethod(f="initialize", signature="MappingMasks",
          definition=function(.Object,...)
          {            
            ## super
            .Object <- callNextMethod()

            ## detault init.
            .Object@fill.angle <- -pi/2

            ## eop
            return(.Object)
          }
          )

setMethod(f="fillAngle", signature="MappingMasks",
          definition=function(object)
          {
            return(object@fill.angle)
          }
          )

setReplaceMethod(f="fillAngle", signature="MappingMasks",
                 definition=function(.Object,value)
                 {
                   ## check
                   if(!is.numeric(value))
                     stop("fill angle 'value' must be a numeric")
                   if((value < -pi) || (value > pi))
                     stop("fill angle 'value' must be in the range [-pi,pi]")

                   ## init.
                   .Object@fill.angle <- value
                   return(.Object)
                 }
                 )

setMethod(f="exec", signature="MappingMasks",
          definition=function(.Object, svg)
          {
            .getID <- function(node) {
              attr <- xmlAttrs(node)
              return(node["id"])
            }
            
            .v2mask <- function(v,group) {

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
            callNextMethod()

            ## init.
            ncond <- ncol(.Object@values)
            angle <- .Object@fill.angle
            namedOjbect <- deparse(substitute(.Object))          

            ## check
            if(ncond > 1)
              warning("only values in the first column will be used")

            ## TODO CONTINUE HERE..
            
            ## eop
            assign(namedOjbect, .Object, envir=parent.frame())
            return(invisible(.Object))
          }
          )

## F A C T O R Y
##--------------
