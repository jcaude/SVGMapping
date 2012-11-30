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

## M A S K
## --------------------------------------------------

setGenericVerif <- function(name,y){if(!isGeneric(name)){setGeneric(name,y)}else{}}

#' Mask class
#' 
#' Using a Mask, you can specify that any other graphics object or ‘g’ element
#' can be used as an alpha mask for compositing the current object into the
#' background.
#' 
#' @name Mask
#' @exportClass "Mask"
#' @aliases Mask-class
setClass("Mask",
         representation(mask.units="character",
                        mask.content.units="character",
                        mask.content="ANY"),
         contains=c("SVGNode","Rectangle")
         )

#' Mask Units Accessors
#' 
#' These methods defines the coordinate system for attributes ‘x’, ‘y’, ‘width’ 
#' and ‘height’.
#' 
#' The \code{maskUnits(object)} method retrieves the current mask untis value of
#' \emph{object}.
#' 
#' @note 
#' If it is equal to \emph{userSpaceOnUse} (by default) then ‘x’, ‘y’,
#' ‘width’ and ‘height’ represent values in the current user coordinate system
#' in place at the time when the ‘mask’ element is referenced (\emph{ie}, the
#' user coordinate system for the element referencing the ‘mask’ element via the
#' ‘mask’ property).
#' 
#' If it is equal to \emph{objectBoundingBox} then ‘x’, ‘y’, ‘width’ and 
#' ‘height’ represent fractions or percentages of the bounding box of the 
#' element to which the mask is applied. (See Object bounding box units.)
#' 
#' @name maskUnits
#'   
#' @param object is the Mask object
#' 
#' @return the mask units value as a string
#' 
#' @rdname mask.maskunits-methods
#' @exportMethod maskUnits
#' @docType methods
setGeneric(name="maskUnits", function(object) { standardGeneric("maskUnits") })

#' <title already defined>
#' 
#' 
#' The \code{maskUnits(object) <- value} sets the mask units of the
#' \emph{object} Mask instance.
#' 
#' @name maskUnits<- 
#' @rdname mask.maskunits-methods
#' @exportMethod maskUnits<-
#' @docType methods
setGeneric(name="maskUnits<-", function(.Object, value) { standardGeneric("maskUnits<-") })

#' Mask Content Units Accessors
#' 
#' These methods define the coordinate system for the contents of the mask.
#' 
#' The \code{maskContentUnits(object)} method retrieves the current mask content
#' untis value of \emph{object}.
#' 
#' @note
#' If it is equal to \emph{userSpaceOnUse} (by default), then the user
#' coordinate system for the contents of the ‘mask’ element is the current user
#' coordinate system in place at the time when the ‘mask’ element is referenced
#' (\emph{ie}, the user coordinate system for the element referencing the ‘mask’
#' element via the ‘mask’ property).
#' 
#' If it is equal to \emph{objectBoundingBox}, then the user coordinate system
#' for the contents of the ‘mask’ is established using the bounding box of the
#' element to which the mask is applied.
#' 
#' @name maskContentUnits
#' 
#' @param object is the Mask object
#' 
#' @return the mask content units value
#' 
#' @rdname mask.maskcontentunits-methods
#' @exportMethod maskContentUnits
#' @docType methods
setGeneric(name="maskContentUnits", function(object) { standardGeneric("maskContentUnits") })

#' <title already defined>
#' 
#' 
#' The  \code{maskContentUnits(object) <- value} sets the mask content units of
#' the \emph{object} Mask instance.
#' 
#' @name maskContentUnits<-
#' @rdname mask.maskcontentunits-methods
#' @exportMethod maskContentUnits<-
#' @docType methods
setGeneric(name="maskContentUnits<-", function(.Object, value) { standardGeneric("maskContentUnits<-") })

#' Mask Content Accessors
#' 
#' These methods give access of the content (graphical elements) of the mask
#' 
#' The \code{maskContent(object)} method returns the content of the mask as an 
#' XMLInternalNode
#' 
#' @note
#' The child elements of the \emph{mask} are rendered into an offscreen image 
#' which has been initialized to transparent black. Any graphical object which 
#' uses/references the given \emph{mask} element will be painted onto the
#' background through the mask, thus completely or partially masking out parts
#' of the graphical object.
#' 
#' @name maskContent
#' 
#' @param object is the Mask instance
#' 
#' @return an XMLInternalNode
#' 
#' @rdname mask.maskcontent-methods
#' @exportMethod maskContent
#' @docType methods
setGeneric(name="maskContent", function(object) { standardGeneric("maskContent") })

#' <title already defined>
#' 
#' 
#' 
#' The \code{maskContent(object) <- value} method set the content of the mask. Here
#' \emph{value} is an XMLInternalNode.
#' 
#' @name maskContent<-
#' 
#' @rdname mask.maskcontent-methods
#' @exportMethod maskContent<-
#' @docType methods
setGeneric(name="maskContent<-", function(.Object, value) { standardGeneric("maskContent<-") })

setMethod(f="initialize", signature="Mask",
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

            ## super (SVGNode) 
            .Object <- callNextMethod(.Object,...)

            ## super (Rectangle) -- note: default overiden below 
            .Object <- .callMethod("initialize","Rectangle",.Object,...)
            
            ## get args
            args <- list(...)
            args.names <- names(args)
            if(is.null(args.names)) args.names <- list()

            ## rectangle init with mask defaults
            x(.Object) <- .arg("x",SVGCoord.factory(-10,"%"))
            y(.Object) <- .arg("y",SVGCoord.factory(-10,"%"))
            width(.Object) <- .arg("width",SVGLength.factory(120,"%"))
            height(.Object) <- .arg("height",SVGLength.factory(120,"%"))
            
            ## default init.
            maskUnits(.Object) <- .arg("mask.units","objectBoundingBox")
            maskContentUnits(.Object) <- .arg("mask.content.units","userSpaceOnUse")
            maskContent(.Object) <- .arg("mask.content",NULL)

            ## eop
            return(.Object)
          }
          )

#' @rdname mask.maskunits-methods
#' @aliases maskUnits,Mask-method
setMethod(f="maskUnits", signature="Mask",
          definition=function(object)
          {
            return(object@mask.units)
          }
          )

#' @name maskUnits<-,Mask-method
#' @rdname mask.maskunits-methods
#' @aliases maskUnits<-,Mask-method 
setReplaceMethod(f="maskUnits", signature="Mask",
                 definition=function(.Object, value)
                 {
                   ## check
                   if(!is.character(value))
                     stop("'value' must be a character string")
                   if(!(value %in% list("userSpaceOnUse", "objectBoundingBox")))
                     stop("'value' must be either 'userSpaceOnUse' or 'objectBoundingBox'")

                   ## assign & eop
                   .Object@mask.units <- value
                   return(.Object)
                 }
                 )

#' @rdname mask.maskcontentunits-methods
#' @aliases maskContentUnits,Mask-method
setMethod(f="maskContentUnits", signature="Mask",
          definition=function(object)
          {
            return(object@mask.content.units)
          }
          )

#' @name maskContentUnits<-,Mask-method
#' @rdname mask.maskcontentunits-methods
#' @aliases maskContentUnits<-,Mask-method 
setReplaceMethod(f="maskContentUnits", signature="Mask",
                 definition=function(.Object, value)
                 {
                   ## check
                   if(!is.character(value))
                     stop("'value' must be a character string")
                   if(!(value %in% list("userSpaceOnUse", "objectBoundingBox")))
                     stop("'value' must be either 'userSpaceOnUse' or 'objectBoundingBox'")

                   ## assign & eop
                   .Object@mask.content.units <- value
                   return(.Object)
                 }
)
 
#' @rdname mask.maskcontent-methods
#' @aliases maskContent,Mask-method
setMethod(f="maskContent", signature="Mask",
          definition=function(object)
          {
            return(object@mask.content)
          }
)

#' @name maskContent<-,Mask-method
#' @rdname mask.maskcontent-methods
#' @aliases maskContent<-,Mask-method 
setReplaceMethod(f="maskContent", signature="Mask",
                 definition=function(.Object,value)
                 {
                   ## check
                   if(!is.object(value) 
                      && !(is(value,"XMLInternalNode") || is.null(value))) 
                     stop("'value' must be a XMLInternalNode object or NULL")

                   ## assign & eop
                   .Object@mask.content <- value
                   return(.Object)
                 }
)

#' @name .xml,Mask-method
#' @rdname svgnode.xml-methods
#' @aliases .xml,Mask-method
setMethod(f=".xml", signature="Mask",
          definition=function(object)
          {
            ## init.
            mask <- newXMLNode("mask")
            
            ## super
            attr <- callNextMethod(object)

            ## return a core attribute list
            if(maskUnits(object) != "objectBoundingBox")
              attr <- c(attr,maskUnits=maskUnits(object))
            if(maskContentUnits(object) != "userSpaceOnUse")
              attr <- c(attr,maskContentUnits=maskContentUnits(object))
            if(x(object) != SVGCoord.factory(-10,"%"))
              attr <- c(attr,x=as.character(x(object)))
            if(y(object) != SVGCoord.factory(-10,"%"))
              attr <- c(attr,y=as.character(y(object)))
            if(width(object) != SVGLength.factory(120,"%"))
              attr <- c(attr,width=as.character(width(object)))
            if(height(object) != SVGLength.factory(120,"%"))
              attr <- c(attr,height=as.character(height(object)))
            xmlAttrs(mask) <- attr
            
            ## eop
            addChildren(mask,kids=list(maskContent(object)))
            return(mask)
          }
)

## F A C T O R Y
##----------------------------------------

#' Mask Factory
#' 
#' This function returns a new \code{\link{Mask}} object.
#' 
#' \code{Mask} objects are used to specify Masks that are used to alter a shape 
#' or a group of shapes.
#' 
#' @name Mask.factory
#'   
#' @param x the x-axis location as an \code{\link{SVGCoord}} value
#' @param y the y-axis location as an \code{\link{SVGCoord}} value
#' @param width the width length as an \code{\link{SVGLength}} value
#' @param height the height length as an \code{\link{SVGLength}} value
#' @param mask.units the mask units system. See \code{\link{maskUnits}} for 
#'   details.
#' @param mask.content.units the mask content units system. See 
#'   \code{\link{maskContentUnits}} for details.
#' @param mask.content the content, shapes, of the mask. See
#'   \code{\link{maskContent}} for details.
#'   
#' @return a \code{\link{Mask}} object
#'   
#' @export Mask.factory
#'   
#' @examples
#' ## default mask
#' Mask.factory()
Mask.factory <- function(x,y,width,height,mask.units,mask.content.units,mask.content) {

  ## init. mask
  args <- list("Mask")
  if(!missing(x)) args <- c(args,x=x)
  if(!missing(y)) args <- c(args,y=y)
  if(!missing(width)) args <- c(args,width=width)
  if(!missing(height)) args <- c(args,height=height)
  if(!missing(mask.units)) args <- c(args,mask.units=mask.units)
  if(!missing(mask.content.units)) args <- c(args,mask.content.units=mask.content.units)
  if(!missing(mask.content)) args <- c(args,mask.content=mask.content)
  mask = do.call(new,args)

  ## eop
  return(mask)
}
