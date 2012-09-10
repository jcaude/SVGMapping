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

## M A P P I N G   -   V A L U E S
## --------------------------------------------------

setGenericVerif <- function(name,y){if(!isGeneric(name)){setGeneric(name,y)}else{}}

#' Values Mapping Operation
#' 
#' This class allows to convert input values into output values that can be 
#' assigned to a given shape attribute. For example, arbitrary numerical values 
#' can be used to change the width or opacity level of target shapes.
#' 
#' The Mapping Values operation use the input value, without any modification 
#' (such as the linear mapping done in the \code{\link{MappingColors}} class), 
#' expect a character transformation (using default R casting). Optionally, one 
#' can postfix the output values with units using the \code{\link{targetUnit}} 
#' method. Thus, one has to use a transformation function (see the 
#' \code{\link{Mapping}} class) to change the  input values prior to the 
#' mapping. It can be useful for example for scaling or unit conversion
#' purposes.
#' 
#' @name MappingValues
#' @exportClass "MappingValues"
#' @aliases MappingValues-class
setClass("MappingValues",
         representation(target.attribute="vector",
                        target.unit="vector"),
         contains="Mapping"
         )


#' <title already defined>
#' 
#' 
#' 
#'  @rdname mappingcolors.targetattr-methods
#'  @exportMethod targetAttribute
#'  @docType methods 
NULL
#setGenericVerif(name="targetAttribute", function(object) { standardGeneric("targetAttribute") })

#' <title already defined>
#' 
#' 
#' 
#'  @rdname mappingcolors.targetattr-methods
#'  @exportMethod targetAttribute
#'  @docType methods 
NULL
#setGenericVerif(name="targetAttribute<-", function(.Object,value) { standardGeneric("targetAttribute<-") })

#' Target Unit Accessor
#' 
#' Target units are used to postfix outputs of a mapping values operation. This 
#' is useful to specify units (\emph{ie} after a conversion transformation
#' function).
#' 
#' There is no need to add an extra space in the unit specification, it will be
#' automatically added during the mapping process.
#' 
#' The \code{targetUnit(object)} method retrieves the current target unit value.
#' 
#' @name targetUnit
#'   
#' @param object the mapping instance object
#'   
#' @return the unit value as a character string
#'   
#' @rdname mappingvalues.targetunit-methods
#' @exportMethod targetUnit
#' @docType methods
setGeneric(name="targetUnit", function(object) { standardGeneric("targetUnit") })

#' <title already defined>
#' 
#' 
#' 
#' The \code{targetUnit(object) <- value} method sets the target unit value 
#' of the \code{object} mapping instance
#' 
#' @rdname mappingvalues.targetunit-methods
#' @exportMethod targetUnit
#' @docType methods
setGeneric(name="targetUnit<-", function(.Object,value) { standardGeneric("targetUnit<-") })

#' <title already defined>
#' 
#' 
#' 
#'  @rdname mapping.exec-methods
#'  @exportMethod exec
#'  @docType methods 
NULL
#setGenericVerif(name="exec", function(.Object,svg) { standardGeneric("exec") })

setMethod(f="initialize", signature="MappingValues",
          definition=function(.Object,...)
          {            
            ## super
            .Object <- callNextMethod()

            ## default init.
            .Object@target.attribute <- c("opacity")
            .Object@target.unit <- vector()

            ## eop
            return(.Object)
          }
          )

#' @aliases targetAttribute,MappingValues-method
#' @rdname mappingcolors.targetattr-methods
setMethod(f="targetAttribute", signature="MappingValues",
          definition=function(object)
          {
            return(object@target.attribute)
          }
          )

#' @name targetAttribute<-
#' @aliases targetAttribute<-,MappingValues-method
#' @rdname mappingcolors.targetattr-methods
setReplaceMethod(f="targetAttribute", signature="MappingValues",
                 definition=function(.Object,value)
                 {
                   ## init.
                   if(is.atomic(value)) value <- c(value)
                   
                   ## check
                   if(!is.character(value))
                     stop("Target attribute 'value' must be character strings")

                   ## init.
                   .Object@target.attribute <- value
                   return(.Object)
                 }
                 )

#' @aliases targetUnit,MappingValues-method
#' @rdname mappingvalues.targetunit-methods
setMethod(f="targetUnit", signature="MappingValues",
          definition=function(object)
          {
            return(object@target.unit)
          }
          )

#' @name targetUnit<-
#' @aliases targetUnit<-,MappingValues-method
#' @rdname mappingvalues.targetunit-methods
setReplaceMethod(f="targetUnit", signature="MappingValues",
                 definition=function(.Object,value)
                 {
                   ## init.
                   if(is.atomic(value)) value <- c(value)
                   
                   ## check
                   if(!is.character(value))
                     stop("Values unit 'value' must be character strings")

                   ## init.
                   .Object@target.unit <- value
                   return(.Object)
                 }
                 )

#' @aliases exec,MappingValues-method
#' @rdname mapping.exec-methods 
setMethod(f="exec", signature="MappingValues",
          definition=function(.Object,svg)
          {
            ## init.            
            ncond <- ncol(.Object@values)
            if(is.null(ncond)) ncond <- 1
            
            ## call super
            callNextMethod()

            ## check & bound
            if(ncond < 2) 
              .Object@.values <- sapply(.Object@.values,
                                        function(x,u) { return(paste(x,u,sep="")) },
                                        .Object@target.unit
                                        )
            else {
              .Object@.values <- t(apply(.Object@.values, 1,
                                       function(x,u) { return(paste(x,u,sep="")) },
                                       .Object@target.unit))
            }
            
            ## set values
            svg[.Object@targets, .Object@target.attribute] <- .Object@.values

            ## eop
            return(invisible(.Object))
          }
          )

## F A C T O R Y
##--------------
MappingValues.factory <- function(data,targets=rownames(data),
                                  target.attribute,target.units=c(""),
                                  fn="Identity", fn.parameters=list()) {
  
  ## init.
  mapV <- new("MappingValues")

  ## check length
  if(length(target.units) != length(target.attribute))
    target.units <- rep(target.units[[1]], length(target.attribute))
  
  ## fill mapping structure
  values(mapV) <- data
  targets(mapV) <- targets
  targetAttribute(mapV) <- target.attribute
  targetUnit(mapV) <- target.units

  ## set transform function
  if(missing(fn.parameters))
    mapV <- setFunction(mapV,fn)
  else
    mapV <- setFunction(mapV,fn,fn.parameters)
  
  #eop
  return(mapV)
}

MappingOpacity.factory <- function(data,targets=rownames(data),
                                   target.attribute=c("opacity"),
                                   fn="Identity", fn.parameters=list()) {
  ## init.
  if(missing(fn.parameters)) 
    mapV <- MappingValues.factory(data,targets,
                                  target.attribute,target.units=c(""),
                                  fn)
  else
    mapV <- MappingValues.factory(data,targets,
                                  target.attribute,target.units=c(""),
                                  fn,fn.parameters)

  ## eop
  return(mapV)
}

MappingFillOpacity.factory <- function(data,targets=rownames(data),
                                       target.attribute=c("style::fill-opacity"),
                                       fn="Identity", fn.parameters=list()) {
  ## init.
  if(missing(fn.parameters)) 
    mapV <- MappingValues.factory(data,targets,
                                  target.attribute,target.units=c(""),
                                  fn)
  else
    mapV <- MappingValues.factory(data,targets,
                                  target.attribute,target.units=c(""),
                                  fn,fn.parameters)

  ## eop
  return(mapV)
}

MappingStrokeOpacity.factory <- function(data,targets=rownames(data),
                                         target.attribute=c("style::stroke-opacity"),
                                         fn="Identity", fn.parameters=list()) {
  ## init.
  if(missing(fn.parameters)) 
    mapV <- MappingValues.factory(data,targets,
                                  target.attribute,target.units=c(""),
                                  fn)
  else
    mapV <- MappingValues.factory(data,targets,
                                  target.attribute,target.units=c(""),
                                  fn,fn.parameters)

  ## eop
  return(mapV)
}

MappingStrokeWidth.factory <- function(data,targets=rownames(data),
                                       target.attribute=c("style::stroke-width"),
                                       fn="Identity", fn.parameters=list()) {
  ## init.
  if(missing(fn.parameters)) 
    mapV <- MappingValues.factory(data,targets,
                                  target.attribute,target.units=c(""),
                                  fn)
  else
    mapV <- MappingValues.factory(data,targets,
                                  target.attribute,target.units=c(""),
                                  fn,fn.parameters)

  ## eop
  return(mapV)
}
