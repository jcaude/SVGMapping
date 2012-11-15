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
#'  @name targetAttribute
#'  @rdname mappingcolors.targetattr-methods
#'  @exportMethod targetAttribute
#'  @docType methods 
NULL
#setGenericVerif(name="targetAttribute", function(object) { standardGeneric("targetAttribute") })

#' <title already defined>
#' 
#' 
#' 
#'  @name targetAttribute<-
#'  @rdname mappingcolors.targetattr-methods
#'  @exportMethod targetAttribute<-
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
#' @name targetUnit<-
#' @rdname mappingvalues.targetunit-methods
#' @exportMethod targetUnit<-
#' @docType methods
setGeneric(name="targetUnit<-", function(.Object,value) { standardGeneric("targetUnit<-") })

#' <title already defined>
#' 
#' 
#' 
#'  @name exec
#'  @rdname mapping.exec-methods
#'  @exportMethod exec
#'  @docType methods 
NULL
#setGenericVerif(name="exec", function(.Object,svg) { standardGeneric("exec") })

setMethod(f="initialize", signature="MappingValues",
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
            targetAttribute(.Object) <- .arg("target.attribute",character(0))
            targetUnit(.Object) <- .arg("target.unit",vector("character"))

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
            .Object <- callNextMethod(.Object,svg)

            ## empty target attribute ?
            if(length(targetAttribute(.Object)) == 0)
              return(invisible(.Object))
            
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

#' Mapping Values Factory
#' 
#' This function returns a \emph{default} \code{\link{MappingValues}} instance.
#' 
#' Using this factory function, only the input \emph{values} and \emph{targets}
#' are initialized in the object. By default this mapping operation will have no
#' effect (\emph{ie} no target attribute assigned) if applied to an SVG template
#' (unless one has use the relevant methods to change this operation behavior).
#' 
#' @name MappingValues.factory
#'   
#' @param data is the input dataset to use for this mapping
#'   
#' @param targets is the list of template node targets to alter. This can be a 
#'   list of SVG nodes identifiers or any node selection expression. By default 
#'   the targets are the row names of the input data variable.
#'   
#' @param target.attribute specifies to which attribute the output value will be
#'   set. It can be either an SVG attribute (eg. \emph{opacity}) or a CSS style 
#'   sub-attribute (eg. \emph{style::stroke} or \emph{style::x}).
#'   
#' @param target.unit contains the unit(s) character string(s) that are use as 
#'   postfix for output values.
#'   
#' @param trans.function is the transformation function that is applied onto the
#'   data, prior to the color mapping (see the \code{\link{Mapping}} class
#'   documentation). By default the \emph{identity} function, which do not
#'   transformed the input data, is assigned to the newly created instance.
#'   
#' @param trans.parameters is the list of parameters values associated with the 
#'   transformation function.
#'   
#' @return a \code{\link{MappingValues}} object
#' 
#' @export MappingValues.factory
#' @seealso Other possible factory functions are:
#'   \code{\link{MappingOpacity.factory}}, 
#'   \code{\link{MappingFillOpacity.factory}},
#'   \code{\link{MappingStrokeOpacity.factory}} and
#'   \code{\link{MappingStrokeWidth.factory}}
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
#' ## one column named x. Row names are the circles identifiers.
#' dummy <- data.frame(x=c(0,0.2,0.4,0.6,0.8,1.0),
#'                    row.names=circles)
#'                    
#' ## Now, let's create a MappingValues instance using the default factory
#' ## function. We initialize the target attribute to 'opacity'
#' my.map <- MappingValues.factory(dummy)
#' targetAttribute(my.map) <- "opacity"
#' 
#' ## Then, we apply this mapping object to the template, and show
#' ## it in the default browser.
#' ## mapping(template,my.map)
#' ## show(template)
MappingValues.factory <- function(data,targets,
                                  target.attribute,target.unit,
                                  trans.function, trans.parameters) {
  
  ## check.
  if(missing(data))
    stop("'data' argument is absolutely required")

  ## check target attribute/unit length
  if(!(missing(target.attribute) || missing(target.unit)) 
     && (length(target.unit) != length(target.attribute)))
    target.unit <- rep(target.unit[[1]], length(target.attribute))
  
  ## init.
  args <- list("MappingValues")
  args[["values"]] <- data
  
  ## fill mapping structure
  if(!missing(targets)) 
    args[["targets"]] <- targets
  else    
    args[["targets"]] <- row.names(data)
  if(!missing(target.attribute)) args[["target.attribute"]] <- target.attribute
  if(!missing(target.unit)) args[["target.unit"]] <- target.unit
  if(!missing(trans.function)) args <- c(args,trans.function=trans.function)
  if(!missing(trans.parameters)) args[["trans.parameters"]] <- trans.parameters
  mapV <- do.call(new,args)
  
  #eop
  return(mapV)
}

#' Mapping Opacity Factory
#' 
#' This function returns a \code{\link{MappingValues}} instance that can be use
#' to change the opacity level according to some input values.
#' 
#' By default, this factory function expect that input values are levels in the 
#' range [0,1]. Thus, a default \code{\link{fnRangeLinear}} transformation, with
#' the parameters \emph{a=1, b=0, min=0} and \emph{max=1}, is assigned to the
#' mapping operation. Nevertheless, one can provide its own transformation 
#' function if necessary.
#' 
#' The \code{MappingOpacity.factory} factory function will create a mapping
#' operation that change the \strong{overall} opacity of the target shapes.
#' 
#' @name MappingOpacity.factory
#'   
#' @param data is the input dataset to use for this mapping
#'   
#' @param targets is the list of template node targets to alter. This can be a 
#'   list of SVG nodes identifiers or any node selection expression. By default 
#'   the targets are the row names of the input data variable.
#'   
#' @param trans.function is the transformation function that is applied onto the
#'   data, prior to the color mapping (see the \code{\link{Mapping}} class
#'   documentation). By default the \emph{identity} function, which do not
#'   transformed the input data, is assigned to the newly created instance.
#'   
#' @param trans.parameters is the list of parameters values associated with the 
#'   transformation function.
#'   
#' @return a \code{\link{MappingValues}} object
#' 
#' @export MappingOpacity.factory
#' @seealso Other possible factory functions are:
#'   \code{\link{MappingValues.factory}}, 
#'   \code{\link{MappingFillOpacity.factory}},
#'   \code{\link{MappingStrokeOpacity.factory}} and
#'   \code{\link{MappingStrokeWidth.factory}}
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
#' ## one column named x. Row names are the circles identifiers.
#' dummy <- data.frame(x=c(0,0.2,0.4,0.6,0.8,1.0),
#'                    row.names=circles)
#'                    
#' ## -------------- Overall Opacity Example                    
#' ## Now, let's create a MappingValues instance using the default factory
#' ## function. We initialize the target attribute to 'opacity'
#' all.map <- MappingOpacity.factory(dummy[,"x",drop=FALSE])
#' 
#' ## Then, we apply this mapping object to the template, and show
#' ## it in the default browser.
#' ## mapping(template, all.map)
#' ## show(template)
#' 
#' ## -------------- Filling Opacity Example                    
#' ## just using a different factory function we can restrict the change 
#' ## of opacity to only the filling of shapes
#' fill.map <- MappingFillOpacity.factory(dummy[,"x",drop=FALSE])
#' ## mapping(template, fill.map)
#' ## show(template)
#' 
#' ## -------------- Stroke Opacity Example                    
#' ## and now the stroke opacity..
#' stroke.map <- MappingStrokeOpacity.factory(dummy[,"x",drop=FALSE])
#' ## mapping(template, stroke.map)
#' ## show(template)
#' 
#' @rdname mappingvalues.opacity-factory
MappingOpacity.factory <- function(data,targets,
                                   trans.function,trans.parameters) {
  ## check.
  if(missing(data))
    stop("'data' argument is absolutely required")

  ## init.
  args <- list("MappingValues")
  args[["values"]] <- data
  args <- c(args, target.attribute="opacity")
  
  ## fill mapping structure
  if(!missing(targets)) 
    args[["targets"]] <- targets
  else    
    args[["targets"]] <- row.names(data)
  if(!missing(trans.parameters)) args[["trans.parameters"]] <- trans.parameters
  if(!missing(trans.function)) 
    args <- c(args,trans.function=trans.function)
  else {
    args <- c(args,trans.function=fnRangeLinear)
    args[["trans.parameters"]] <- list(a=1,b=0,min=0,max=1)
  }
  mapV <- do.call(new,args)
  
  #eop
  return(mapV)
}

#' <title already defined>
#' 
#' 
#' 
#' The \code{MappingFillOpacity.factory} factory function will create a mapping
#' operation that only change the \strong{filling} opacity of the target shapes.
#' 
#' @name MappingFillOpacity.factory
#' @rdname mappingvalues.opacity-factory
#' @export MappingFillOpacity.factory
MappingFillOpacity.factory <- function(data,targets,
                                       trans.function, trans.parameters) {
  ## check.
  if(missing(data))
    stop("'data' argument is absolutely required")
  
  ## init.
  args <- list("MappingValues")
  args[["values"]] <- data
  args <- c(args, target.attribute="style::fill-opacity")
  
  ## fill mapping structure
  if(!missing(targets)) 
    args[["targets"]] <- targets
  else    
    args[["targets"]] <- row.names(data)
  if(!missing(trans.parameters)) args[["trans.parameters"]] <- trans.parameters
  if(!missing(trans.function)) 
    args <- c(args,trans.function=trans.function)
  else {
    args <- c(args,trans.function=fnRangeLinear)
    args[["trans.parameters"]] <- list(a=1,b=0,min=0,max=1)
  }
  mapV <- do.call(new,args)
  
  #eop
  return(mapV)
}

#' <title already defined>
#' 
#' 
#' 
#' The \code{MappingStrokeOpacity.factory} factory function will create a
#' mapping operation that only change the \strong{stroke} opacity of the target
#' shapes.
#' 
#' @name MappingStrokeOpacity.factory
#' @rdname mappingvalues.opacity-factory
#' @export MappingStrokeOpacity.factory
MappingStrokeOpacity.factory <- function(data,targets,
                                         trans.function,trans.parameters) {
  ## check.
  if(missing(data))
    stop("'data' argument is absolutely required")
  
  ## init.
  args <- list("MappingValues")
  args[["values"]] <- data
  args <- c(args, target.attribute="style::stroke-opacity")
  
  ## fill mapping structure
  if(!missing(targets)) 
    args[["targets"]] <- targets
  else    
    args[["targets"]] <- row.names(data)
  if(!missing(trans.parameters)) args[["trans.parameters"]] <- trans.parameters
  if(!missing(trans.function)) 
    args <- c(args,trans.function=trans.function)
  else {
    args <- c(args,trans.function=fnRangeLinear)
    args[["trans.parameters"]] <- list(a=1,b=0,min=0,max=1)
  }
  mapV <- do.call(new,args)
  
  #eop
  return(mapV)
}

#' Mapping Stroke Width Factory
#' 
#' This function returns a \code{\link{MappingValues}} instance that can be use
#' to change the stroke width according to some input values.
#' 
#' By default, this factory function expect that input values are numerics given
#' in the SVG user coordinates system. Thus, if inputs are not given in this
#' system on can fix this by using the \code{\link{targetUnit}} method.
#' 
#' @name MappingStrokeWidth.factory
#'   
#' @param data is the input dataset to use for this mapping
#'   
#' @param targets is the list of template node targets to alter. This can be a 
#'   list of SVG nodes identifiers or any node selection expression. By default 
#'   the targets are the row names of the input data variable.
#'   
#' @param target.unit contains the unit(s) character string(s) that are use as 
#'   postfix for stroke width values. By default it is assumed that width values
#'   are given in the user coordinates system (see the SVG specification for
#'   more details about units). If one wants to use a different size, making a
#'   unit conversion, both the target unit and a transformation function (see
#'   below) should be provided.
#'   
#' @param trans.function is the transformation function that is applied onto the
#'   data, prior to the color mapping (see the \code{\link{Mapping}} class
#'   documentation). By default the \emph{identity} function, which do not
#'   transformed the input data, is assigned to the newly created instance.
#'   
#' @param trans.parameters is the list of parameters values associated with the 
#'   transformation function.
#'   
#' @return a \code{\link{MappingValues}} object
#' 
#' @export MappingStrokeWidth.factory
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
#' ## one column named x. Row names are the circles identifiers.
#' dummy <- data.frame(x=runif(6,min=1,max=5),
#'                    row.names=circles)
#'                    
#' ## Now, let's create a MappingValues instance using the stroke width factory
#' ## function. We also set the target attribute unit to 'cm'
#' my.map <- MappingOpacity.factory(dummy[,"x",drop=FALSE])
#' targetUnit(my.map) <- "cm"
#' 
#' ## Then, we apply this mapping object to the template, and show
#' ## it in the default browser.
#' ## mapping(template, my.map)
#' ## show(template)
MappingStrokeWidth.factory <- function(data,targets,
                                       target.unit,
                                       trans.function, trans.parameters) {
  ## check.
  if(missing(data))
    stop("'data' argument is absolutely required")
  
  ## init.
  args <- list("MappingValues")
  args[["values"]] <- data
  args <- c(args, target.attribute="style::stroke-width")
  
  ## fill mapping structure
  if(!missing(targets)) 
    args[["targets"]] <- targets
  else    
    args[["targets"]] <- row.names(data)
  if(!missing(target.unit)) args[["target.unit"]] <- target.unit
  if(!missing(trans.parameters)) args[["trans.parameters"]] <- trans.parameters
  if(!missing(trans.function)) args <- c(args,trans.function=trans.function)
  mapV <- do.call(new,args)
  
  #eop
  return(mapV)
}
