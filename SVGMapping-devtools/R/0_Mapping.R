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

## M A P P I N G  (virtual)
## --------------------------------------------------
library(methods)

setGenericVerif <- function(name,y){if(!isGeneric(name)){setGeneric(name,y)}else{}}

#' Mapping built-in transformation functions
#' 
#' SVGMapping comes with a set of predifined \emph{transformation} functions.  
#' \emph{transformation} function are applied to the input values. Still,
#' one can implement it's own function.
#' 
#' \code{MAPPING.Random} returns random values (whatever the input) in the range
#' \code{[p$min,p$max]}.
#' 
#' @section Note: One can design its own transformation function as long as it 
#' uses the standard prototype \code{function(x,p)}. Where \code{x} is an 
#' atomic value and \code{p} the function parameters as a list.
#' 
#' @param x the input value 
#' @param p the function parameters list
#' @return the transformed value which can be of any type (\emph{eg} character,
#' numeric,...)
#' 
#' @name MAPPING.Random
#' @rdname mapping.transform-functions
#' @export
MAPPING.Random <- function(x,p) { return(runif(1,min=p$min, max=p$max)) }

#' <title already defined>
#' 
#' 
#' 
#' \code{MAPPING.Identity} returns the input value without doing any 
#' modification.
#' 
#' @rdname mapping.transform-functions
#' @export
MAPPING.Identity <- function(x,p) { return(x) }

#' <title already defined>
#' 
#' 
#' 
#' \code{MAPPING.Linear} is used to apply a linear transformation, 
#' \deqn{y=p_a \time x+p_b}{y=p$a*x+p$b}.
#' 
#' @rdname mapping.transform-functions
#' @export
MAPPING.Linear <- function(x,p) { return(p$a*x+p$b) }

#' <title already defined>
#' 
#' 
#' 
#' \code{MAPPING.RangeLinear} is used to apply a bound linear transformation.
#' \deqn{y = \begin{cases} 
#'              p_{min} & \text{if $(p_a x+b) \leq p_{min}$} \\
#'              p_{max} & \text{if $(p_a x+b) \geq p_{max}$} \\
#'              p_a x+b & \text{otherwise}
#'            \end{cases}
#'     }{y = p$min (if ax+b<p$min) ; p$max (if ax+b>p$max); ax+b (otherwise)}
#' 
#' @rdname mapping.transform-functions
#' @export
MAPPING.RangeLinear <- function(x,p) {return(min(p$max, max(p$min,p$a*x+p$b))) } 

#' <title already defined>
#' 
#' 
#' 
#' \code{MAPPING.Logistic} is used to apply a logistic transformation.
#' \deqn{ y = \frac{p_k}{1+p_a \exp^{-p_r x}}
#' } {y = p$k / (1 + p$a * exp(-p$r * x))}
#' 
#' @rdname mapping.transform-functions
#' @export
MAPPING.Logistic <- function(x,p) { return(p$K/(1+p$a*exp(-p$r*x))) }

#' <title already defined>
#' 
#' 
#' 
#' \code{MAPPING.Sigmoid} is used to apply a sigmoid transformation.
#' \deqn{ y = \frac{1}{1+p_a \exp^{-p_r x}}
#' } {y = 1 / (1 + p$a * exp(-p$r * x))}
#' 
#' @rdname mapping.transform-functions
#' @export
MAPPING.Sigmoid <- function(x,p) { return(1/(1+exp(-p$r*x))) }

#' <title already defined>
#' 
#' 
#' 
#' \code{MAPPING.Log2FC} is a transformation function specifically design for
#' microarrays analysis. It can be used to transform log based 2 expression 
#' ratios into fold-change.
#' \deqn{y = \begin{cases} 
#'              \exp(x \log(2)) & \text{if $x \geq 0$} \\
#'              \frac{-1}{\exp(x \log(2))} & \text{otherwise}
#'            \end{cases}
#'     }{y = exp(x * log(2)) (if x>0) ; -1/(exp(x*log(2))) (otherwise)}
#' 
#' @rdname mapping.transform-functions
#' @export
MAPPING.Log2FC <- function(x,p) { return(ifelse(x>=0,exp(x*log(2)),-1/exp(x*log(2)))) }

#' Mapping Root Class (VIRTUAL)
#' 
#' This class is the root class for all mapping operations. 
#' This class can't be directly instantiate and must be derived. 
#' 
#' Mapping operations allows to transform the content of a template. This 
#' process used \strong{data}, a set of \strong{values} and \strong{targets}.
#' First values are transformed using built-in or user-defined function 
#' (see \link{transFunction}). Then, these newly transformed values are used for 
#' by mapping operations (\emph{eg} color modification, change of the opacity) 
#' to alter the set of \strong{targets} into the template. It is noteworthy to
#' mention that Mapping processes can be stack to create complex templante 
#' rendering.
#' 
#' @exportClass "Mapping"
#' @aliases Mapping-class
setClass("Mapping",
         representation(targets="ANY",
                        values="ANY",
                        fn="ANY",
                        fn.parameters="list",
                        animation="logical",
                        .values="ANY",
                        "VIRTUAL"
                        )
         )

#' Mapping Data (Values and Targets) Accessors
#' 
#' Mapping requires a set of \strong{values} and \strong{targets} to proceed. 
#' \strong{Values}, after transformation, are used to modify various 
#' attributes of graphical shapes. \strong{Targets} are used to identify the 
#' shapes to alter.
#' 
#' \code{Targets} must be a valid node selectors (SEE SVG:NODE-SELECTORS). The 
#' most common node selector is a set of shape identifiers or labels (note: 
#' labels are not part of the official SVG 1.1 specification and a valid
#' namespace must be provided, such as Inkscape NS). But more complex target
#' selection can be done using XPath expression for example.
#' 
#' The \code{targets(object)} method retrieve the list of target shapes to
#' modify
#' 
#' @name targets
#'  
#' @param object the mapping instance object
#'   
#' @return an object set identifiers. 
#' 
#' @rdname mapping.data-methods
#' @exportMethod targets
#' @docType methods
setGeneric(name="targets", function(object) { standardGeneric("targets") })

#' <title already defined>
#' 
#' 
#' 
#' @name targets<-
#' @rdname mapping.data-methods
#' @exportMethod targets<-
#' @docType methods
setGeneric(name="targets<-", function(.Object,value) { standardGeneric("targets<-") })

#' <title already defined>
#' 
#' 
#' 
#' @name values
#' @rdname mapping.data-methods
#' @exportMethod values
#' @docType methods
setGeneric(name="values", function(object) { standardGeneric("values") })

#' <title already defined>
#' 
#' 
#' 
#' @name values<-
#' @rdname mapping.data-methods
#' @exportMethod values<-
#' @docType methods
setGeneric(name="values<-", function(.Object,value) { standardGeneric("values<-") })

#' Mapping Transformation function accessors
#' 
#' Mapping is a two steps procedure. First, data are processed by a 
#' \emph{transformation} function. Second, transformed data are used by the
#' mapping \emph{operation} to alter the template content.
#' The \emph{transformation} function is applied to the whole input dataset. 
#' Input datasets can be a data-frame or a vector. In the first case data will
#' be applied on each "array" values by iteratively process rows and columns. 
#' In the second case, the transformation function is applied iteratively on 
#' each vector values. 
#' The SVGMapping package provides a set of built-in transformation function. 
#' But you can easily design your own transformation as long as it is compliant
#' with the \code{function(x,p)} prototype. Where \code{x} is an atomic input
#' value and \code{p} a set of parameters given as a list. A complete example
#' of a user transformation function is given in the example section
#' 
#' The \code{transFunction(object)} retrieve the current transformation 
#' function
#' 
#' @name transFunction
#' 
#' @param object the mapping instance
#' 
#' @return a \code{function(x,p)} or the transformation function parameters 
#' \code{p}.
#' 
#' @rdname mapping.transfunc-methods
#' @exportMethod transFunction
#' @docType methods
setGeneric(name="transFunction", function(object) { standardGeneric("transFunction") })

#' <title already defined>
#' 
#' 
#' 
#' The \code{transFunction(object) <- value} set the transformation function
#' for the \code{object} mapping instance
#' 
#' @rdname mapping.transfunc-methods
#' @exportMethod transFunction<-
#' @docType methods
setGeneric(name="transFunction<-", function(.Object,value) { standardGeneric("transFunction<-") })

#' <title already defined>
#' 
#' 
#' 
#' The \code{transParameters(object)} returns the parameters list used by 
#' the transformation function
#' 
#' @rdname mapping.transfunc-methods
#' @exportMethod transParameters
#' @docType methods
setGeneric(name="transParameters", function(object) { standardGeneric("transParameters") })

#' <title already defined>
#' 
#' 
#' 
#' The \code{transParameters(object)<-value} sets the parameters list used by 
#' the transformation function
#' 
#' @rdname mapping.transfunc-methods
#' @exportMethod transParameters<-
#' @docType methods
setGeneric(name="transParameters<-", function(.Object,value) { standardGeneric("transParameters<-") })

#' Mapping Transformation Function
#' 
#' These methods allow to set the transformation function, either using one
#' of the built-in function or using a user-define function.
#' 
#' The \code{fnRandom(.Object,min,max)} method assigns the \code{\link{MAPPING.Random}} function
#' to the current mapping instance. 
#' 
#' @name fnRandom
#' 
#' @param .Object the mapping instance
#' @param min the minimum parameter of a transformation function
#' @param max the maximum parameter of a transformation function
#' 
#' @return the mapping instance as an invisible object
#' 
#' @rdname mapping.transformation-methods
#' @exportMethod fnRandom
#' @docType methods
setGeneric(name="fnRandom", function(.Object,min,max) { standardGeneric("fnRandom") })

#' <title already defined>
#' 
#' 
#' 
#' The \code{fnIdentity(.Object)} is a trivial transformation function that 
#' returns the input value as without any modifications.
#' 
#'  @name fnIdentity
#'  @rdname mapping.transformation-methods
#'  @exportMethod fnIdentity
#'  @docType methods 
setGeneric(name="fnIdentity", function(.Object) { standardGeneric("fnIdentity") })

#' <title already defined>
#' 
#' 
#' 
#' The \code{fnLinear(.Object,a,b)} method assigns the 
#' \code{\link{MAPPING.Linear}} transformation function to the current mapping 
#' instance.
#' 
#' @name fnLinear
#' 
#' @param a a parameter of the transformation function
#' @param b a parameter of the transformation functions
#' 
#' @rdname mapping.transformation-methods
#' @exportMethod fnLinear
#' @docType methods
setGeneric(name="fnLinear", function(.Object,a,b) { standardGeneric("fnLinear") })

#' <title already defined>
#' 
#' 
#' 
#' The \code{fnRangeLinear(.Object,a,b,min,max)} method assigns the
#' \code{\link{MAPPING.RangeLinear}} transformation function to the current
#' mapping instance.
#' 
#'  @name fnRangeLinear
#'  
#'  @rdname mapping.transformation-methods
#'  @exportMethod fnRangeLinear
#'  @docType methods 
setGeneric(name="fnRangeLinear", function(.Object,a,b,min,max) { standardGeneric("fnRangeLinear") })

#' <title already defined>
#' 
#' 
#' 
#' The \code{fnLogistic(.Object,K,a,r)} method assigns the
#' \code{\link{MAPPING.Logistic(.Object,K,a,r)}} transformation function to
#' the current mapping instance.
#' 
#' @name fnLogistic
#' 
#' @param K a parameter of the transformation function
#' @param r a parameter of the transformation function
#' 
#' @rdname mapping.transformation-methods
#' @exportMethod fnLogistic
#' @docType methods
setGeneric(name="fnLogistic", function(.Object,K,a,r) { standardGeneric("fnLogistic") })

#' <title already defined>
#' 
#' 
#' 
#' The \code{fnSigmoid(.Object,r)} method assigns the 
#' \code{\link{MAPPING.Sigmoid}} transformation function to the mapping 
#' instance.
#' 
#' @name fnSigmoid
#' @rdname mapping.transformation-methods
#' @exportMethod fnSigmoid
#' @docType methods
setGeneric(name="fnSigmoid", function(.Object,r) { standardGeneric("fnSigmoid") })

#' <title already defined>
#' 
#' 
#' 
#' The \code{fnLog2FC(.Object)} method assigns the \code{\link{MAPPING.Log2FC}}
#' transformation function to the mapping instance.
#' 
#' @name fnLog2FC
#' @rdname mapping.transformation-methods
#' @exportMethod fnLog2FC
#' @docType methods
setGeneric(name="fnLog2FC", function(.Object) { standardGeneric("fnLog2FC") })

#' <title already defined>
#' 
#' 
#' 
#' The \code{fnUser(.Object,fn,fn.params)} method assigns a user-defined 
#' transformation function to the mapping instance. The \code{fn} parameter
#' is a transformation function with the prototype \code{function(x,p)}. The
#' \code{fn.params} is the function parameters named list.
#' 
#' @name fnUser
#' @rdname mapping.transformation-methods
#' @exportMethod fnUser
#' @docType methods
setGeneric(name="fnUser", function(.Object,fn,fn.params) { standardGeneric("fnUser") })

#' <title already defined>
#' 
#' 
#' 
#' The \code{fnNone(.Object)} remove any previous transformation function
#' assignment. Thus, the input data are not processed by a function prior
#' to the mapping operation. Compare to \code{fnIdentity}, which at the end 
#' gives a similar output, the \code{fnNone} is much more efficient because
#' there is no function called.
#' 
#' @name fnNone
#' @rdname mapping.transformation-methods
#' @exportMethod fnNone
#' @docType methods
setGeneric(name="fnNone", function(.Object) { standardGeneric("fnNone") })


setGenericVerif(name="exec", function(.Object,svg) { standardGeneric("exec") })

setMethod(f="initialize", signature="Mapping",
          definition=function(.Object,...)
          {
            ## default init.
            targets(.Object) <- NULL
            .Object@values <- NULL
            transFunction(.Object) <- NULL
            transParameters(.Object) <- list()
            .Object@animation <- logical()
            
            ## eop
            return(.Object)
          }
          )

#' @aliases targets,Mapping-method
#' @rdname mapping.data-methods
setMethod(f="targets", signature="Mapping",
          definition=function(object)
          {
            return(object@targets)
          }
          )

#' @name targets<-
#' @aliases targets<-,Mapping-method
#' @rdname mapping.data-methods
setReplaceMethod(f="targets", signature="Mapping",
                 definition=function(.Object,value)
                 {
                   ## !!!!! CHECKING ??
                   ## eop
                   .Object@targets <- value
                   return(.Object)
                 }
                 )

#' @aliases values,Mapping-method
#' @rdname mapping.data-methods
setMethod(f="values", signature="Mapping",
          definition=function(object)
          {
            return(object@values)
          }
          )
          
#' @name values<-
#' @aliases values<-,Mapping-method
#' @rdname mapping.data-methods
setReplaceMethod(f="values", signature="Mapping",
                 definition=function(.Object,value)
                 {
                   ## transform
                   if(is.list(value)) value <- data.frame(value)
                   
                   ## eop
                   .Object@values <- value
                   return(.Object)
                 }
                 )

#' @rdname mapping.transfunc-methods
#' @aliases transFunction,Mapping-method
setMethod(f="transFunction", signature="Mapping",
          definition=function(object)
          {
            return(object@fn)
          }
          )
          
#' @name transFunction<-
#' @rdname mapping.transfunc-methods
#' @aliases transFunction<-,Mapping-method
setReplaceMethod(f="transFunction", signature="Mapping",
                 definition=function(.Object,value)
                 {
                   ## check
                   if(!(is.function(value) || is.null(value))) {
                     stop("Invalid 'value' argument, must be a 'function' or NULL")
                   }
                   
                   ## eop
                   .Object@fn <- value
                   return(.Object)
                 }
)

#' @rdname mapping.transfunc-methods
#' @aliases transParameters,Mapping-method
setMethod(f="transParameters", signature="Mapping",
          definition=function(object)
          {
            return(object@fn.parameters)
          }
          )

#' @name transParameters<- 
#' @rdname mapping.transfunc-methods
#' @aliases transParameters<-,Mapping-method
setReplaceMethod(f="transParameters", signature="Mapping",
                 definition=function(.Object,value)
                 {
                   ## check
                   if(!is.list(value) && 
                     !(length(names(value)) == length(value)))
                     stop("Invalid 'value' argument, must be a named list")
                   
                   ## eop
                   .Object@fn.parameters <- value
                   return(.Object)
                 }
  )

#' @rdname mapping.transformation-methods
#' @aliases fnRandom,Mapping-method
setMethod(f="fnRandom", signature="Mapping",
          definition=function(.Object,min,max)
          {
            ## init.
            namedOjbect <- deparse(substitute(.Object))
            min <- if(missing(min)) 0 else as.numeric(min)
            max <- if(missing(max)) 1 else as.numeric(max)

            ## check
            if(!is.numeric(min) || !is.numeric(max))
              stop("Random generator, parameters 'min' and 'max' must be numeric") 
            
            ## update
            transFunction(.Object) <- MAPPING.Random
            transParameters(.Object) <- list(min=min, max=max)
            
            ## eop
            assign(namedOjbect, .Object, envir=parent.frame())
            return(invisible(.Object))
          }
          )

#' @rdname mapping.transformation-methods
#' @aliases fnIdentity,Mapping-method
setMethod(f="fnIdentity", signature="Mapping",
          definition=function(.Object)
          {
            ## init.
            namedOjbect <- deparse(substitute(.Object))

            ## update
            transFunction(.Object) <- MAPPING.Identity
            transParameters(.Object) <- list()
            
            ## eop
            assign(namedOjbect, .Object, envir=parent.frame())
            return(invisible(.Object))
          }
          )
          
#' @rdname mapping.transformation-methods
#' @aliases fnLinear,Mapping-method
setMethod(f="fnLinear", signature="Mapping",
          definition=function(.Object,a,b)
          {
            ## init.
            namedOjbect <- deparse(substitute(.Object))
            a <- if(missing(a)) 1 else a
            b <- if(missing(b)) 0 else b

            ## check
            if(!is.numeric(a) || !is.numeric(b))
              stop("linear (a*x+b) parameters 'a' and 'b' must be numeric") 

            ## update
            transFunction(.Object) <- MAPPING.Linear
            transParameters(.Object) <- list(a=a,b=b)
            
            ## eop
            assign(namedOjbect, .Object, envir=parent.frame())
            return(invisible(.Object))
          }
          )

#' @rdname mapping.transformation-methods
#' @aliases fnRangeLinear,Mapping-method
setMethod(f="fnRangeLinear", signature="Mapping",
          definition=function(.Object,a,b,min,max)
          {
            ## init.
            namedOjbect <- deparse(substitute(.Object))
            a <- if(missing(a)) 1 else a
            b <- if(missing(b)) 0 else b
            min <- if(missing(min)) -Inf else min
            max <- if(missing(max)) Inf else max

            ## check
            if(!is.numeric(a) || !is.numeric(b) || !is.numeric(min) || !is.numeric(max))
              stop("Range linear [min,(a*x+b),max] parameters 'a','b','min' and 'max' must be numeric") 

            ## update
            transFunction(.Object) <- MAPPING.Linear
            transParameters(.Object) <- list(a=a,b=b,min=min,max=max)
            
            ## eop
            assign(namedOjbect, .Object, envir=parent.frame())
            return(invisible(.Object))
          }
          )

#' @rdname mapping.transformation-methods
#' @aliases fnLogistic,Mapping-method
setMethod(f="fnLogistic", signature="Mapping",
          definition=function(.Object,K,a,r)
          {
            ## init.
            namedOjbect <- deparse(substitute(.Object))
            K <- if(missing(K)) 1 else K
            a <- if(missing(a)) 1 else a
            r <- if(missing(r)) 1 else r

            ## check
            if(!is.numeric(K) || !is.numeric(a) || !is.numeric(r))
              stop("logistic (K/(1+a*exp(-rx)) parameters 'K','a' and 'r' must be numeric") 

            ## update
            transFunction(.Object) <- MAPPING.Logistic
            transParameters(.Object) <- list(K=K,a=a,r=r)
            
            ## eop
            assign(namedOjbect, .Object, envir=parent.frame())
            return(invisible(.Object))
          }
          )

#' @rdname mapping.transformation-methods
#' @aliases fnSigmoid,Mapping-method
setMethod(f="fnSigmoid", signature="Mapping",
          definition=function(.Object,r)
          {
            ## init.
            namedOjbect <- deparse(substitute(.Object))
            r <- if(missing(r)) 1 else r

            ## check
            if(!is.numeric(r))
              stop("Sigmoid (1/(1+exp(-rx)) parameter 'r' must be numeric") 

            ## update
            transFunction(.Object) <- MAPPING.Sigmoid
            transParameters(.Object) <- list(r=r)
            
            ## eop
            assign(namedOjbect, .Object, envir=parent.frame())
            return(invisible(.Object))
          }
          )

#' @rdname mapping.transformation-methods
#' @aliases fnLog2FC,Mapping-method
setMethod(f="fnLog2FC", signature="Mapping",
          definition=function(.Object)
          {
            ## init.
            namedOjbect <- deparse(substitute(.Object))

            ## update
            transFunction(.Object) <- MAPPING.Log2FC
            transParameters(.Object) <- list()
            
            ## eop
            assign(namedOjbect, .Object, envir=parent.frame())
            return(invisible(.Object))
          }
          )

#' @rdname mapping.transformation-methods
#' @aliases fnUser,Mapping-method
setMethod(f="fnUser", signature="Mapping",
          definition=function(.Object,fn,fn.params)
          {
            ## check
            if(missing(fn) || !is.function(fn))
              stop("You must provide a valid function 'fn'")           
            if(missing(fn.params) || !is.list(fn.params))
              stop("You must provide a valid list of parameters") 
            
            ## init.
            namedOjbect <- deparse(substitute(.Object))          

            ## update
            transFunction(.Object) <- fn
            transParameters(.Object) <- fn.params
            
            ## eop
            assign(namedOjbect, .Object, envir=parent.frame())
            return(invisible(.Object))
          }
          )

#' @rdname mapping.transformation-methods
#' @aliases fnNone,Mapping-method
setMethod(f="fnNone", signature="Mapping",
          definition=function(.Object)
          {
            ## init.
            namedOjbect <- deparse(substitute(.Object))

            ## update
            transFunction(.Object) <- NULL
            transParameters(.Object) <- list()
            
            ## eop
            assign(namedOjbect, .Object, envir=parent.frame())
            return(invisible(.Object))            
          }
          )

setMethod(f="exec", signature="Mapping",
          definition=function(.Object,svg)
          {
            ## init.
            namedOjbect <- deparse(substitute(.Object))          

            ## 'none' case
            if(is.null(.Object@fn))
              .Object@.values <- .Object@values

            ## apply function to values and update object.
            else {
              if(is.list(.Object@values) || is.vector(.Object@values)) 
                .Object@.values <- sapply(.Object@values,
                                          function(x) { sapply(x, .Object@fn, .Object@fn.parameters) }
                                          )
              else
                .Object@.values <- apply(.Object@values, c(1,2),
                                         function(x) { sapply(x, .Object@fn, .Object@fn.parameters) }
                                         )
            }

            ## eop
            assign(namedOjbect, .Object, envir=parent.frame())
            return(invisible(.Object))
          }
          )