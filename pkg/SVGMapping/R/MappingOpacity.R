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

setClass("MappingValues",
         representation(target.attributes="vector",
                        values.unit="vector"),
         contains="Mapping"
         )

setGenericVerif(name="targetAttributes", function(object) { standardGeneric("targetAttributes") })
setGenericVerif(name="targetAttributes<-", function(.Object,value) { standardGeneric("targetAttributes<-") })
setGenericVerif(name="valuesUnit", function(object) { standardGeneric("valuesUnit") })
setGenericVerif(name="valuesUnit<-", function(.Object,value) { standardGeneric("valuesUnit<-") })
setGenericVerif(name="exec", function(.Object,svg) { standardGeneric("exec") })

setMethod(f="initialize", signature="MappingValues",
          definition=function(.Object,...)
          {            
            ## super
            .Object <- callNextMethod()

            ## default init.
            .Object@target.attributes <- c("opacity")
            .Object@values.unit <- vector()

            ## eop
            return(.Object)
          }
          )

setMethod(f="targetAttributes", signature="MappingValues",
          definition=function(object)
          {
            return(object@target.attributes)
          }
          )

setReplaceMethod(f="targetAttributes", signature="MappingValues",
                 definition=function(.Object,value)
                 {
                   ## init.
                   if(is.atomic(value)) value <- c(value)
                   
                   ## check
                   if(!is.character(value))
                     stop("Target attribute 'value' must be character strings")

                   ## init.
                   .Object@target.attributes <- value
                   return(.Object)
                 }
                 )

setMethod(f="valuesUnit", signature="MappingValues",
          definition=function(object)
          {
            return(object@values.unit)
          }
          )

setReplaceMethod(f="valuesUnit", signature="MappingValues",
                 definition=function(.Object,value)
                 {
                   ## init.
                   if(is.atomic(value)) value <- c(value)
                   
                   ## check
                   if(!is.character(value))
                     stop("Values unit 'value' must be character strings")

                   ## init.
                   .Object@values.unit <- value
                   return(.Object)
                 }
                 )

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
              .Object@.values <- sapply(.Object@.values, function(x) {return(min(max(0,x),1))})
            else
              .Object@.values <- apply(.Object@.values, c(1,2),
                                       function(x) {return(as.character(min(max(0,x),1)))}
                                       )
            ## paste units
            if(length(.Object@target.attributes) == length(.Object@values.unit))
              .Object@.values <- apply(.Object@.values,1,
                                       function(x,u) {return(paste(x,u,sep=""))},
                                       .Object@values.unit)
            
            ## set opacity values
            svg[.Object@targets, .Object@target.attributes] <- .Object@.values

            ## eop
            return(invisible(.Object))
          }
          )

## F A C T O R Y
##--------------
MappingValues.factory <- function(data,targets=rownames(data),
                                   target.attributes=c("opacity"),
                                   fn="Identity", fn.parameters=list()) {
  ## init.
  mapO <- new("MappingValues")

  ## fill mapping structure
  values(mapO) <- data
  targets(mapO) <- targets
  targetAttributes(mapO) <- target.attributes
  if(missing(fn.parameters))
    mapO <- setFunction(mapO,fn)
  else
    mapO <- setFunction(mapO,fn,fn.parameters)

  ## eop
  return(mapO)
}
